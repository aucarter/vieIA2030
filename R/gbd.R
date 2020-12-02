prep_gbd_rr_data <- function() {
    ## Pull in data that isn't already loaded in global environment
    message("Loading GBD data...")
    data_names <- c(
       "all_deaths", "coverage_inputs"
    )
    unloaded_data <- setdiff(data_names, ls())
    if (length(unloaded_data) > 0) {
        temp <- lapply(unloaded_data, function(table) {
            dt <- db_pull(table)
            assign(table, dt, envir = .GlobalEnv)
        })
    }

    # Calculate relative risk
    message("Calculating GBD relative risk...")
    rr_dt <- gbd_rr(all_deaths, coverage_inputs)


    # Merge on covariates and coverage
    message("Merging on covariates and coverage...")
    dt <- right_join(
            rr_dt[year %in% 2000:2019],
            gbd_cov[year %in% 2000:2019],
            by = c("location_id",  "year")
        ) %>%
        left_join(
            vaccine_table[, .(vaccine_id, vaccine_short)],
            by = "vaccine_id"
        ) %>%
        left_join(
            coverage_inputs[, .(location_id, year, vaccine_id, value)],
            by = c("location_id", "year", "vaccine_id")
        ) %>%
        rename(coverage = value)
    return(dt)
}

forecast_gbd_cov <- function(plot = F) {
    fcast_list <- list()
    if (plot) {
        par(mfrow = c(2, 1))
    }
    for (cov in c("haqi", "sdi")) {
        dt <- gbd_cov[, c("location_id", "year", cov), with = F]
        cast_dt <- dcast(dt, location_id ~ year, value.var = cov)
        mat <- as.matrix(cast_dt[, -1])
        start_vec <-  mat[, dim(mat)[2] - 5]
        end_vec <-  mat[, dim(mat)[2]]
        aroc_5 <- log(end_vec / start_vec) / 5
        t_mat <- matrix(1:11, byrow = T, ncol = 11, nrow = length(end_vec))
        pred_mat <- end_vec * exp(aroc_5 * t_mat)
        colnames(pred_mat) <- 2020:2030
        if (max(pred_mat >= 1)) {
            warning("Forecast greater than or equal to 1. Consider forecasting 
                     in logit space")
        }
        out_mat <- cbind(mat, pred_mat)
        if (plot) {
            matplot(1990:2030, t(out_mat), type = "l", ylim = c(0, 1),
                    xlab = "Year", ylab = cov)
            abline(h = 1, col = "red")
            abline(v = 2019.5, col = "black")
        }
        melt_dt <- melt(
            data.table(location_id = cast_dt$location_id, out_mat),
            id.var = "location_id",
            variable.name = "year",
            value.name = cov
        )
        melt_dt[, year := as.integer(as.character(year))]
        fcast_list[[cov]] <- melt_dt
    }
    out_dt <- merge(fcast_list[[1]], fcast_list[[2]])

    return(out_dt)
}

convert_single_year <- function(gbd_dt) {
    conv_table <- data.table(
        age = c(0, rep(1, 4), rep(seq(5, 75, 5), each = 5), 80),
        age_new = 0:80,
        n = c(1, rep(4, 4), rep(5, 75), 1)
    )
    out_dt <- merge(gbd_dt, conv_table, by = "age", allow.cartesian = T)
    out_dt[, age := age_new]
    out_dt[, value := value / n]
    out_dt[, c("age_new", "n") := NULL]

    return(out_dt[])
}

#' Calculate all-cause mortality reduction by vaccine for VIMC 10
#' @param all_deaths All-cause deaths from calibrated pop projection
#' @param coverage_inputs Observed coverage
#' @return A table with GBD relative-risk
gbd_rr <- function(all_deaths, coverage_inputs) {
    # Merge on VIMC impact estimates + coverage and calculate mortality reduction
    dt <- left_join(
            convert_single_year(gbd_estimates[year %in% 2000:2019]),
            all_deaths,
            by = c("age", "year", "location_id")
        ) %>%
        rename(vaccine_deaths = value, deaths_obs = deaths) %>%
        left_join(
            coverage_inputs[, .(location_iso3, year, vaccine_id, value)],
            by = c("location_iso3", "year", "vaccine_id")
        ) %>%
        left_join(efficacy[, .(mean, vaccine_id)], by = "vaccine_id") %>%
        rename(efficacy = mean) %>%
        rename(coverage = value) %>%
        mutate(deaths_no = vaccine_deaths / (1 - efficacy * coverage)) %>%
        mutate(rr = (deaths_obs - vaccine_deaths + (1 - efficacy) * deaths_no) /
            (deaths_obs - vaccine_deaths + deaths_no))

    # Check for missingness
    if (any(is.na(dt$rr))) {
        missing_locs <- unique(dt[is.na(rr)]$location_name)
        warning(paste(
            "Missing relative-risk for",
            paste(missing_locs, collapse = ", ")
        ))
        dt <- dt[!is.na(rr)]
    }

    # Check for non-sensical numbers
    if (any(range(dt$rr) < 0 | range(dt$rr) > 1)) {
        warning("Over 1 or less than 0 mortality reduction")
        dt[rr < 0]$rr <- min(dt[rr > 0]$rr)
        dt[rr > 1]$rr <- max(dt[rr < 1]$rr)
    }

    out_dt <- dt %>%
        select(c(location_id, age, year, vaccine_id, deaths_obs,
                 vaccine_deaths, rr))

    return(out_dt)
}