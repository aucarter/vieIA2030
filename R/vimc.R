prep_vimc_rr_data <- function() {
    ## Pull in data that isn't already loaded in global environment
    message("Loading data...")
    data_names <- c(
        "wpp_input", "vimc_impact", "coverage_inputs", "all_deaths"
    )
    unloaded_data <- setdiff(data_names, ls())
    if (length(unloaded_data) > 0) {
        temp <- lapply(unloaded_data, function(table) {
            dt <- db_pull(table)
            assign(table, dt, envir = .GlobalEnv)
        })
    }

    # Calculate relative risk
    message("Calculating relative risk...")
    rr_dt <- vimc_rr(wpp_input, all_deaths, vimc_impact)

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


#' Calculate all-cause mortality reduction by vaccine for VIMC 10
#' @param wpp_input Input WPP data
#' @param all_deaths All-cause deaths from calibrated pop projection
#' @param vimc_impact VIMC deaths averted
#' @return A list of tables with population projection results
vimc_rr <- function(wpp_input, all_deaths, vimc_impact) {
    # Calculate both-sexes deaths
    deaths <- all_deaths %>%
        group_by(age, year_id, location_iso3) %>%
        summarise(deaths_obs = sum(deaths)) %>%
        ungroup() %>%
        rename(year = year_id) %>%
        as.data.table()

    # Merge on VIMC impact estimates + coverage and calculate mortality reduction
    dt <- left_join(
            vimc_impact[year %in% 2000:2019],
            deaths,
            by = c("age", "year", "location_iso3")
        ) %>%
        rename(vaccine_deaths_averted = value) %>%
        left_join(
            coverage_inputs[, .(location_iso3, year, vaccine_id, value)],
            by = c("location_iso3", "year", "vaccine_id")
        ) %>%
        rename(coverage = value) %>%
        mutate(rr = (deaths_obs - (vaccine_deaths_averted * (1 - coverage) / coverage)) /
            (deaths_obs + vaccine_deaths_averted))

    # Check for missingness
    if (any(is.na(dt$rr))) {
        missing_locs <- unique(dt[is.na(rr)]$location_name)
        warning(paste(
            "Missing relative-risk for",
            paste(missing_locs, collapse = ", ")
        ))
        # Save the problem location, vaccine, years and remove NAs
        prob_dt <- unique(dt[is.na(rr) & is.na(coverage), 
                             .(location_name, year, vaccine_short)])
        write.csv(prob_dt, "missing_coverage.csv", row.names = F)
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
                 vaccine_deaths_averted, rr))

    return(out_dt)
}

vimc_impute_vacc_rr <- function(vacc, dt) {
    print(vacc)
    fit <- glm(
        rr ~ haqi + sdi + year + splines::bs(age, knots = c(2, 5, 10, 25)),
        data = dt[vaccine_short == vacc],
        family = "binomial"
    )
    forecast_cov <- forecast_gbd_cov()
    pred_dt <- merge(
        forecast_cov[, idx := .I],
        CJ(
            age = seq(
                min(dt[vaccine_short == vacc]$age),
                max(dt[vaccine_short == vacc]$age)
            ),
            idx = 1:nrow(dt[is.na(rr), .(location_id, year, haqi, sdi)])
        ),
        by = "idx"
    )[, idx := NULL]
    pred_dt[, pred := predict(fit, pred_dt)]
    pred_dt[, pred_rr := exp(pred) / (exp(pred) + 1)]
    pred_dt[, pred := NULL]
    pred_dt[, vaccine_short := vacc]
    out_dt <- merge(
        pred_dt,
        dt[, .(location_id, age, year, vaccine_id, rr, coverage, deaths_obs,
               vaccine_deaths_averted)],
        all.x = T
    )

    return(out_dt)
}