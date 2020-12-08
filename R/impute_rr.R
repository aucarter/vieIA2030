check_rr <- function(dt) {
    # Check for missingness
    if (any(is.na(dt$rr))) {
        missing_locs <- unique(dt[is.na(rr)]$location_id)
        missing_names <- loc_table[location_id %in% missing_locs]$location_name
        warning(paste(
            "Missing relative-risk for",
            paste(missing_names, collapse = ", ")
        ))
        dt <- dt[!is.na(rr)]
    }

    # Check for non-sensical numbers
    if (any(range(dt$rr) < 0 | range(dt$rr) > 1)) {
        prop <- round(nrow(dt[rr < 0 | rr > 1]) / nrow(dt) * 100, 2)
        warning(paste0(
            "Over 1 or less than 0 mortality reduction in ", prop, 
            "% of location-age-years"))
    }
}

prep_vimc_rr <- function(alpha) {
    ## Pull in data that isn't already loaded in global environment
    message("Loading VIMC data...")
    data_names <- c(
        "vimc_impact", "all_deaths", "coverage_inputs"
    )
    unloaded_data <- setdiff(data_names, ls())
    if (length(unloaded_data) > 0) {
        temp <- lapply(unloaded_data, function(table) {
            dt <- db_pull(table)
            assign(table, dt, envir = .GlobalEnv)
        })
    }

    ## Calculate relative risk
    message("Calculating VIMC relative risk...")
    # Calculate both-sexes deaths
    deaths <- all_deaths[, .(deaths_obs = sum(deaths)),
                         by = .(age, year, location_id)]

    # Merge on VIMC impact estimates + coverage and calculate mortality reduction
    dt <- left_join(
            vimc_impact[year %in% 2000:2019],
            deaths,
            by = c("age", "year", "location_id")
        ) %>%
        rename(vaccine_deaths_averted = value) %>%
        left_join(
            coverage_inputs[, .(location_id, year, vaccine_id, value)],
            by = c("location_id", "year", "vaccine_id")
        ) %>%
        rename(coverage = value) %>%
        mutate(rr = (deaths_obs - (vaccine_deaths_averted *
            (1 - coverage ^ alpha) / coverage ^ alpha)) /
            (deaths_obs + vaccine_deaths_averted))

    if(nrow(dt[coverage == 0 & vaccine_deaths_averted > 0]) > 0) {
        prop <- round(nrow(dt[coverage == 0 & vaccine_deaths_averted > 0]) / 
            nrow(dt[vaccine_deaths_averted > 0]) * 100, 2)
        warning("Missing coverage in ", prop, "% of location-age-years with deaths averted")
    }

    out_dt <- dt %>%
        select(c(location_id, age, year, vaccine_id, deaths_obs,
                 vaccine_deaths_averted, coverage, rr))
    
    check_rr(out_dt)

    return(out_dt)
}

prep_gbd_rr <- function(alpha, beta) {
    ## Pull in data that isn't already loaded in global environment
    message("Loading GBD data...")
    data_names <- c(
       "all_deaths", "coverage_inputs", "gbd_vaccine_deaths"
    )
    unloaded_data <- setdiff(data_names, ls())
    if (length(unloaded_data) > 0) {
        temp <- lapply(unloaded_data, function(table) {
            dt <- db_pull(table)
            assign(table, dt, envir = .GlobalEnv)
        })
    }

    ## Calculate relative risk
    message("Calculating GBD relative risk...")
    # Merge on VIMC impact estimates + coverage and calculate mortality reduction
    dt <- left_join(
        gbd_vaccine_deaths,
        all_deaths,
        by = c("age", "year", "location_id", "sex_id")
    )
    # Collapse sex
    dt <- dt[, .(vaccine_deaths = sum(value), deaths_obs = sum(deaths)),
        by = .(location_id, year, age, vaccine_id)]
    # Merge observed coverage and efficiency
    dt <- dt %>%
        left_join(
            coverage_inputs[, .(location_id, year, vaccine_id, value)],
            by = c("location_id", "year", "vaccine_id")
        ) %>%
        left_join(efficacy[, .(mean, vaccine_id)], by = "vaccine_id") %>%
        rename(efficacy = mean) %>%
        rename(coverage = value)
    # Set a cap on BCG effect at age 15
    dt[vaccine_id == 14 & age >= 15, coverage := 0]

    # Calcualte RR
    dt <- dt %>%
        mutate(deaths_no = vaccine_deaths / (1 - beta * efficacy * coverage ^ alpha)) %>%
        mutate(rr = (deaths_obs - vaccine_deaths + (1 - beta * efficacy) * deaths_no) /
            (deaths_obs - vaccine_deaths + deaths_no))

    out_dt <- dt %>%
        select(c(location_id, age, year, vaccine_id, deaths_obs,
                 vaccine_deaths, coverage, rr))
    
    check_rr(out_dt)

    return(out_dt)
}

merge_rr_covariates <- function(dt) {
        # Merge on covariates
    message("Merging on covariates...")
    dt <- right_join(
            dt,
            gbd_cov,
            by = c("location_id",  "year")
        ) %>%
        left_join(
            vaccine_table[, .(vaccine_id, vaccine_short)],
            by = "vaccine_id"
        )
    return(dt)
}

impute_vacc_rr <- function(vacc, dt) {
    print(vacc)
    fit <- glm(
        rr ~ haqi + sdi + year + splines::bs(age, knots = c(2, 5, 10, 25)),
        data = dt[vaccine_short == vacc & rr < 1 & rr > 0],
        family = "binomial"
    )
    pred_dt <- merge(
        gbd_cov[, idx := .I],
        CJ(
            age = seq(
                min(dt[vaccine_short == vacc]$age),
                max(dt[vaccine_short == vacc]$age)
            ),
            idx = 1:nrow(gbd_cov)
        ),
        by = "idx"
    )[, idx := NULL]
    pred_dt[, pred := predict(fit, pred_dt)]
    pred_dt[, pred_rr := exp(pred) / (exp(pred) + 1)]
    pred_dt[, pred := NULL]
    pred_dt[, vaccine_short := vacc]
    out_dt <- merge(
        pred_dt,
        dt[, .(location_id, age, year, vaccine_short, rr, coverage, deaths_obs,
               vaccine_deaths_averted)],
        all.x = T
    )

    return(out_dt)
}

impute_rr <- function(alpha, beta) {
    vimc_dt <- prep_vimc_rr(alpha)
    gbd_dt <- prep_gbd_rr(alpha, beta)
    dt <- rbind(vimc_dt, gbd_dt, fill = T)
    dt <- merge_rr_covariates(dt)

    ## Simple model for VIMC imputation
    pred_all <- rbindlist(
        lapply(
            unique(dt[!is.na(vaccine_short)]$vaccine_short),
            impute_vacc_rr,
            dt
        )
    )

    return(pred_all)
}
