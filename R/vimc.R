prep_vimc_rr_data <- function() {
    ## Pull in data that isn't already loaded in global environment
    message("Loading data...")
    data_names <- c(
        "wpp_input", "vimc_impact_estimates", "coverage_inputs"
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
    rr_dt <- vimc_rr(wpp_input, vimc_impact_estimates)

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
#' @param vimc_impact_estimates VIMC deaths averted
#' @return A list of tables with population projection results
vimc_rr <- function(wpp_input, vimc_impact_estimates) {
    # Calculate both-sexes deaths
    deaths <- get_all_deaths(2000, 2019, wpp_input) %>%
        group_by(age, year_id, location_iso3) %>%
        summarise(deaths_obs = sum(deaths)) %>%
        ungroup() %>%
        rename(year = year_id) %>%
        as.data.table()

    # Merge on VIMC impact estimates + coverage and calculate mortality reduction
    dt <- left_join(
            deaths,
            vimc_impact_estimates,
            by = c("age", "year", "location_iso3")
        ) %>%
        rename(vaccine_deaths_averted = value) %>%
        left_join(
            coverage_inputs[, .(location_id, year, vaccine_id, value)],
            by = c("location_id", "year", "vaccine_id")
        ) %>%
        rename(coverage = value) %>%
        mutate(rr = (deaths_obs - (vaccine_deaths_averted * (1 - coverage) / coverage)) /
            (deaths_obs + vaccine_deaths_averted))

    # Check for missingness
    if (any(is.na(dt$rr))) {
        missing_locs <- unique(dt[is.na(rr)]$location_name)
        warning(paste(
            "Missing deaths for",
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
                 vaccine_deaths_averted, rr))

    return(out_dt)
}

## Impute VIMC deaths averted for missing locations
impute_vimc <- function(fit, covariates) {
    vimc_imputation <- NULL
    return(vimc_imputation)
}