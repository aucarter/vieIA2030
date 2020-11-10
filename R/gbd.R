prep_gbd_data <- function() {
    ## Pull in data that isn't already loaded in global environment
    message("Loading data...")
    data_names <- c(
        "wpp_input", "obs_wpp", "coverage_inputs"
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
    rr_dt <- gbd_rr(
        wpp_input, obs_wpp
    )

    # Merge on covariates and coverage
    message("Merging on covariates and coverage...")
    dt <- left_join(
            rr_dt[year %in% 2000:2019],
            gbd_cov,
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


#' Calculate all-cause mortality reduction by vaccine for GBD
#' @param wpp_input Input WPP data
#' @param obs_wpp Observed WPP data
#' @return A list of tables with population projection results
gbd_rr <- function(wpp_input, obs_wpp) {

    # Calculate both-sexes deaths
    deaths <- get_all_deaths(1999, 2029, wpp_input, obs_wpp) %>%
        group_by(age, year_id, location_iso3) %>%
        summarise(deaths_obs = sum(deaths)) %>%
        ungroup() %>%
        rename(year = year_id)

    # Calculate total deaths averted
    vimc_impact_estimates <- vimc_impact_estimates %>%
        group_by(age, year, location_iso3) %>%
        mutate(deaths_averted = sum(value)) %>%
        ungroup() %>%
        as.data.table()

    # Merge on VIMC impact estimates and calculate mortality reduction
    dt <- left_join(
            vimc_impact_estimates,
            deaths,
            by = c("age", "year", "location_iso3")
        ) %>%
        rename(vaccine_deaths_averted = value) %>%
        mutate(rr = (deaths_obs + deaths_averted - vaccine_deaths_averted) /
            (deaths_obs + deaths_averted))

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
    }

    out_dt <- dt %>%
        select(c(location_id, age, year, vaccine_id, deaths_obs,
                     deaths_averted, rr))

    return(out_dt)
}