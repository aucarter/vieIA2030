prep_vimc_rr_data <- function() {
    ## Pull in data that isn't already loaded in global environment
    message("Loading data...")
    data_names <- c(
        "wpp_input", "obs_wpp", "vimc_impact_estimates", "coverage_inputs"
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
    rr_dt <- vimc_rr(
        wpp_input, obs_wpp, vimc_impact_estimates
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
