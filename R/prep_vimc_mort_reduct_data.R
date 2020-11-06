prep_vimc_mort_reduct_data <- function() {
    ## Pull in data
    wpp_input <- db_pull("wpp_input")
    obs_wpp <- db_pull("obs_wpp")
    vimc <- db_pull("vimc_impact_estimates")
    coverage <- db_pull("coverage_inputs")

    mort_reduct_dt <- vimc_mort_reduct(wpp_input, obs_wpp, vimc)

    # Merge on covariates and coverage
    dt <- left_join(
            mort_reduct_dt[year %in% 2000:2019],
            gbd_cov,
            by = c("location_id",  "year")
        ) %>%
        left_join(
            vaccine_table[, .(vaccine_id, vaccine_short)],
            by = "vaccine_id"
        ) %>%
        left_join(
            coverage[, .(location_id, year, vaccine_id, value)],
            by = c("location_id", "year", "vaccine_id")
        ) %>%
        rename(coverage = value)
    return(dt)
}
