prep_gbd_data <- function() {
    # Merge covariates and coverage onto GBD mortality estimates
    message("Merging on covariates and coverage...")
    dt <- gbd_estimates %>%
        rename(mort_rate = value) %>%
        left_join(gbd_cov, by = c("location_id",  "year")) %>%
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
