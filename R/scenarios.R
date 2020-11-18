get_mx_scen <- function(is, y0, y1, scen, mx, nx) {
    default_coverage <- get_scen_coverage(is, y0, y1, "Default") %>%
        rename(default_cov = coverage)
    scen_coverage <- get_scen_coverage(is, y0, y1, scen) %>%
        rename(scen_cov = coverage)
    deaths <- mx * nx

    ## VIMC
    vimc_death_change <- vimc_impact_estimates %>%
        left_join(default_coverage, by = c("year", "vaccine_id")) %>%
        left_join(scen_coverage, by = c("year", "vaccine_id")) %>%
        mutate(scalar = (default_cov - scen_cov) / default_cov) %>%
        mutate(death_change = scalar * value)

    ## DTP
    dtp_death_change <- NULL

    ## BCG
    bcg_death_change <- NULL

    scen_deaths <- deaths + vimc_death_change + dtp_death_change +
        bcg_death_change

    mx <- scen_deaths / nx

    return(mx)
}

get_scen_coverage <- function(is, y0, y1, scen) {
    if (scen == "Default") {
        #TODO: We need to know what the VIMC assumed future coverage is here
        coverage_dt <- coverage_inputs[location_name == is,
                                       .(vaccine_id, year, value)] %>%
            rename(coverage = value)
    }
    if (scen == "No vaccination") {
        coverage_dt <- data.table(expand.grid(
            vaccine_id = unique(vaccine_table$vaccine_id),
            year = y0:y1,
            coverage = 0
        ))
    }

    return(coverage_dt)
}
