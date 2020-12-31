get_mx_scen <- function(is, y0, y1, scen, mx, nx) {
    default_coverage <- get_scen_coverage(is, y0, y1, "Default") %>%
        rename(default_cov = coverage)
    scen_coverage <- get_scen_coverage(is, y0, y1, scen) %>%
        rename(scen_cov = coverage)
    deaths <- mx * nx

    ## VIMC
    vimc_deaths_change <- get_vimc_deaths_change(is, y0, y1, default_coverage,
        scen_coverage, mx)

    ## DTP
    # Calculate relative risk given coverage and scale deaths accordingly
    dtp_deaths_change <- NULL

    ## BCG
    bcg_deaths_change <- NULL

    scen_deaths <- deaths + vimc_deaths_change + dtp_deaths_change +
        bcg_deaths_change

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

get_vimc_deaths_change <- function(is, y0, y1, default_coverage, scen_coverage,
    mx) {
    vimc_deaths_change <- vimc_impact %>%
        filter(location_name == is & year %in% y0:y1) %>%
        left_join(default_coverage, by = c("year", "vaccine_id")) %>%
        left_join(scen_coverage, by = c("year", "vaccine_id")) %>%
        mutate(scalar = (default_cov - scen_cov) / default_cov) %>%
        mutate(deaths_change = scalar * value) %>%
        group_by(year, age) %>%
        summarise(deaths_change = sum(deaths_change, na.rm = T)) %>%
        ungroup() %>%
        tidyr::spread(year, deaths_change) %>%
        select(-c(age)) %>%
        as.matrix()
    # Fill in older age groups TODO: fix this on vimc_impact side
    vimc_deaths_change <- rbind(
        vimc_deaths_change,
        matrix(
            nrow = dim(mx)[1] / 2 - dim(vimc_deaths_change)[1],
            ncol = dim(mx)[2]
        )
    )
    # Split evenly across sexes
    vimc_deaths_change <- rbind(
        vimc_deaths_change / 2,
        vimc_deaths_change / 2
    )

    return(vimc_deaths_change)
}

get_deaths_scen <- function(deaths_obs, averted_obs, averted_scen) {
    deaths_scen <- deaths_obs + sum(averted_obs) - sum(averted_scen)

    return(deaths_scen)
}