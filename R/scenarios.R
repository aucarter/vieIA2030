gen_ia2030_goals <- function(ia2030_dtp_goal, linear = T,
                             no_covid_effect = 2022, intro_year = 2025) {
    # Load 2019 coverage
    cov_dt <- db_pull("coverage_inputs")[year == 2019]

    # Need to get explicit about age of intro here (missing 2)
    # Could pick max difference? (most administered age)
    zero_dt <- cov_dt[age == 0]

    # Iterate through each vaccine
    pdf("plots/ia2030_coverage.pdf")
    for(v in unique(vaccine_table$vaccine_id)) {
        v_dt <- zero_dt[vaccine_id == v][order(location_id)]

        # Repeat out to 'no covid effect' year
        n_covid <- no_covid_effect - 2019
        covid_mat <- matrix(rep(v_dt$value, n_covid), ncol = n_covid)
        
        # Linear increase to goal with zeros delayed to intro_year
        n_increase <- 2030 - no_covid_effect + 1
        zero_n <- 2030 - intro_year + 1
        zero_idx <- which(v_dt$value == 0)
        setnames(v_dt, "value", "current")
        roc_dt <- merge(
            v_dt[, .(location_id, current)],
            ia2030_dtp_goal[, .(location_id, value)],
            by = "location_id"
        )
        roc_dt[, n := ifelse(current == 0, zero_n, n_increase)]
        roc_dt[, roc := (value - current) / n]
        t_mat <- matrix(
            1:n_increase, byrow = T, ncol = n_increase, 
            nrow = nrow(roc_dt)
        )
        t_mat[zero_idx,] <- matrix(c(rep(0, n_increase - zero_n), 1:zero_n), 
            nrow = length(zero_idx), ncol = n_increase, byrow = T)
        inc_mat <- v_dt$current + roc_dt$roc * t_mat
       
        # Combine and convert to data.table
        c_mat <- cbind(covid_mat, inc_mat)
        colnames(c_mat) <- 2019:2030
        dt <- cbind(v_dt[, -c("year", "current"), with = F], c_mat)
        melt_dt <- melt(dt,
            id.vars = c("location_id", "vaccine_id", "age", "sex_id"),
            variable.name = "year"
        )

        matplot(t(c_mat), type = "l", xaxt = "n",
            main = vaccine_table[vaccine_id == v]$vaccine_long)
        axis(1, seq(2, 12, 2), labels = seq(2020, 2030, 2))
    }
    dev.off()
}


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