
#' Calculate all-cause mortality reduction by vaccine for VIMC 10
#' @param wpp_input Input WPP data
#' @param obs_wpp Observed WPP data
#' @param vimc VIMC deaths averted
#' @return A list of tables with population projection results
vimc_rr <- function(wpp_input, obs_wpp, vimc) {
    # Calculate both-sexes deaths
    deaths <- get_all_deaths(1999, 2029, wpp_input, obs_wpp) %>%
        group_by(age, year_id, location_iso3) %>%
        summarise(deaths_obs = sum(deaths)) %>%
        ungroup() %>%
        rename(year = year_id)

    # Calculate total deaths averted
    vimc <- vimc %>%
        group_by(age, year, location_iso3) %>%
        mutate(deaths_averted = sum(value)) %>%
        ungroup() %>%
        as.data.table()

    # Merge on VIMC impact estimates and calculate mortality reduction
    dt <- left_join(vimc, deaths, by = c("age", "year", "location_iso3")) %>%
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
