# Impute VIMC estimates in missing locations
# TODO: Bring in single-age deaths as denominator

impute_vimc_estimates <- function() {
    ## Load location table
    data(loc_table)

    ## Pull in VIMC estimates
    mydb <- DBI::dbConnect(RSQLite::SQLite(), "vieIA2030.db")
    vimc_dt <- data.table::as.data.table(
        dplyr::collect(
            dplyr::tbl(mydb, "vimc_impact_estimates")
        )
    )
    data.table::setnames(vimc_dt, "value", "deaths_averted")

    ## Load SDI and merge on
    data(gbd_sdi)
    gbd_sdi[, c("country_name", "country_iso3") := NULL]
    data.table::setnames(gbd_sdi, "value", "sdi")
    dt <- merge(vimc_dt, gbd_sdi, by = c("country_id", "year"))
    dt <- dt[deaths_averted > 0 & !is.na(deaths_averted)]

    ## Look at Measles by age
    gg <- ggplot2::ggplot(
        data = dt[vaccine_id == 5 & age < 11],
        ggplot2::aes(x = age, y = deaths_averted, color = sdi)
    ) + ggplot2::geom_point()
    gg

    ## Model deaths_averted as function of SDI
    fit_deaths_averted <- brms::brm(
        deaths_averted ~ 1 + sdi + s(age, k = 8),
        knots = list(age = c(0, 1, 2, 3, 4, 5, 10, 50)),
        family = Gamma(link = "log"),
        dt[vaccine_id == 5],
        cores = 4
    )

    # ## Predict for missing locations
    # missing_locs  <- setdiff(loc_table$country_iso3, dt$country_iso3)
    # pred_dt <- data.table::rbindlist(
    #     lapply(
    #         unique(cov_dt$vaccine_id), function(vacc) {
    #             gbd_sdi[country_iso3 %in% missing_locs][, vaccine_id := vacc]
    #         }
    #     )
    # )
    # pred_dt[, coverage := predict(fit_coverage, pred_dt)]

    # ## Predict start year
    # start_dt <- dt[, .(year = min(year)), by = .(country_iso3, vaccine_id)]
    # merge_dt <- merge(
    #     start_dt,
    #     dt[, .(country_iso3, year, vaccine_id, sdi)],
    #     by = c("country_iso3", "year", "vaccine_id")
    # )
    # fit_start <- lm(
    #     year ~ 1 + sdi + vaccine_id,
    #     merge_dt
    # )
    # pred_dt[, start_year := round(predict(fit_start, pred_dt))]
    # pred_dt[year < start_year, coverage := 0]
    # pred_dt[, start_year := NULL]

    # coverage <- rbind(cov_dt, pred_dt)
}
