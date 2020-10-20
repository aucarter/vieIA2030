#' Prep VIMC estimates for usage in the projection
#'
#' @return data.table with VIMC
#' @export
prep_vimc <- function() {
    dt <- data.table::fread(
        "data/global-impact-2020-201910gavi_20200731-163545-a2bf8c3f_Summary_201910gavi_calendar_estimates.csv"
    )
    dt[, c("gavi73", "who_region") := NULL]
    data.table::setnames(dt, "country", "country_iso3")
    melt_dt <- data.table::melt.data.table(
        dt,
        id.vars = c("country_iso3", "country_name", "age", "year"),
        variable = "vaccine_short"
    )
    out_dt <- melt_dt[order(
        country_iso3, country_name, year, age, vaccine_short
    )]
    return(out_dt)
}