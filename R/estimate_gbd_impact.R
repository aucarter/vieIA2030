## Estimate GBD vaccine-related mortality reduction
combine_inputs <- function() {
    ## Pull in GBD estimates, SDI, and HAQI
    data(gbd_estimates)
    data(gbd_haqi)
    data(gbd_sdi)

    ## Pull in coverage
    mydb <- DBI::dbConnect(RSQLite::SQLite(), "vieIA2030.db")
    coverage_dt <- data.table::as.data.table(
        collect(tbl(mydb, "coverage_inputs"))
    )
    DBI::dbDisconnect(mydb)

    ## Combine
    data.table::setnames(gbd_estimates, "value", "mortality")
    data.table::setnames(gbd_haqi, "value", "haqi")
    dt <- merge(gbd_estimates, gbd_haqi, by = c("country_id", "year"))
    data.table::setnames(gbd_sdi, "value", "sdi")
    dt <- merge(dt, gbd_sdi, by = c("country_id", "year"))
    data.table::setnames(coverage_dt, "value", "coverage")
    coverage_dt[, sex_id := NULL]
    dt <- merge(dt, coverage_dt, by = c("country_id", "year", "vaccine_id"))

    return(dt)
}

dt <- combine_inputs()