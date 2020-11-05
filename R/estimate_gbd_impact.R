## Estimate GBD vaccine-related mortality reduction
combine_inputs <- function() {
    ## Pull in coverage
    mydb <- DBI::dbConnect(RSQLite::SQLite(), "vieIA2030.db")
    coverage_dt <- as.data.table(
        collect(tbl(mydb, "coverage_inputs"))
    )
    DBI::dbDisconnect(mydb)

    ## Combine
    setnames(gbd_estimates, "value", "mortality")
    setnames(gbd_haqi, "value", "haqi")
    dt <- merge(gbd_estimates, gbd_haqi, by = c("location_id", "year"))
    setnames(gbd_sdi, "value", "sdi")
    dt <- merge(dt, gbd_sdi, by = c("location_id", "year"))
    setnames(coverage_dt, "value", "coverage")
    coverage_dt[, sex_id := NULL]
    dt <- merge(dt, coverage_dt, by = c("location_id", "year", "vaccine_id"))

    return(dt)
}