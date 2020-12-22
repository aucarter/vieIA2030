# Prep GBD 2019 VPD burden estimates

## Load data
load(system.file("extdata", "gbd19_estimates.RData", package = "vieIA2030"))

convert_single_year <- function(gbd_dt) {
    conv_table <- data.table(
        age = c(0, rep(1, 4), rep(seq(5, 75, 5), each = 5), 80),
        age_new = 0:80,
        n = c(1, rep(4, 4), rep(5, 75), 1)
    )
    out_dt <- merge(gbd_dt, conv_table, by = "age", allow.cartesian = T)
    out_dt[, age := age_new]
    out_dt[, value := value / n]
    out_dt[, c("age_new", "n") := NULL]

    return(out_dt[])
}

gbd_estimates <- convert_single_year(gbd_estimates)

mydb <- open_connection()
DBI::dbWriteTable(
    conn = mydb,
    name = "gbd_vaccine_deaths",
    value = gbd_estimates,
    fields = bigrquery::as_bq_fields(gbd_estimates),
    overwrite = TRUE
)
DBI::dbDisconnect(mydb)
