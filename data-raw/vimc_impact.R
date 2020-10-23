# Prep VIMC vaccine impact estimates
dt <- data.table::fread(
    system.file("extdata", "vimc_estimates.csv", package = "vieIA2030")
)
dt[, c("gavi73", "who_region") := NULL]
data.table::setnames(dt, "country", "country_iso3")
melt_dt <- data.table::melt.data.table(
    dt,
    id.vars = c("country_iso3", "country_name", "age", "year"),
    variable = "vaccine_short"
)
vimc_impact <- melt_dt[order(
    country_iso3, country_name, year, age, vaccine_short
)]

mydb <- dbConnect(RSQLite::SQLite(), "vieIA2030.db")
dbWriteTable(mydb, "vimc_impact_estimates", vimc_impact, overwrite = TRUE)