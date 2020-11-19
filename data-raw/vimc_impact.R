# Prep VIMC vaccine impact estimates
dt <- data.table::fread(
    system.file("extdata", "vimc_estimates.csv", package = "vieIA2030")
)
dt[, c("gavi73", "who_region") := NULL]
data.table::setnames(dt, "country", "location_iso3")
melt_dt <- data.table::melt.data.table(
    dt,
    id.vars = c("location_iso3", "location_name", "age", "year"),
    variable = "vaccine_short"
)
vimc_impact <- melt_dt[order(
    location_iso3, location_name, year, age, vaccine_short
)]
vimc_impact <- vimc_impact[!is.na(value) & value > 0]

# Convert to vaccine_id and location_id to save space
vimc_dt <- merge(vimc_impact, vaccine_table)
vimc_dt[, c("vaccine_long", "vaccine_short") := NULL]
vimc_dt <- merge(vimc_dt, loc_table)
vimc_dt[, c("location_name", "location_iso3") := NULL]

mydb <- open_connection()
DBI::dbWriteTable(mydb, "vimc_impact_estimates", vimc_dt, overwrite = TRUE)
DBI::dbDisconnect(mydb)