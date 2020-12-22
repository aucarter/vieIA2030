# Prep VIMC vaccine impact estimates
dt <- fread(
    system.file("extdata", "vimc_estimates.csv", package = "vieIA2030")
)
dt[, c("gavi73", "who_region") := NULL]
setnames(dt, "country", "location_iso3")
melt_dt <- melt.data.table(
    dt,
    id.vars = c("location_iso3", "location_name", "age", "year"),
    variable = "vaccine_short"
)
vimc_impact <- melt_dt[order(
    location_iso3, location_name, year, age, vaccine_short
)]
vimc_impact <- vimc_impact[!is.na(value) & value > 0]

# Convert to vaccine_id and location_id to save space
vimc_dt <- merge(vimc_impact, vaccine_table, by = "vaccine_short")
vimc_dt <- merge(vimc_dt, loc_table[, .(location_iso3, location_id)],
                 by = "location_iso3")

save(vimc_dt, file = "inst/shiny/vimc.RData")
vimc_dt[, c("vaccine_long", "vaccine_short", "cause_name") := NULL]
vimc_dt[, c("location_name", "location_iso3") := NULL]



mydb <- open_connection()
DBI::dbWriteTable(
    conn = mydb,
    name = "vimc_impact",
    value = vimc_dt,
    fields = bigrquery::as_bq_fields(vimc_dt),
    overwrite = TRUE
)
DBI::dbDisconnect(mydb)