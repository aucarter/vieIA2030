# Prep VIMC vaccine impact estimates
dt <- readRDS(
    system.file("extdata", "vimc_estimates.rds", package = "vieIA2030")
)
setnames(dt, "country", "location_iso3")
vimc_impact <- dt[order(
    location_iso3, year, age, vaccine
)]
# vimc_impact <- vimc_impact[!is.na(deaths_averted) & deaths_averted > 0]

# Convert to strata_id and location_id to save space
vimc_dt <- merge(vimc_impact, d_v_at_table, by = c("disease", "vaccine", "activity_type"))
vimc_dt <- merge(vimc_dt, loc_table[, .(location_iso3, location_id)],
                 by = "location_iso3")

saveRDS(vimc_dt, "inst/shiny/vimc.rds")
vimc_dt <- vimc_dt[order(location_id, d_v_at_id, age, year),
        .(location_id, d_v_at_id, age, year, deaths_averted)]

upload_object(vimc_dt, "vimc_impact")
