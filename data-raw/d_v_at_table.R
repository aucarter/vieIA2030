dt <- readRDS(
    system.file("extdata", "vimc_estimates.rds", package = "vieIA2030")
)
vimc <- unique(dt[order(disease, vaccine, activity_type),
    .(disease, vaccine, activity_type)])
non_vimc <- data.table(
    disease = c("D", "T", "P", "TB"),
    vaccine = c("DTP3", "DTP3", "DTP3", "BCG"),
    activity_type = "routine"
)
rub <- data.table(
    disease = "Rubella",
    vaccine = "Rubella",
    activity_type = "combined"
)
d_v_at_table <- rbindlist(list(vimc, non_vimc, rub))
d_v_at_table[, d_v_at_id := .I]
usethis::use_data(d_v_at_table, overwrite = TRUE)