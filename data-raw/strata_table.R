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
strata_table <- rbind(vimc, non_vimc)
strata_table[, strata_id := .I]
usethis::use_data(strata_table, overwrite = TRUE)