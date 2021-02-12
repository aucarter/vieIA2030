dt <- readRDS(
    system.file("extdata", "vimc_estimates.rds", package = "vieIA2030")
)
vimc <- unique(dt[order(disease, vaccine),
    .(disease, vaccine)])
non_vimc <- data.table(
    disease = c("D", "T", "P", "TB"),
    vaccine = c("DTP3", "DTP3", "DTP3", "BCG")
)
d_v_table <- rbind(vimc, non_vimc)
d_v_table[, d_v_id := .I]
usethis::use_data(d_v_table, overwrite = TRUE)