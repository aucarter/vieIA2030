dt <- readRDS(
    system.file("extdata", "vimc_estimates.rds", package = "vieIA2030")
)
vimc <- unique(dt[order(disease, vaccine),
    .(disease, vaccine)])
non_vimc <- data.table(
    disease = c("D", "T", "P", "TB"),
    vaccine = c("DTP3", "BCG")
)
disease_vaccine_table <- rbind(vimc, non_vimc)
usethis::use_data(disease_vaccine_table, overwrite = TRUE)