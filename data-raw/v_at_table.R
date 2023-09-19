dt <- readRDS(
    system.file("extdata", "vimc_estimates.rds", package = "vieIA2030")
)
vimc <- unique(dt[order(vaccine, activity_type),
    .(vaccine, activity_type)])
non_vimc <- data.table(
    vaccine = c("DTP3", "BCG"),
    activity_type = "routine"
)
rub <- data.table(
    vaccine = "Rubella",
    activity_type = "combined"
)
v_at_table <- rbindlist(list(vimc, non_vimc, rub))
v_at_table <- rbind(v_at_table, data.table(vaccine = "DTP1", activity_type = "routine"))
v_at_table[, v_at_id := .I]
usethis::use_data(v_at_table, overwrite = TRUE)