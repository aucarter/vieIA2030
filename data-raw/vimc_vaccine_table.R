vimc_vaccine_table <- fread("data-raw/vimc_vaccine_table.csv")
vimc_vaccine_table[, vaccine_id := .I]
usethis::use_data(vimc_vaccine_table, overwrite = TRUE)