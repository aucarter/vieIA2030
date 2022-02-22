vimc_vaccine_table <- fread("data-raw/vimc_vaccine_table.csv")
usethis::use_data(vimc_vaccine_table, overwrite = TRUE)