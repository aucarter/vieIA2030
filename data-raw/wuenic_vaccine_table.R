wuenic_vaccine_table <- fread("data-raw/wuenic_vaccine_table.csv")
wuenic_vaccine_table[, vaccine_id := .I]
usethis::use_data(wuenic_vaccine_table, overwrite = TRUE)