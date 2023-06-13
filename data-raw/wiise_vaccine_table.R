wiise_vaccine_table <- fread("data-raw/wiise_vaccine_table.csv")
usethis::use_data(wiise_vaccine_table, overwrite = TRUE)