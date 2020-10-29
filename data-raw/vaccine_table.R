vaccine_table <- fread("data-raw/vaccine_table.csv")
vaccine_table[, vaccine_id := .I]
usethis::use_data(vaccine_table, overwrite = TRUE)