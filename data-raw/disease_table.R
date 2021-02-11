disease_table <- fread("data-raw/disease_table.csv")
disease_table[, disease_id := .I]
usethis::use_data(disease_table, overwrite = TRUE)