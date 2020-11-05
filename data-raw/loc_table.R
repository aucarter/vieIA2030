loc_table <- fread("data-raw/loc_table.csv")
# write.csv(loc_table, "data-raw/loc_table.csv", row.names = F)
usethis::use_data(loc_table, overwrite = T)