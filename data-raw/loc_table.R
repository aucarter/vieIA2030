loc_table <- fread("data-raw/loc_table.csv")

## Merge on income group
income_dt <- fread("data-raw/incomegroup.csv")
missing <- setdiff(loc_table$location_iso3, income_dt$iso3)
names(income_dt) <- c("location_iso3", "income_group")
loc_table <- merge(loc_table, income_dt, by = "location_iso3", all.x = T)
loc_table[location_iso3 %in% c("COK", "NIU"), income_group := "Upper middle income"]

usethis::use_data(loc_table, overwrite = T)