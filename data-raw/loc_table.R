loc_table <- fread("data-raw/loc_table.csv")

## Merge on income group
income_dt <- fread("data-raw/incomegroup.csv")
missing <- setdiff(loc_table$location_iso3, income_dt$iso3)
names(income_dt) <- c("location_iso3", "income_group")
loc_table <- merge(loc_table, income_dt, by = "location_iso3", all.x = T)
loc_table[location_iso3 %in% c("COK", "NIU"), income_group := "Upper middle income"]

## Get the Gavi status
yov_dt <- as.data.table(
    readRDS(("supp_data/interim-update-202007wue_summary.rds"))
)
gavi_dt <- unique(yov_dt[, .(country, gavi73)])
setnames(gavi_dt, "country", "location_iso3")
gavi_dt[, gavi73 := as.integer(gavi73)]
loc_table <- merge(loc_table, gavi_dt, all.x = T)
loc_table[is.na(gavi73), gavi73 := 0]

usethis::use_data(loc_table, overwrite = T)