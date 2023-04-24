
# Load country information
country_dt = fread("data-raw/country_table.csv")
economy_dt = fread("data-raw/economy_table.csv")

# Join economy status (LIC, LMIC, UPIC, HIC)
country_table = country_dt %>%
  left_join(economy_dt, by = "country")

# yov_dt = as.data.table(readRDS("supp_data/interim-update-202007wue_summary.rds"))
# gavi_dt <- unique(yov_dt[, .(country, gavi73)])
# setnames(gavi_dt, "country", "country_id")
# gavi_dt[, gavi73 := as.integer(gavi73)]
# 
# country_table <- merge(country_table, gavi_dt, all.x = T)
# country_table[is.na(gavi73), gavi73 := 0]

# TEMP: until we find supp_data/interim-update-202007wue_summary.rds
country_table$gavi73 = FALSE

usethis::use_data(country_table, overwrite = T)

