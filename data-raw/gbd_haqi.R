# Prep GBD 2019 HAQI for use as a covariate

## Read data
dt <- data.table::fread(
    system.file("extdata", "gbd19_haqi.csv", package = "vieIA2030"),
    header = T
)

## Location mapping
data(loc_table)
data.table::setnames(dt, "location_name", "country_name")
loc_map <- data.table::fread(
    system.file("extdata", "un_gbd_loc_map.csv", package = "vieIA2030")
)
data.table::setnames(loc_map, "gbd_name", "country_name")
loc_merge <- merge(dt, loc_map, all.x = T)
loc_merge[!is.na(un_name), country_name := un_name]
loc_merge[, un_name := NULL]
gbd_haqi <- merge(loc_table, loc_merge, all.x = T, by = "country_name")
setnames(gbd_haqi, c("year_id", "val"), c("year", "value"))
usethis::use_data(gbd_haqi, overwrite = TRUE)