# Prep GBD 2019 SDI for use as a covariate

## Read data
dt <- data.table::fread(
    system.file("extdata", "gbd19_sdi.csv", package = "vieIA2030"),
    header = T
)

## Make long
melt_dt <- data.table::melt(
    dt, id.vars = "Location", variable.name = "year"
)
melt_dt[, year := as.integer(as.character(year))]
melt_dt[, value := gsub("·", ".", value)]
melt_dt[, value := as.numeric(value)]
melt_dt <- unique(melt_dt)

## Location mapping
data(loc_table)
data.table::setnames(melt_dt, "Location", "country_name")
loc_map <- data.table::fread("inst/extdata/un_gbd_loc_map.csv")
data.table::setnames(loc_map, "gbd_name", "country_name")
loc_merge <- merge(melt_dt, loc_map, all.x = T)
loc_merge[!is.na(un_name), country_name := un_name]
loc_merge[, un_name := NULL]
gbd_sdi <- merge(loc_table, loc_merge, all.x = T, by = "country_name")
gbd_sdi[, c("country_name", "country_iso3") := NULL]
usethis::use_data(gbd_sdi, overwrite = TRUE)