# Prep GBD 2019 SDI for use as a covariate

## Read data
dt <- data.table::fread("inst/extdata/gbd19_sdi.csv", header = T)

## Make long
melt_dt <- data.table::melt(
    dt, id.vars = "Location", variable.name = "year"
)
melt_dt[, year := as.integer(as.character(year))]
melt_dt[, value := gsub("·", ".", value)]
melt_dt[, value := as.numeric(value)]
melt_dt <- unique(melt_dt)

## Use Western Europe for Liechtenstein
melt_dt[Location == "Western Europe", Location := "Liechtenstein"]

## Use North Africa and Middle East for Palestine
melt_dt[Location == "North Africa and Middle East", Location := "Palestine"]

## Use Italy for Holy See
hs_dt <- data.table::copy(melt_dt[Location == "Italy"])
hs_dt[, Location := "Holy See"]
bind_dt <- rbind(melt_dt, hs_dt)

## Location mapping
data(loc_table)
data.table::setnames(bind_dt, "Location", "country_name")
loc_map <- data.table::fread("inst/extdata/un_gbd_loc_map.csv")
data.table::setnames(loc_map, "gbd_name", "country_name")
loc_merge <- merge(bind_dt, loc_map, all.x = T)
loc_merge[!is.na(un_name), country_name := un_name]
loc_merge[, un_name := NULL]
gbd_sdi <- merge(loc_table, loc_merge, all.x = T, by = "country_name")
usethis::use_data(gbd_sdi, overwrite = TRUE)