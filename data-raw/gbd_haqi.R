# Prep GBD 2019 HAQI for use as a covariate

## Read data
dt <- data.table::fread(
    system.file("extdata", "gbd19_haqi.csv", package = "vieIA2030"),
    header = T
)

## Location mapping
gbd_haqi <- merge(loc_table, dt, all.x = T, by = "location_name")
setnames(gbd_haqi, c("year_id", "val"), c("year", "value"))

## Ignore uncertainty (for now)
gbd_haqi[, c("upper", "lower") := NULL]

usethis::use_data(gbd_haqi, overwrite = TRUE)