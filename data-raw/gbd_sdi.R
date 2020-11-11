# Prep GBD 2019 SDI for use as a covariate

## Read data
dt <- fread(
    system.file("extdata", "gbd19_sdi.csv", package = "vieIA2030"),
    header = T
)

## Make long
melt_dt <- melt(
    dt, id.vars = "Location", variable.name = "year"
)
melt_dt[, year := as.integer(as.character(year))]
melt_dt[, value := gsub("·", ".", value)]
melt_dt[, value := as.numeric(value)]
melt_dt <- unique(melt_dt)

## Location mapping
setnames(melt_dt, "Location", "gbd_alt_name")
gbd_sdi <- merge(loc_table, melt_dt, all.x = T, by = "gbd_alt_name")
usethis::use_data(gbd_sdi, overwrite = TRUE)