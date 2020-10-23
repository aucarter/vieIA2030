# Prep GBD 2019 SDI for use as a covariate
dt <- fread("inst/extdata/gbd19_sdi.csv", header = T)
melt_dt <- data.table::melt(
    dt, id.vars = "Location", variable.name = "year"
)
melt_dt[, year := as.integer(as.character(year))]

data(loc_table)
setnames(melt_dt, "Location", "country_name")
gbd_sdi <- merge(loc_table, melt_dt)
usethis::use_data(gbd_sdi, overwrite = TRUE)