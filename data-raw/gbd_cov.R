# Prep GBD 2019 SDI for use as a covariate

## Read data
dt <- fread(
    system.file("extdata", "gbd19_sdi.csv", package = "vieIA2030"),
    header = T
)

## Get rid of Georgia the state
dt <- dt[-which(dt$Location == "Georgia")[2]]

## Make long
melt_dt <- melt(
    dt, id.vars = "Location", variable.name = "year"
)
melt_dt[, year := as.integer(as.character(year))]
melt_dt[, value := gsub("·", ".", value)]
melt_dt[, sdi := as.numeric(value)]
melt_dt[, value := NULL]
melt_dt <- unique(melt_dt)

## Location mapping
setnames(melt_dt, "Location", "gbd_alt_name")
gbd_sdi <- merge(loc_table, melt_dt, all.x = T, by = "gbd_alt_name")

# GBD 2019 HAQI for use as a covariate

## Read data
dt <- fread(
    system.file("extdata", "gbd19_haqi.csv", package = "vieIA2030"),
    header = T
)

## Get rid of Georgia the state
g_idx <- which(dt$location_name == "Georgia")
state_idx <- g_idx[(length(g_idx) / 2 + 1):length(g_idx)]
dt <- dt[-state_idx]


## Location mapping
gbd_haqi <- merge(loc_table, dt, all.x = T, by = "location_name")
setnames(gbd_haqi, c("year_id", "val"), c("year", "haqi"))

## Ignore uncertainty (for now)
gbd_haqi[, c("upper", "lower") := NULL]
gbd_haqi[, haqi := haqi / 100]

gbd_cov <- merge(
    gbd_haqi[, .(location_id, year, haqi)],
    gbd_sdi[, .(location_id, year, sdi)],
    by = c("location_id", "year")
)

usethis::use_data(gbd_cov, overwrite = TRUE)