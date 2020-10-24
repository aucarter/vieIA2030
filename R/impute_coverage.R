# Impute_coverage
# TODO: Expand to all ages and sexes
# TODO: Address the sex-specificity of HPV

## Load location table
data(loc_table)

## Pull in coverage data
mydb <- DBI::dbConnect(RSQLite::SQLite(), "vieIA2030.db")
cov_dt <- data.table::as.data.table(
    dplyr::collect(
        dplyr::tbl(mydb, "coverage_inputs")
    )
)
data.table::setnames(cov_dt, "value", "coverage")

## Load SDI and merge on
data(gbd_sdi)
gbd_sdi[, c("country_name", "country_id") := NULL]
data.table::setnames(gbd_sdi, "value", "sdi")
dt <- merge(cov_dt, gbd_sdi, by = c("country_iso3", "year"))
dt <- dt[coverage > 0]

## Model coverage as function of SDI
fit_coverage <- lm(
    coverage ~ 1 + sdi + vaccine_short,
    dt
)

## Predict for missing locations
missing_locs  <- setdiff(loc_table$country_iso3, dt$country_iso3)
pred_dt <- data.table::rbindlist(
    lapply(
        unique(cov_dt$vaccine_short), function(vacc) {
            gbd_sdi[country_iso3 %in% missing_locs][, vaccine_short := vacc]
        }
    )
)
pred_dt[, coverage := predict(fit_coverage, pred_dt)]

## Predict start year
start_dt <- dt[, .(year = min(year)), by = .(country_iso3, vaccine_short)]
merge_dt <- merge(
    start_dt,
    dt[, .(country_iso3, year, vaccine_short, sdi)],
    by = c("country_iso3", "year", "vaccine_short")
)
fit_start <- lm(
    year ~ 1 + sdi + vaccine_short,
    merge_dt
)
pred_dt[, start_year := round(predict(fit_start, pred_dt))]
pred_dt[year < start_year, coverage := 0]
pred_dt[, start_year := NULL]

coverage <- rbind(cov_dt, pred_dt)