load_tables("coverage")

# Grab this from the coverage file just to identify the VIMC HPV locations
prep_vimc_coverage_data <- function() {
    dt <- fread(
        system.file("extdata", "vimc_coverage.csv", package = "vieIA2030")
    )
    setnames(dt,"country", "location_iso3")
    vimc_dt <- dt[, lapply(.SD, sum),
             by = .(location_iso3, disease, vaccine, activity_type, year, age, gender),
             .SDcols = c("fvps_adjusted", "cohort_size")]
    vimc_dt[, coverage := fvps_adjusted / cohort_size]
    vimc_dt[, c("disease") := NULL]
    vimc_dt <- merge(vimc_dt,
        data.table(gender = c("Both", "Female", "Male"), sex_id = c(3, 2, 1)),
        by = "gender")
    vimc_dt[, gender := NULL]
    setnames(vimc_dt, "fvps_adjusted", "fvps")

    return(vimc_dt[])
}
vimc_dt <- prep_vimc_coverage_data()
hpv_vimc_locs <- unique(vimc_dt[vaccine == "HPV"]$location_iso3)

# Merge on location_iso3
cov_temp <- merge(coverage, loc_table[, .(location_iso3, location_id)], by = "location_id")
# Subset to non-VIMC countries and grab the nine year olds
sub_cov <- cov_temp[!(location_iso3 %in% hpv_vimc_locs) & v_at_id == 1 & age == 9]
# Swap from campaign to routine
sub_cov[, v_at_id := 2]
# Execute the substitution
upload_cov <- rbind(
    # NOT non-VIMC campaign HPV
    cov_temp[!(!(location_iso3 %in% hpv_vimc_locs) & v_at_id == 1)],
    sub_cov
)
upload_cov[, location_iso3 := NULL]

upload_object(coverage, "coverage")
