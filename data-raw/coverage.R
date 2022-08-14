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

prep_wuenic_data <- function() {
    url <- "http://www.who.int/entity/immunization/monitoring_surveillance/data/coverage_estimates_series.xls"
    xls <- tempfile()
    download.file(url, xls, quiet = T, mode = 'wb')
    sheets <- readxl::excel_sheets(xls)
    data_list <- lapply(sheets[2:15], function(s) {
        df <- readxl::read_excel(path = xls, sheet = s)
        df$Region <- NULL
        df <- tidyr::pivot_longer(
            df,
            cols = !c("ISO_code", "Cname", "Vaccine"),
            names_to = "year",
            names_transform = list(year = as.integer)
        )
    })
    temp <- file.remove(xls)

    dt <- rbindlist(data_list, fill = T)
    dt[, Cname := NULL]
    setnames(dt, "ISO_code", "location_iso3")
    setnames(dt, "Vaccine", "wuenic_name")
    wuenic_dt <- merge(dt, wuenic_vaccine_table, by = "wuenic_name")
    wuenic_dt[, wuenic_name := NULL]

    wuenic_dt[is.na(value), value := 0]
    wuenic_dt[, coverage := value / 100]
    wuenic_dt[, c("age", "activity_type", "sex_id", "value") := .(0, "routine", 3, NULL)]

    return(wuenic_dt[])
}


prep_reported_data <- function() {
    url <- "http://www.who.int/entity/immunization/monitoring_surveillance/data/coverage_series.xls"
    xls <- tempfile()
    download.file(url, xls, quiet = T, mode = 'wb')
    sheets <- readxl::excel_sheets(xls)
    dt <- suppressWarnings(
        data.table(readxl::read_excel(path = xls, sheet = sheets[2]))
    )
    temp <- file.remove(xls)
    dt[, c("WHO_REGION", "Continent", "Asterisc") := NULL]
    setnames(
        dt,
        c("ISO_code", "Vaccine", "Year", "Percent_covrage"),
        c("location_iso3", "vaccine", "year", "value")
    )
    dt <- dt[vaccine %in% c("JapEnc", "MenA")]
    dt[vaccine == "JapEnc", vaccine := "JE"]
    dt[, sex_id := 3]
    dt[, coverage := value / 100]
    dt <- dt[, .(location_iso3, sex_id, year, vaccine, coverage)]
    je_dt <- rbindlist(lapply(0:14, function(a) {
        copy_dt <- copy(dt[vaccine == "JE"])
        copy_dt[, age := a]
    }))
    mena_dt <- rbindlist(lapply(0:29, function(a) {
        copy_dt <- copy(dt[vaccine == "MenA"])
        copy_dt[, age := a]
    }))
    dt <- rbind(je_dt, mena_dt)
    dt[, activity_type := "campaign"]

    return(dt)
}

prep_hpv_data <- function() {
    url <- "http://www.who.int/immunization/monitoring_surveillance/data/HPV_estimates.xlsx"
    xls <- tempfile()
    download.file(url, xls, quiet = T, mode = 'wb')
    sheets <- readxl::excel_sheets(xls)
    dt <- data.table(readxl::read_excel(path = xls, sheet = sheets[2]))
    temp <- file.remove(xls)

    # NOTE: We are subsetting to only those received a complete dosage!!
    dt <- dt[grepl("prHPVc", indicator)]
    dt[, vaccine := "HPV"]
    setnames(dt, "iso3code", "location_iso3")
    dt[, sex_id := ifelse(sex == "Male", 1, 2)]
    dt[, value_no_pct := tstrsplit(value_str, "%")[[1]]]
    dt[value_no_pct == "-", value_no_pct := "0"]
    dt[, coverage := as.numeric(value_no_pct) / 100]
    dt <- dt[, .(location_iso3, sex_id, year, vaccine, coverage)]
    # Give the same coverage level to age 8 through 13
    dt[, age := 9]
    dt[, activity_type := "routine"]

    return(dt)
}

vimc_dt <- prep_vimc_coverage_data()
wuenic_dt <- prep_wuenic_data()
reported_dt <- prep_reported_data()
hpv_dt <- prep_hpv_data()

non_vimc_dt <- rbindlist(
    list(wuenic_dt, reported_dt, hpv_dt), use.names = T
)
# Sub in WUENIC data for locations not in VIMC
for (v in unique(non_vimc_dt$vaccine)) {
    non_vimc_dt <- non_vimc_dt[
        !(vaccine == v &
        location_iso3 %in% unique(vimc_dt[vaccine == v]$location_iso3)
        )
    ]
}
# Merge on cohort size and calculate fvps
load_tables("wpp_input")
both_dt <- wpp_input[, .(cohort_size = sum(nx)), by = .(location_id, year, age)]
non_vimc_dt <- merge(non_vimc_dt, loc_table[, .(location_id, location_iso3)])
non_vimc_dt <- merge(non_vimc_dt, both_dt, by = c("location_id", "year", "age"))
non_vimc_dt[, fvps := coverage * cohort_size]

vimc_dt <- merge(vimc_dt, loc_table[, .(location_id, location_iso3)])
coverage <- rbind(vimc_dt, non_vimc_dt)
coverage[, location_iso3 := NULL]

## Merge on vaccine_id
coverage <- merge(
    coverage, v_at_table[, .(vaccine, activity_type, v_at_id)],
    by = c("vaccine", "activity_type")
)
coverage[, c("vaccine", "activity_type") := NULL]
coverage[, age := as.integer(age)]
coverage[, sex_id := as.integer(sex_id)]
coverage <- coverage[order(location_id, v_at_id, year, age, sex_id),
                 .(location_id, v_at_id, year, age, sex_id, fvps, coverage)]

coverage <- coverage[fvps != 0]

upload_object(coverage, "coverage")
