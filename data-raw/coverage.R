## Pull in various vaccine coverage data from the WHO website

prep_wuenic_data <- function() {
    url <- "www.who.int/entity/immunization/monitoring_surveillance/data/coverage_estimates_series.xls"
    xls <- tempfile()
    download.file(url, xls, quiet = T)
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
    file.remove(xls)

    dt <- data.table::rbindlist(data_list, fill = T)
    data.table::setnames(
        dt,
        c("ISO_code", "Cname"),
        c("location_iso3", "location_name")
    )
    dt[, sex_id := 3]
    # NOTE: We are subsetting to specific dose numbers here
    vaccine_short_map <- data.table::data.table(
        Vaccine = c("Hib3", "RCV1", "RotaC", "YFV", "Pol3", "HepB3", "MCV1",
            "PCV3", "DTP3", "BCG"),
        vaccine_short = c("Hib", "Rubella", "Rota", "YF", "Polio", "HepB",
            "Measles", "PCV", "DTP", "BCG")
    )
    dt <- merge(dt, vaccine_short_map, by = "Vaccine")
    dt[, Vaccine := NULL]

    return(dt)
}

prep_reported_coverage_data <- function() {
    url <- "http://www.who.int/entity/immunization/monitoring_surveillance/data/coverage_series.xls"
    xls <- tempfile()
    download.file(url, xls, quiet = T)
    sheets <- readxl::excel_sheets(xls)
    dt <- suppressWarnings(
        data.table(readxl::read_excel(path = xls, sheet = sheets[2]))
    )
    file.remove(xls)
    dt[, c("WHO_REGION", "Continent", "Asterisc") := NULL]
    data.table::setnames(
        dt,
        c("ISO_code", "Cname", "Vaccine", "Year", "Percent_covrage"),
        c("location_iso3", "location_name", "vaccine_short", "year", "value")
    )
    dt <- dt[vaccine_short %in% c("JapEnc", "MenA")]
    dt[vaccine_short == "JapEnc", vaccine_short := "JE"]
    dt[, sex_id := 3]

    return(dt)
}

prep_hpv_coverage_data <- function() {
    url <- "http://www.who.int/immunization/monitoring_surveillance/data/HPV_estimates.xlsx"
    xls <- tempfile()
    download.file(url, xls, quiet = T)
    sheets <- readxl::excel_sheets(xls)
    dt <- data.table(readxl::read_excel(path = xls, sheet = sheets[2]))
    file.remove(xls)

    # NOTE: We are subsetting to only those received a complete dosage!!
    dt <- dt[grepl("prHPVc", indicator)]
    dt[, vaccine_short := "HPV"]
    setnames(
        dt,
        c("iso3code", "area_name"),
        c("location_iso3", "location_name")
    )
    dt[, sex_id := ifelse(sex == "Male", 1, 2)]
    dt[, value_no_pct := data.table::tstrsplit(value_str, "%")[[1]]]
    dt[value_no_pct == "-", value_no_pct := "0"]
    dt[, value := as.numeric(value_no_pct)]
    dt <- dt[, .(location_iso3, location_name, sex_id, year, vaccine_short, value)]

    return(dt)
}

wuenic_dt <- prep_wuenic_data()
hpv_dt <- prep_hpv_coverage_data()
reported_dt <- prep_reported_coverage_data()

coverage <- data.table::rbindlist(
    list(wuenic_dt, hpv_dt, reported_dt),
    use.names = T
)
coverage[is.na(value), value := 0]
coverage[, value := value / 100]

## Merge on location_id
coverage <- merge(coverage, loc_table[, .(location_iso3, location_id)])
coverage[, c("location_iso3", "location_name") := NULL]

## Use DTP for D, T, and P
dtp_dt <- coverage[vaccine_short == "DTP"]
dtp_add <- rbindlist(
    lapply(c("D", "T", "P"), function(v) {
        copy(dtp_dt)[, vaccine_short := v]
    })
)
coverage <- rbind(coverage[vaccine_short != "DTP"], dtp_add)

## Merge on vaccine_id
coverage <- merge(coverage, vaccine_table[, .(vaccine_short, vaccine_id)])
coverage[, vaccine_short := NULL]

mydb <- open_connection()
DBI::dbWriteTable(mydb, "coverage_inputs", coverage, overwrite = TRUE)
DBI::dbDisconnect(mydb)