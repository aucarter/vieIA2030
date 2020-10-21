
#' Pull all vaccine coverage data for user supplied list of locations
#' 
#' @param iso3 Vector of iso3 codes
#' @return A data.table with historical coverage for all vaccines for each
#'         supplied location
#' @export
prep_coverage <- function(iso3 = NULL) {
    wuenic_dt <- prep_wuenic_data()
    hpv_dt <- prep_hpv_coverage_data()
    dt <- rbind(wuenic_dt, hpv_dt)[country_iso3 %in% iso3]
    dt[is.na(value), value := 0]

    class(dt) <- c("coverage", class(dt))

    return(dt)
}

#' Pull in WUENIC data and bind it together
#' @return A data.table with all WUENIC data
#' @export
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
        c("ISO_code", "Cname", "Vaccine"),
        c("country_iso3", "country_name", "vaccine_short")
    )
    dt[, sex_id := 3]

    return(dt)
}

#' Pull in HPV coverage data
#' @return A data.table with all HPV data
#' @export
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
        c("country_iso3", "country_name")
    )
    dt[, sex_id := ifelse(sex == "Male", 1, 2)]
    dt[, value_no_pct := data.table::tstrsplit(value_str, "%")[[1]]]
    dt[value_no_pct == "-", value_no_pct := "0"]
    dt[, value := as.numeric(value_no_pct)]
    dt <- dt[, .(country_iso3, country_name, sex_id, year, vaccine_short, value)]

    return(dt)
}