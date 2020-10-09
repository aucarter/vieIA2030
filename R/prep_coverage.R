#' Pull in WUENIC data and bind it together
#' @return A data.table with all WUENIC data
prep_wuenic_data <- function() {
    url <- "www.who.int/entity/immunization/monitoring_surveillance/data/coverage_estimates_series.xls"
    xls <- tempfile()
    download.file(url, xls)
    sheets <- readxl::excel_sheets(xls)
    data_list <- lapply(sheets[2:15], function(s) {
        df <- readxl::read_excel(path = xls, sheet = s)
        df <- tidyr::pivot_longer(
            df,
            cols = !c("Region", "ISO_code", "Cname", "Vaccine"),
            names_to = "year",
            names_transform = list(year = as.integer)
        )
    })
    file.remove(xls)

    dt <- data.table::rbindlist(data_list, fill = T)

    return(dt)
}
