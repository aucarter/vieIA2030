## Pull in various vaccine coverage data from the WHO website

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
    file.remove(xls)

    dt <- rbindlist(data_list, fill = T)
    setnames(
        dt,
        c("ISO_code", "Cname"),
        c("location_iso3", "location_name")
    )
    dt[, sex_id := 3]
    # NOTE: We are subsetting to specific dose numbers here
    vaccine_short_map <- data.table(
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
    download.file(url, xls, quiet = T, mode = 'wb')
    sheets <- readxl::excel_sheets(xls)
    dt <- suppressWarnings(
        data.table(readxl::read_excel(path = xls, sheet = sheets[2]))
    )
    file.remove(xls)
    dt[, c("WHO_REGION", "Continent", "Asterisc") := NULL]
    setnames(
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
    download.file(url, xls, quiet = T, mode = 'wb')
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
    dt[, value_no_pct := tstrsplit(value_str, "%")[[1]]]
    dt[value_no_pct == "-", value_no_pct := "0"]
    dt[, value := as.numeric(value_no_pct)]
    dt <- dt[, .(location_iso3, location_name, sex_id, year, vaccine_short, value)]

    return(dt)
}

prep_vimc_coverage_data <- function() {
    dt <- fread(
        system.file("extdata", "vimc_coverage.csv", package = "vieIA2030")
    )
    setnames(
        dt, 
        c("country", "vaccine"), 
        c("location_iso3", "vaccine_short")
    )
    dt <- dt[, lapply(.SD, sum), by = .(location_iso3, year, age, disease),
                .SDcols = c("fvps_adjusted", "cohort_size")]
    dt[, value := fvps_adjusted / cohort_size]
    full_dt <- CJ(age = 0:100, year = 2000:2039)
    cov_dt <- rbindlist(lapply(unique(dt$location_iso3), function(c) {
        loc_dt <- dt[location_iso3 == c]
        cohort_loc <- rbindlist(lapply(unique(loc_dt$disease), function(d) {
            vacc_dt <- unique(loc_dt[disease == d , .(age, year, value)])
            vacc_dt <- vacc_dt[, .(value = sum(value)), by = .(age, year)]
            merge_dt <- merge(
                full_dt, 
                vacc_dt, 
                by = c("age", "year"), all.x = T
            )
            cast_mat <- as.matrix(dcast(merge_dt, age ~ year))
            rownames(cast_mat) <- cast_mat[, 1]
            mat <- cast_mat[, -1]
            mat[is.na(mat)] <- 0
            nrow <- dim(mat)[1]
            ncol <- dim(mat)[2]
            for(i in (ncol - 1):1) {
                vec <- mat[, i]
                for(j in (i + 1):min(nrow, ncol)) {
                    if(d == "HepB") {
                        mat[(j - i + 1):nrow, j] <- max(
                            mat[(j - i + 1):nrow, j],
                            vec[1:(nrow - j + i)]
                        )
                    } else {
                        mat[(j - i + 1):nrow, j] <- 1 - 
                            (1 - mat[(j - i + 1):nrow, j]) *
                            (1 - vec[1:(nrow - j + i)]) 
                    }
                    
                }
            }
            cohort_dt <- melt(as.data.table(cbind(age = cast_mat[, 1], mat)), id.vars = "age", variable.name = "year")
            cohort_dt[, year := as.integer(as.character(year))]
            cohort_dt[, disease := d]
        }))
        cohort_loc[, location_iso3 := c]
    }))

        pdf("plots/imputed_coverage.pdf", width = 8, height = 8)
        for(c in sort(unique(cov_dt$location_iso3))) {
            loc_dt <- cov_dt[location_iso3 == c]
            gg <- ggplot(loc_dt, aes(x = year, y = age, fill = value)) + 
                geom_tile() +
                xlab("Year") + ylab("Age") + labs(fill = "Coverage") +
                viridis::scale_fill_viridis(
                    option = "viridis", 
                    direction = -1,
                    limits = c(0, 1)) + 
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                coord_fixed() +
                facet_wrap(~disease, nrow = 2) +
                ggtitle(c)
            print(gg)
        }
        dev.off()
        saveRDS(cov_dt, "outputs/imputed_coverage.rds")
    

    
    
    ## Look at coverage observations by 
    year_age_coverage <- lapply(unique(dt$vaccine_short), function(v) {
        table(dt[vaccine_short == v, .(year, age)])
    })
    names(year_age_coverage) <- unique(dt$vaccine_short)
    return(dt[, .(location_iso3, vaccine_short, age, year,)])
}

wuenic_dt <- prep_wuenic_data()
reported_dt <- prep_reported_coverage_data()
hpv_dt <- prep_hpv_coverage_data()
vimc_dt <- prep_vimc_coverage_data()

## Use DTP for D, T, and P
dtp_dt <- wuenic_dt[vaccine_short == "DTP"]
dtp_add <- rbindlist(
    lapply(c("D", "T", "P"), function(v) {
        copy(dtp_dt)[, vaccine_short := v]
    })
)
wuenic_dt <- rbind(wuenic_dt[vaccine_short != "DTP"], dtp_add)

wuenic_coverage <- rbindlist(
    list(wuenic_dt, hpv_dt, reported_dt),
    use.names = T
)
wuenic_coverage[is.na(value), value := 0]
wuenic_coverage[, value := value / 100]

## Merge on location_id
wuenic_coverage <- merge(
    wuenic_coverage, loc_table[, .(location_iso3, location_id)]
)
wuenic_coverage[, c("location_iso3", "location_name") := NULL]

## Merge on vaccine_id
wuenic_coverage <- merge(
    wuenic_coverage, vaccine_table[, .(vaccine_short, vaccine_id)]
)
wuenic_coverage[, vaccine_short := NULL]

vimc_coverage <- rbind(
    vimc_dt, wuenic_dt [vaccine_short %in% c("D", "T", "P", "BCG")]
)


mydb <- open_connection()
DBI::dbWriteTable(mydb, "coverage_inputs", coverage, overwrite = TRUE)
DBI::dbDisconnect(mydb)