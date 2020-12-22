## Pull in various vaccine coverage data from the WHO website
calc_total_cov <- function(admin_dt) {
    full_dt <- CJ(
        age = 0:100, year = 2000:2039, activity_type = c("routine", "campaign")
    )
    merge_dt <- merge(
        full_dt,
        admin_dt[, .(activity_type, age, year, value)],
        by = c("activity_type", "age", "year"), all.x = T
    )
    merge_dt[is.na(value), value := 0]
    mat <- as.matrix(
        dcast(
            merge_dt[activity_type == "routine"],
            age ~ year, value.var = "value"
        )
    )[, -1]
    nrow <- dim(mat)[1]
    ncol <- dim(mat)[2]

    # Routine -- take the max
    for (i in (ncol - 1):1) {
        vec <- mat[, i]
        for (j in (i + 1):min(nrow, ncol)) {
            mat[(j - i + 1):nrow, j] <- pmax(
                mat[(j - i + 1):nrow, j],
                vec[1:(nrow - j + i)]
            )
        }
    }

    c_mat <- as.matrix(
        dcast(
            merge_dt[activity_type == "campaign"],
            age ~ year, value.var = "value"
        )
    )[, -1]
    # Campaign - Assume independence
    for (i in (ncol - 1):1) {
        vec <- c_mat[, i]
        for (j in (i + 1):min(nrow, ncol)) {
            mat[(j - i + 1):nrow, j] <- 1 -
                (1 - mat[(j - i + 1):nrow, j]) *
                (1 - vec[1:(nrow - j + i)])
        }
    }
    dt <- melt(
        as.data.table(cbind(age = 0:100, mat)),
        id.vars = "age", variable.name = "year"
    )
    dt[, year := as.integer(as.character(year))]

    return(dt[])
}

prep_vimc_coverage_data <- function() {
    dt <- fread(
        system.file("extdata", "vimc_coverage.csv", package = "vieIA2030")
    )
    setnames(
        dt,
        c("country", "disease"),
        c("location_iso3", "vaccine_short")
    )
    vimc_dt <- dt[, lapply(.SD, sum),
             by = .(location_iso3, vaccine_short, activity_type, year, age),
             .SDcols = c("fvps_adjusted", "cohort_size")]
    vimc_dt[, value := fvps_adjusted / cohort_size]
    vimc_dt[, c("fvps_adjusted", "cohort_size") := NULL]
    vimc_dt[, sex_id := ifelse(vaccine_short == "HPV", 2, 3)]

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
    file.remove(xls)

    dt <- rbindlist(data_list, fill = T)
    dt[, Cname := NULL]
    setnames(dt, "ISO_code", "location_iso3")
    setnames(dt, "Vaccine", "wuenic_name")
    wuenic_dt <- merge(dt, wuenic_vaccine_table, by = "wuenic_name")
    wuenic_dt[, wuenic_name := NULL]

    wuenic_dt[is.na(value), value := 0]
    wuenic_dt[, value := value / 100]
    wuenic_dt[, c("age", "activity_type", "sex_id") := .(0, "routine", 3)]

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
    file.remove(xls)
    dt[, c("WHO_REGION", "Continent", "Asterisc") := NULL]
    setnames(
        dt,
        c("ISO_code", "Vaccine", "Year", "Percent_covrage"),
        c("location_iso3", "vaccine_short", "year", "value")
    )
    dt <- dt[vaccine_short %in% c("JapEnc", "MenA")]
    dt[vaccine_short == "JapEnc", vaccine_short := "JE"]
    dt[, sex_id := 3]
    dt[, value := value / 100]
    dt <- dt[, .(location_iso3, sex_id, year, vaccine_short, value)]
    je_dt <- rbindlist(lapply(0:14, function(a) {
        copy_dt <- copy(dt[vaccine_short == "JE"])
        copy_dt[, age := a]
    }))
    mena_dt <- rbindlist(lapply(0:29, function(a) {
        copy_dt <- copy(dt[vaccine_short == "MenA"])
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
    file.remove(xls)

    # NOTE: We are subsetting to only those received a complete dosage!!
    dt <- dt[grepl("prHPVc", indicator)]
    dt[, vaccine_short := "HPV"]
    setnames(dt, "iso3code", "location_iso3")
    dt[, sex_id := ifelse(sex == "Male", 1, 2)]
    dt[, value_no_pct := tstrsplit(value_str, "%")[[1]]]
    dt[value_no_pct == "-", value_no_pct := "0"]
    dt[, value := as.numeric(value_no_pct) / 100]
    dt <- dt[, .(location_iso3, sex_id, year, vaccine_short, value)]
    # Give the same coverage level to age 8 through 13
    dt <- rbindlist(lapply(8:13, function(a) {
        copy_dt <- copy(dt)
        copy_dt[, age := a]
    }))
    dt[, activity_type := "campaign"]

    return(dt)
}

vimc_dt <- prep_vimc_coverage_data()
wuenic_dt <- prep_wuenic_data()
reported_dt <- prep_reported_data()
hpv_dt <- prep_hpv_data()

non_vimc_dt <- rbindlist(
    list(wuenic_dt, reported_dt, hpv_dt), use.names = T
)
for (v in unique(non_vimc_dt$vaccine_short)) {
    non_vimc_dt <- non_vimc_dt[
        !(vaccine_short == v &
        location_iso3 %in% unique(vimc_dt[vaccine_short == v]$location_iso3)
        )
    ]
}
all_dt <- rbind(vimc_dt, non_vimc_dt)

## Calculate total coverage for every vaccine, location, sex
coverage <- rbindlist(lapply(unique(all_dt$vaccine_short), function(v) {
    v_dt <- all_dt[vaccine_short == v]
    vacc_dt <- rbindlist(lapply(unique(v_dt$location_iso3), function(l) {
        l_dt <- v_dt[location_iso3 == l]
        loc_dt <- rbindlist(lapply(unique(l_dt$sex_id), function(s) {
            s_dt <- l_dt[sex_id == s]
            sex_dt <- calc_total_cov(s_dt)
            sex_dt[, sex_id := s]
        }))
        loc_dt[, location_iso3 := l]
        return(loc_dt)
    }))
    vacc_dt <- vacc_dt[, vaccine_short := v]
    # Set a cap on BCG effect at age 15
    if (v == "BCG") {
        vacc_dt[age >= 15, value := 0]
    }
    return(vacc_dt)
}))

## Merge on location_id
coverage <- merge(
    coverage, loc_table[, .(location_iso3, location_id)]
)
coverage[, location_iso3 := NULL]

## Merge on vaccine_id
coverage <- merge(
    coverage, vaccine_table[, .(vaccine_short, vaccine_id)]
)
coverage[, vaccine_short := NULL]

mydb <- open_connection()
DBI::dbWriteTable(
    conn = mydb,
    name = "coverage_inputs",
    value = coverage,
    fields = bigrquery::as_bq_fields(coverage),
    overwrite = TRUE
)
DBI::dbDisconnect(mydb)