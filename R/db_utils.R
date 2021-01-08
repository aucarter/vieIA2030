open_connection <- function() {
    bigrquery::bq_auth(path = "gcs_creds.json")
    my_db <- DBI::dbConnect(
        bigrquery::bigquery(),
        project = "vaccine-impact",
        dataset = "data"
    )

    return(my_db)
}

#' Generates the SQLite database with input data
gen_db <- function() {
    response <- menu(c("Yes", "No"), title = "Delete and rebuild database?")
    if (response == 1) {
        message("   - Prepping vaccine coverage...")
        source("data-raw/coverage.R")

        message("   - Prepping WPP inputs and all-cause deaths...")
        source("data-raw/wpp_input.R")

        message("   - Prepping WPP observed...")
        source("data-raw/obs_wpp.R")

        if (file.exists("inst/extdata/vimc_estimates.csv")) {
            message("   - Prepping VIMC impact estimates...")
            source("data-raw/vimc_impact.R")
        } else {
            warning("Add VIMC estimates to inst/extdata")
        }

        message("Done!")
    }
}

list_db_tables <- function() {
    mydb <- open_connection()
    tables <- DBI::dbListTables(mydb)
    DBI::dbDisconnect(mydb)

    return(tables)
}

db_pull <- function(table, iso3_list = NULL, append_names = F) {
    if (!is.null(iso3_list)) {
        loc_ids <- loc_table[location_iso3 %in% iso3_list]$location_id
    } else {
        loc_ids <- loc_table$location_id
    }
    mydb <- open_connection()
    dt <- as.data.table(
        tbl(mydb, table) %>%
        filter(location_id %in% loc_ids) %>%
        collect()
    )
    DBI::dbDisconnect(mydb)
    if (append_names) {
        if ("location_id" %in% names(dt)) {
            dt <- left_join(
                dt,
                loc_table[, .(location_id, location_iso3, location_name)],
                by = "location_id"
            )
        }
        if ("vaccine_id" %in% names(dt)) {
            dt <- left_join(
                dt,
                vaccine_table[, .(vaccine_id, vaccine_short)],
                by = "vaccine_id"
            )
        }
    }

    return(dt)
}

load_tables <- function(table_list) {
    unloaded_data <- setdiff(table_list, ls(envir = .GlobalEnv))
    if (length(unloaded_data) > 0) {
        temp <- lapply(unloaded_data, function(table) {
            dt <- db_pull(table)
            assign(table, dt, envir = .GlobalEnv)
        })
    }
}

upload_object <- function(object, name) {
    file <- paste0(name, ".csv")
    func_path <- system.file("upload_file.py", package = "vieIA2030")
    write.csv(object, file, row.names = F)
    system(paste("python3", func_path, file, name))
    file.remove(file)
}
