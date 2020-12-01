open_connection <- function() {
    my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "vieIA2030.db")

    return(my_db)
}

#' Generates the SQLite database with input data
gen_db <- function() {
    if (!file.exists("vieIA2030.db")) {
        message("No database found -- building now")

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

db_pull <- function(table, iso3_list = NULL) {
    if (!is.null(iso3_list)) {
        loc_ids <- loc_table[location_iso3 %in% iso3_list]$location_id
    } else {
        loc_ids <- loc_table$location_id
    }
    mydb <- open_connection()
    dt <- as.data.table(
        tbl(mydb, table) %>%
        filter(location_id %in% loc_ids) %>%
        collect() %>%
        left_join(
            loc_table[, .(location_id, location_iso3, location_name)],
            by = "location_id"
        )
    )
    if ("vaccine_id" %in% names(dt)) {
        dt <- left_join(dt, vaccine_table, by = "vaccine_id")
    }
    DBI::dbDisconnect(mydb)

    return(dt)
}