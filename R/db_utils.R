list_db_tables <- function() {
    mydb <- DBI::dbConnect(RSQLite::SQLite(), "vieIA2030.db")
    DBI::dbListTables(mydb)
}

db_pull <- function(table, iso3_list = NULL) {
    if (!is.null(iso3_list)) {
        loc_ids <- loc_table[location_iso3 %in% iso3_list]$location_id
    } else {
        loc_ids <- loc_table$location_id
    }
    mydb <- DBI::dbConnect(RSQLite::SQLite(), "vieIA2030.db")
    dt <- as.data.table(
        tbl(mydb, table) %>%
        filter(location_id %in% loc_ids) %>%
        collect() %>%
        left_join(
            loc_table[, .(location_id, location_iso3)],
            by = "location_id"
        )
    )
    DBI::dbDisconnect(mydb)

    return(dt)
}