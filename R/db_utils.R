open_connection <- function() {
    db_path <- system.file("vieIA2030.db", package = "vieIA2030")
    my_db <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    return(my_db)
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
    DBI::dbDisconnect(mydb)

    return(dt)
}