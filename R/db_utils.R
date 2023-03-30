###########################################################
# DATABASE UTILITIES
#
# Functions required to connect to SQLite databases, pull
# the relevant data, and load to the global environment.
#
###########################################################

# ---------------------------------------------------------
# Parent function to load (possibly several) database tables to global environment
# Called by: numerous funcions
# ---------------------------------------------------------
load_tables <- function(...) {
  
  # All tables we want to load
  tables_to_load = list(...) %>%
    unlist() %>%
    setdiff(ls(envir = .GlobalEnv))  # Check if any have already been loaded
  
  # Loop through tables to load
  for (table in tables_to_load) {
    
    # Pull data from database
    dt = db_pull(table)
    
    # Assign to global environment
    assign(table, dt, envir = .GlobalEnv)
  }
  
  browser()
}

# ---------------------------------------------------------
# Pull data from database for all or specific locations
# Called by: load_tables(), numerous other funcions
# ---------------------------------------------------------
db_pull <- function(table, iso3_list = NULL, append_names = F) {
  
  # Location IDs - either or all a specified subset
  # if (!is.null(iso3_list)) {
  #   loc_ids <- loc_table[location_iso3 %in% iso3_list]$location_id
  # } else {
  #   loc_ids <- loc_table$location_id
  # }
  
  # Open database connection
  db_con <- open_connection()
  
  # Pull the desired table from the database
  db_dt = tbl(db_con, table) %>%
    collect() %>%
    quiet() %>%
    # filter(location_id %in% loc_ids) %>%
    as.data.table()
  
  # Close database connection
  DBI::dbDisconnect(db_con)
  
  browser()
  
  # Check if we want to append location and/or vaccine details
  if (append_names) {
    
    # Details we may wish to append
    location_dt = loc_table[, .(location_id, location_iso3, location_name)]
    vaccine_dt  = vaccine_table[, .(vaccine_id, vaccine, vaccine_long)]  # NOTE: can't see where vaccine_short is defined
    
    if ("location_id" %in% names(db_dt))
      db_dt %<>% left_join(location_dt, by = "location_id")
    
    if ("vaccine_id" %in% names(db_dt))
      db_dt %<>% left_join(vaccine_dt, by = "vaccine_id")
  }
  
  return(db_dt)
}

# ---------------------------------------------------------
# Open database connection
# Called by: db_pull(), list_db_tables()
# ---------------------------------------------------------
open_connection <- function() {
  
  # Fetch token defined in gcs_creds.json file to authorise database query
  bigrquery::bq_auth(path = "gcs_creds.json")
  
  # Create a database connecion
  db_con = DBI::dbConnect(bigrquery::bigquery(),
                          project = "vaccine-impact",
                          dataset = "data")
  
  return(db_con)
}

# ---------------------------------------------------------
# Generates the SQLite database with input data
# Called by: ?
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# xxx
# Called by: various functions in data-raw directory - via gen_db()
# ---------------------------------------------------------
upload_object <- function(object, name) {
    file <- paste0(name, ".csv")
    func_path <- system.file("upload_file.py", package = "vieIA2030")
    write.csv(object, file, row.names = F)
    system(paste("python3", func_path, file, name))
    file.remove(file)
}

# ---------------------------------------------------------
# Helper function to list available database tables
# Called by: none
# ---------------------------------------------------------
list_db_tables <- function() {
  
  # Open database connection
  db_con <- open_connection()
  
  # Return tables accessible through this connection
  tables <- DBI::dbListTables(db_con)
  
  # Close the connection
  DBI::dbDisconnect(db_con)
  
  return(tables)
}

