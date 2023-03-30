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
    dt <- db_pull(table)
    
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
  if (!is.null(iso3_list)) {
    loc_ids <- loc_table[location_iso3 %in% iso3_list]$location_id
  } else {
    loc_ids <- loc_table$location_id
  }
  
  # Open database connection
  mydb <- open_connection()
  
  browser()
  
  dt <- as.data.table(
    tbl(mydb, table) %>%
      # filter(location_id %in% loc_ids) %>%
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

# ---------------------------------------------------------
# Open database connection
# Called by: db_pull(), list_db_tables()
# ---------------------------------------------------------
open_connection <- function() {
  
  bigrquery::bq_auth(path = "gcs_creds.json")
  
  browser()
  
  my_db <- DBI::dbConnect(
    bigrquery::bigquery(),
    project = "vaccine-impact",
    dataset = "data"
  )
  
  return(my_db)
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
# Helper function (?) to list available database tables
# Called by: none
# ---------------------------------------------------------
list_db_tables <- function() {
  mydb <- open_connection()
  tables <- DBI::dbListTables(mydb)
  DBI::dbDisconnect(mydb)
  
  return(tables)
}
