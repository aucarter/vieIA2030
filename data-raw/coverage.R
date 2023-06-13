###########################################################
# COVERAGE
#
# xxxxxxxxxxxx
#
###########################################################

# ---------------------------------------------------------
# xxx
# Called by: xxx
# ---------------------------------------------------------
prep_vimc_coverage_data <- function() {
  
  dt <- fread(
    system.file("extdata", "vimc_coverage.csv", package = "vieIA2030")
  )
  vimc_dt <- dt[, lapply(.SD, sum),
                by = .(country, disease, vaccine, activity_type, year, age, gender),
                .SDcols = c("fvps_adjusted", "cohort_size")]
  vimc_dt[, coverage := fvps_adjusted / cohort_size]
  vimc_dt[, c("disease") := NULL]
  vimc_dt <- merge(vimc_dt,
                   data.table(gender = c("Both", "Female", "Male"), sex_id = c(3, 2, 1)),
                   by = "gender")
  vimc_dt[, gender := NULL]
  setnames(vimc_dt, "fvps_adjusted", "fvps")
  
  return(vimc_dt[])
}

# ---------------------------------------------------------
# xxx
# Called by: xxx
# ---------------------------------------------------------
prep_wuenic_data <- function() {
  
  xls = "coverage_estimates_series.xls"
  sheets = readxl::excel_sheets(xls)
  
  load_sheet_fn = function(sheet) {
    
    sheet_dt = readxl::read_excel(path  = xls, 
                                  sheet = sheet) %>%
      select(-Region) %>%
      pivot_longer(cols = !c("ISO_code", "Cname", "Vaccine"),
                   names_to = "year") %>%
      mutate(year = as.integer(year))
    
    return(sheet_dt)
  }
  
  data_list = lapply(sheets[2:15], load_sheet_fn)
  
  data_dt = rbindlist(data_list, fill = T) %>%
    inner_join(y  = wuenic_vaccine_table, 
               by = c("Vaccine" = "wuenic_name")) %>%
    mutate(coverage = ifelse(is.na(value), 0, value / 100), 
           activity_type = "routine", 
           sex_id = 3, 
           age    = 0) %>%
    select(country = ISO_code, vaccine, activity_type, 
           sex_id, year, age, coverage)
  
  return(data_dt)
}

# ---------------------------------------------------------
# Extract coverage from WIISE for a given vaccine 
# ---------------------------------------------------------
extract_coverage = function(v_idx, data_dt, vimc_dt) {
  
  # Details of vaccine at index v_idx
  v = wuenic_vaccine_table[v_idx, ]
  
  # Some countries may already be covered for this vaccine by VIMC
  vimc_country = vimc_dt %>%
    filter(vaccine == v$vaccine) %>%
    pull(country) %>%
    unique()
  
  # Filter data by coverage_category type
  coverage_dt = data_dt %>%
    setnames(names(.), tolower(names(.))) %>% 
    rename(country   = code, 
           wuenic_id = antigen) %>%
    # Select only vaccine and countries of interest...
    filter(coverage_category == v$coverage_category, 
           wuenic_id %in% v$wuenic_id,
           country  %in% country_table$country, 
           !country %in% vimc_country) %>%  # Ignoring VIMC countries
    mutate(vaccine = v$vaccine) %>%
    # Format coverage...
    replace_na(list(coverage = 0)) %>%
    mutate(coverage = coverage/ 100) %>%
    # Append additional vaccination details...
    expand_grid(activity_type = v$activity, 
                sex_id = v$sex, 
                age    = eval_str(v$age)) %>%
    select(country, vaccine, activity_type, 
           sex_id, year, age, coverage) %>%
    arrange(country, vaccine, year, age) %>%
    setDT()
  
  return(coverage_dt)
}

# Load population data - used to convert coverage to FVPs
load_tables("wpp_input")

# Load VIMC coverage data
vimc_dt = prep_vimc_coverage_data()

# Non-VIMC coverage taken from WIISE database
data_url = "https://whowiise.blob.core.windows.net/upload/coverage--2021.xlsx"
data_dt  = read_url_xls(data_url, sheet = 1) 

# Extract coverage for each non-VIMC vaccine
non_vimc_dt = 1 : nrow(wuenic_vaccine_table) %>%
  lapply(extract_coverage, data_dt, vimc_dt) %>%
  rbindlist()

# Combine sources
coverage = wpp_input %>%
  # Calculate FVPs for non-VIMC diseases...
  group_by(country, year, age) %>%
  summarise(cohort_size = sum(nx)) %>%
  ungroup() %>%
  inner_join(y  = non_vimc_dt, 
             by = c("country", "year", "age")) %>%
  mutate(fvps = coverage * cohort_size) %>%
  # Combine VIMC diseases and format...
  bind_rows(vimc_dt) %>%
  filter(fvps > 0) %>%  # Remove trivial values
  left_join(y  = v_at_table, 
            by = c("vaccine", "activity_type")) %>%
  select(country, v_at_id, year, age, sex_id, fvps, coverage) %>%
  arrange(country, v_at_id, year, age, sex_id) %>%
  setDT()

# coverage[, age := as.integer(age)]
# coverage[, sex_id := as.integer(sex_id)]

# Upload table to database
upload_object(coverage, "coverage")

