###########################################################
# OPTIONS
#
# Set key options for all things model related.
#
# Any numerical, logical, or character value or vector defined
# in the main set_options() function can be overridden through
# the non-mandatory \config\my_options.yaml file. Note that
# this my_options file is git ignored (indeed, that is the very
# point of such a file), so each user will need to create one.
#
###########################################################

# ---------------------------------------------------------
# Set model options and assumptions
# Called by: launch.R
# ---------------------------------------------------------
set_options = function(do_step = NA, quiet = FALSE) {
  
  if (!quiet) message("* Setting options")
  
  # Reset R's most annoying default options
  options(stringsAsFactors = FALSE, scipen = 999, dplyr.summarise.inform = FALSE)
  
  # Initiate options list
  o = list(do_step = do_step)
  
  # Detect user - required for checking cluster jobs
  # o$user = Sys.info()[["user"]]
  
  # ---- Analysis settings ----
  
  # Name of analysis to run
  o$analysis_name = "v01"
  
  # Create output directory system
  o = set_dirs(o)  # See directories.R
  
  # ---- Time settings ----
  
  # Years to analyse
  o$analysis_years = 2000 : 2030  # Vaccine deployed across these dates
  o$future_years   = 2021 : 2030  # Cohort years
  
  # Year and age ranges stored in coverage database
  o$data_years = 2000 : 2039  # Vaccine effect calculated across these dates
  o$data_ages  = 0 : 95
  
  # ---- Data references ----
  
  # ECDC data links
  o$ecdc_api = 
    list(cases = "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv", 
         hosp  = "https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv")
  
  # ETH effective reproduction number estimates
  o$eth_api = "https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/<country>-estimates.csv"
  
  # API endpoint for national-level Oxford Stringency Index data
  o$osi_api = "https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/"
  
  # Data dictionary: ECDC hospital & ICU indicators
  o$data_dict$ecdc = c(hospital_beds = "Daily hospital occupancy", 
                       icu_beds      = "Daily ICU occupancy",
                       hospital_admissions = "Weekly new hospital admissions per 100k", 
                       icu_admissions      = "Weekly new ICU admissions per 100k")
  
  # ---- Data cache ----
  
  # Force a fresh database pull even if cache loading available
  o$force_db_pull = FALSE
  
  # Only load from cache if pulled within the last n hours
  o$cache_hour_limit = 168 * 52  # 168 hours = 1 week
  
  # ---- Uncertainty settings ----
  
  # Flag for reproducible uncertainty draws (consistent randomly sampled seeds)
  o$uncertainty_reproducible = TRUE  # TODO: Implement this
  
  # Number of draws to sample
  o$n_draws = 200

  # Statistical summary to use for 'best estimate' projection
  #
  # OPTIONS:
  #  "median" := Median of uncertainty simulations (stochastic and parameter uncertainty)
  #    "mean" := Mean of uncertainty simulations (stochastic and parameter uncertainty)
  # o$best_estimate_simulation = "mean"
  
  # Quantiles for credibility intervals
  # o$quantiles = c(0.025, 0.975)
  
  # ---- Results flags ----
  
  # Turn results generation on or off
  o$results_markdown = TRUE  # Full markdown results document
  o$results_upload   = FALSE  # Upload reference results to database
  
  # ---- Plotting flags ----
  
  # Turn figures on or off
  # o$plot_baseline    = TRUE  # Standard baseline figures
  # o$plot_cumulative  = TRUE  # Plot cumulative outcomes
  # o$plot_scenarios   = TRUE  # Plot alternative (non-array) scenarios
  # o$plot_arrays      = TRUE  # Plot grid array scenario bundles
  # o$plot_heatmaps    = TRUE  # Plot heat maps for multidimension grid arrays
  # o$plot_endpoints   = TRUE  # Plot array LHC endpoints across different parameters
  # o$plot_assumptions = TRUE  # Model structure and assumptions figures
  # o$plot_calibration = TRUE  # Calibration performance and diagnostics
  
  # ---- Plotting settings ----
  
  # # Lower bound of age groups for plotting - bounded above by maximum age
  # o$plot_ages = c(0, 18, 60)  # Captures 3 age groups as per ECDC request
  
  # Colour packages and palettes (see colour_scheme in auxiliary.R)
  o$palette_disease = "pals::kovesi.rainbow"  # ~15 values
  o$palette_region  = "brewer::paired"  # 6 values
  o$palette_income  = "brewer::dark2"  # 4 values
  o$palette_gavi    = "brewer::greys"  # 2 values (yes or no)
  
  # # Define some nice properties for baseline metric plots
  # o$baseline_name   = "Baseline scenario"
  # o$baseline_colour = "grey50"  # Light grey
  # 
  # # Grey colour for current date dashed line
  # o$data_colour = "#555555"  # Dark grey
  # o$dash_colour = "#808080"  # Even darker grey
  
  # Font sizes: title, axis, tick, strip, legend, key
  o$font_size = c(34, 28, 16, 24, 20, 18)
  
  # Saved figure size
  o$save_width  = 14
  o$save_height = 10
  
  # Units of figures sizes
  o$save_units = "in"
  
  # Plotting resolution (in dpi)
  o$save_resolution = 300
  
  # Image format for saving figure
  # 
  # NOTE: Use a character vector to save with multiple formats at once
  o$figure_format = "png" # Classic options: "png", "pdf", or "svg"
  
  # ---- Prepare output ----
  
  # Append helpful properties
  o = append_shortcuts(o)
  
  # Override options set in my_options file
  # o = override_options(o, quiet = quiet)
  
  # Display analysis details
  if (!quiet) message(" - Analysis name: ", o$analysis_name)
  
  return(o)
}

# ---------------------------------------------------------
# Override options set in my_options file
# Called by: set_options()
# ---------------------------------------------------------
override_options = function(o, quiet = FALSE) {
  
  # Throw a warning if user still has a my_options.csv file
  if (file.exists(str_replace(o$pth$my_options, ".yaml$", ".csv")))
    warning("my_options.csv has been deprecated: use my_options.yaml instead")
  
  # If user has a 'my options' file, load it
  if (file.exists(o$pth$my_options)) {
    my_options = read_yaml(o$pth$my_options)
    
    # Continue if we have options to override
    if (length(my_options) > 0) {
      
      if (!quiet) message(" - Overriding options using config/my_options.yaml file")
      
      # Throw an error if there are entries that are not well defined options
      unrecognised = names(my_options)[!names(my_options) %in% names(o)]
      if (length(unrecognised) > 0)
        stop("Unrecognised entries in 'my options' file: ", paste(unrecognised, collapse = ", "))
      
      # Throw an error if there are multiple entries for any one option
      duplicates = names(my_options)[duplicated(names(my_options))]
      if (length(duplicates) > 0)
        stop("Duplicate entries in 'my options' file: ", paste(duplicates, collapse = ", "))
      
      # Variable class of the options we wish to overwrite
      class_conversion = paste0("as.", lapply(o[names(my_options)], class))
      
      # Iterate through the options and overwrite with value of correct class
      for (i in seq_along(my_options))
        o[[names(my_options)[i]]] = get(class_conversion[i])(my_options[[i]])
    }
  }
  
  return(o)
}

# ---------------------------------------------------------
# Override options set in my_options file
# Called by: set_options()
# ---------------------------------------------------------
append_shortcuts = function(o) {

  # Disease source (VIMC or GBD)
  for (i in unique(disease_table$source))
    o$disease[[i]] = disease_table[source == i, disease]

  return(o)
}

