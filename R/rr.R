###########################################################
# RELATIVE RISK
#
# xxxxxxx
#
###########################################################

# ---------------------------------------------------------
# Parent function to impute relative risk for all disease-vaccine combinations
# Called by: launch.R, main.R (and other launch-style scripts)
# ---------------------------------------------------------
impute_all_rr <- function(params, routine_only = TRUE) {
  
  message("* Imputing relative risk")
  
  # IDs of stratas to impute
  if (routine_only) {
    strata_ids <- d_v_at_table[activity_type == "routine"]$d_v_at_id
  } else {
    strata_ids <- as.integer(names(params))
  }
  
  
  
  strata_ids = strata_ids[1]
  
  
  # Initiate list
  pred_list = list()
  
  # Impute the relative risk of each strata
  for (strata in strata_ids)
    pred_list[[strata_id]] = impute_strata_rr(strata, params)
  
  browser()
  
  pred_dt = rbindlist(pred_list, fill = TRUE)
  
  return(pred_dt)
}

# ---------------------------------------------------------
# Impute relative risk for a given disease-vaccine combination (aka strata)
# Called by: impute_all_rr()
# ---------------------------------------------------------
impute_strata_rr <- function(strata, params) {
  
  message(" - Strata: ", strata)
  
  # Details of this strata to be imputed
  strata_params <- params[[as.character(strata)]]
  
  # Prepare for relative risk calculation
  dt <- prep_rr(strata, strata_params)
  
  browser()
  
  dt <- merge_rr_covariates(dt)
  
  browser()
  
  if(nrow(dt[rr < 1 & rr > 0]) == 0) {
    return(data.table())
  }
  fit <- glm(
    rr ~ haqi + sdi + year + mx +
      splines::bs(age, knots = strata_params$age_knots),
    data = dt[rr < 1 & rr > 0],
    family = "binomial"
  )
  dt[, pred := predict(fit, dt)]
  dt[, pred_rr := ifelse( # for floating point precision
    pred > 0,
    1 / (1 + exp(-pred)),
    exp(pred) / (exp(pred) + 1))]
  dt[, c("pred", "haqi", "sdi", "mx") := NULL]
  dt[, d_v_at_id := strata]
  
  dt[, averted := get_averted_deaths(
    deaths_obs, coverage, pred_rr, strata_params$alpha)]
  
  return(dt)
}

# ---------------------------------------------------------
# Prepare relative risk calculation for given strata
# Called by: impute_strata_rr()
# ---------------------------------------------------------
prep_rr <- function(strata, strata_params) {
  
  # ---- Extract strata details ----
  
  # Extract details about this disease, vaccine, and activity
  v  <- d_v_at_table[d_v_at_id == strata]$vaccine
  d  <- d_v_at_table[d_v_at_id == strata]$disease
  at <- d_v_at_table[d_v_at_id == strata]$activity_type
  
  # ID of this strata
  # @Austin - is v_at == strata always true?
  v_at <- v_at_table[vaccine == v & activity_type == at]$v_at_id
  
  # TEMP: Testing whether above line is needed
  if (v_at != strata)
    stop("Yep, previous line necessary")
  
  # Is this disease reported by VIMC? Otherwise GBD
  vimc <- disease_table[disease == d]$vimc == 1
  
  # ---- Load data ----
  
  # Load coverage and all cause death data
  load_tables("coverage", "all_deaths")  # See db_utils.R
  
  # TODO: Can we filter for strata here already?
  # TODO: Consistent order of columns and sorting (location_id, year, age, sex_id, ...)?
  
  # For VIMC diseases
  if (vimc) {
    
    # Load vaccine impact from VIMC
    load_tables("vimc_impact")  # See db_utils.R
    
    # Vaccine impact datatable - using VIMC estimates
    dt = copy(vimc_impact) %>%
      filter(d_v_at_id == strata) %>%  # Only this strata
      mutate(strata_deaths = NA, 
             sex_id        = 3) %>%  # ID of both genders combined
      select(location_id, year, age, sex_id, strata_deaths, 
             strata_deaths_averted = deaths_averted)
    
    # Sum deaths across each gender (datatable summarising much quicker)
    deaths = copy(all_deaths)[, .(deaths_obs = sum(deaths)), 
                              by = .(location_id, year, age)] %>%
      mutate(sex_id = 3, .before = deaths_obs)  # ID of both genders combined
  }
  
  # For GBD diseases
  if (!vimc) {
    
    # Load vaccine impact from GBD
    load_tables("gbd_strata_deaths")  # See db_utils.R
    
    browser()
    
    dt <- copy(gbd_strata_deaths)
    setnames(dt, "value", "strata_deaths")
    dt[, strata_deaths_averted := NA]
    
    # # Vaccine impact datatable - using VIMC estimates
    # dt = copy(gbd_strata_deaths) %>%
    #   rename(strata_deaths_averted = deaths_averted) %>%
    #   mutate(strata_deaths = NA, 
    #          sex_id        = 3)  # ID of both genders combined
    
    deaths <- copy(all_deaths)
    setnames(deaths, "deaths", "deaths_obs")
  }
  
  # All-cause deaths
  # dt2 <- merge(
  #   dt[d_v_at_id == strata],
  #   deaths,
  #   by = c("age", "year", "location_id", "sex_id"),
  #   all =  T
  # )
  # dt2[, d_v_at_id := strata]
  
  # Merge vaccine impact deaths with all cause observed deaths
  dt %<>% 
    right_join(deaths, by = c("location_id", "year", "age", "sex_id")) %>%
    mutate(d_v_at_id = strata, .after = location_id) %>%
    arrange(location_id, year, age)
  
  # ---- Total vaccine coverage by cohort ----
  
  # @Austin: I think total_coverage() should be handling the different activities. 
  # Meaning we shouldn't first filter by v_at_id. Thoughts?

  # xxxx
  # tot_cov = 
  x = coverage %>%
    filter(v_at_id == v_at, # TODO: Or just v_at_id == strata?
           year %in% o$analysis_years)
  
  tictoc::tic("v1")
  tot_cov = x %>% total_coverage()  # See total_coverage.R
  time_clock1 = tictoc::toc()
  
  tictoc::tic("v2")
  tot_cov2 = x %>% total_coverage2()  # See total_coverage.R
  time_clock2 = tictoc::toc()
  
  check_dt = tot_cov2 %>%
    mutate(activity_type = at) %>%
    rename(value2 = value) %>%
    left_join(tot_cov) %>%
    mutate(diff_value = abs(value - value2)) %>%
    filter(diff_value > 1e-8)
  
  if (nrow(check_dt) > 0)
    stop("Discrepancy between v1 and v2 functions")
  
  # Gender-related coverage issues...
  
  # @Austin: This feels like a big assumption, especially for vaccines like HPV, could we get
  # gender-specific outcomes from VIMC? Perhaps this is what you meant by your TODO comment?
  
  # TODO: This collapsing of sex should go away
  cov_dt <- tot_cov[, .(coverage = mean(value)),
                    by = .(location_id, year, age)]
  cov_dt[, sex_id := 3]
  
  if (!vimc) {
    cov_dt <- rbindlist(lapply(1:2, function(s) {
      copy(cov_dt)[, sex_id := s]
    }))
  }
  
  # Join coverage details to effect datatable
  dt <- merge(dt, cov_dt, by = c("location_id", "year", "age", "sex_id"),
              all = T)
  
  # ---- Efficacy ----
  
  # Efficacy: placeholder for VIMC, mean effect for non-VIMC vaccines
  dt[, efficacy := ifelse(vimc, NA, efficacy[vaccine == v]$mean)]
  
  # ---- Calcualte relative risk ----
  
  if (vimc) {
    dt <- vimc_rr(dt, strata_params$alpha)
  } else {
    dt <- gbd_rr(dt, strata_params$alpha, strata_params$beta)
    
  }
  
  browser()
  
  out_dt <- dt[, .(location_id, age, year, d_v_at_id, deaths_obs,
                   strata_deaths_averted, strata_deaths, coverage, rr)]
  
  browser()
  
  # Perform sanity checks on relative risk calculations
  check_rr(out_dt)
  
  return(out_dt)
}

# ---------------------------------------------------------
# Calculate relative risk for VIMC disease
# Called by: prep_rr()
# ---------------------------------------------------------
vimc_rr <- function(dt, alpha) {
  
  # Relative risk tends to 1 as coverage tends to 1, can be negative
  #
  # rr = (o - (a * (1 - c ^ alpha) / c ^ alpha)) / (o + a)
  #
  # where:
  #   o = deaths observed
  #   a = deaths averted from vaccine
  #   c = coverage
  
  out_dt <- copy(dt)
  out_dt[coverage > 0, rr := (deaths_obs - (strata_deaths_averted *
                                              (1 - coverage ^ alpha) / coverage ^ alpha)) /
           (deaths_obs + strata_deaths_averted)]
  
  return(out_dt[])
}

# ---------------------------------------------------------
# Calculate relative risk for GBD disease
# Called by: prep_rr()
# ---------------------------------------------------------
gbd_rr <- function(dt, alpha, beta) {
  
  browser()
  
  out_dt <- copy(dt)
  out_dt[coverage > 0, deaths_no := strata_deaths /
           (1 - beta * efficacy * coverage ^ alpha)]
  out_dt[coverage > 0, rr :=
           (deaths_obs - strata_deaths + (1 - beta * efficacy) * deaths_no) /
           (deaths_obs - strata_deaths + deaths_no)]
  out_dt[, deaths_no := NULL]
  
  return(out_dt[])
}

# ---------------------------------------------------------
# Perform sanity checks on relative risk calculations
# Called by: prep_rr()
# ---------------------------------------------------------
check_rr <- function(dt) {
  # Look for missing coverage where deaths averted are non-zero
  if (nrow(dt[coverage == 0 & strata_deaths_averted > 0]) > 0) {
    prop <- round(nrow(dt[coverage == 0 & strata_deaths_averted > 0]) /
                    nrow(dt[strata_deaths_averted > 0]) * 100, 2)
    warning("Missing coverage in ", prop,
            "% of location-age-years with deaths averted")
  }
  # Check for non-sensical numbers
  if (any(range(dt[!is.na(rr)]$rr) < 0 | range(dt[!is.na(rr)]$rr) > 1)) {
    prop <- round(nrow(dt[coverage > 0 & (rr < 0 | rr > 1)]) /
                    nrow(dt) * 100, 2)
    warning(paste0(
      "Over 1 or less than 0 mortality reduction in ", prop,
      "% of location-age-years"))
  }
}

# ---------------------------------------------------------
# xxxxx
# Called by: impute_strata_rr()
# ---------------------------------------------------------
merge_rr_covariates <- function(dt) {
  # Expand to all locations, years, and ages
  full_dt <- data.table(expand.grid(
    location_id = unique(loc_table$location_id),
    age = 0:95,
    year = 2000:2095
  ))
  dt <- merge(full_dt, dt, by = c("location_id", "age", "year"), all.x = T)
  # Add mortality
  load_tables("wpp_input")  # See db_utils.R
  mx_dt <- wpp_input[, .(mx = mean(mx)), by = .(location_id, year, age)]
  dt <- merge(dt, mx_dt, by = c("location_id", "age", "year"), all.x = T)
  # Add GBD covariates(SDI and HAQi)
  dt <- merge(dt, gbd_cov, by = c("location_id",  "year"), all.x = T)
  return(dt)
}

# ---------------------------------------------------------
# xxxxx
# Called by: impute_strata_rr()
# ---------------------------------------------------------
get_averted_deaths <- function(deaths_obs, coverage, rr, alpha) {
  averted_deaths <- deaths_obs * (
    coverage ^ alpha * (1 - rr) / (1 - coverage ^ alpha * (1 - rr))
  )
  
  return(averted_deaths)
}

