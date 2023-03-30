###########################################################
# RELATIVE RISK
#
# xxxxxxx
#
###########################################################

# ---------------------------------------------------------
# Parent function to impute relative risk for all disease-vaccine combinations
# Called by: main.R (and other launch-style scripts)
# ---------------------------------------------------------
impute_all_rr <- function(params, routine_only = TRUE) {
  
  # IDs of stratas to impute
  if (routine_only) {
    s_list <- d_v_at_table[activity_type == "routine"]$d_v_at_id
  } else {
    s_list <- as.integer(names(params))
  }
  
  # Impute all stratas and bind into single datatable
  pred_all <- rbindlist(
    lapply(
      s_list,
      impute_strata_rr,
      params
    ),
    fill = T
  )
  
  browser()
  
  return(pred_all)
}

# ---------------------------------------------------------
# Impute relative risk for a given disease-vaccine combination (aka strata)
# Called by: impute_all_rr()
# ---------------------------------------------------------
impute_strata_rr <- function(strata, params) {
  
  message("Imputing relative risk in strata: ", strata)
  
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
  
  # Is this disease reported by VIMC?
  vimc <- disease_table[disease == d]$vimc
  
  # ---- Load data ----
  
  browser()
  
  if (vimc) {
    load_tables(c("vimc_impact", "all_deaths", "coverage"))  # See yov_model.R
    dt <- copy(vimc_impact)
    setnames(dt, "deaths_averted", "strata_deaths_averted")
    dt[, c("sex_id", "strata_deaths") := .(3, NA)]
    # Calculate both-sexes deaths
    deaths <- copy(all_deaths)[, .(deaths_obs = sum(deaths)),
                               by = .(age, year, location_id)]
    deaths[, sex_id := 3]
  } else {
    load_tables(c("gbd_strata_deaths", "all_deaths", "coverage"))  # See yov_model.R
    dt <- copy(gbd_strata_deaths)
    setnames(dt, "value", "strata_deaths")
    dt[, strata_deaths_averted := NA]
    deaths <- copy(all_deaths)
    setnames(deaths, "deaths", "deaths_obs")
  }
  
  browser()
  
  # All-cause deaths
  dt <- merge(
    dt[d_v_at_id == strata],
    deaths,
    by = c("age", "year", "location_id", "sex_id"),
    all =  T
  )
  dt[, d_v_at_id := strata]
  
  # ---- Coverage ----
  
  browser()
  
  tot_cov <- total_coverage(coverage[v_at_id == v_at & year %in% 2000:2030])  # See total_coverage.R & impact_factors.R
  #TODO: This collapsing of sex should go away
  cov_dt <- tot_cov[, .(coverage = mean(value)),
                    by = .(location_id, year, age)]
  cov_dt[, sex_id := 3]
  
  if (!vimc) {
    cov_dt <- rbindlist(lapply(1:2, function(s) {
      copy(cov_dt)[, sex_id := s]
    }))
  }
  dt <- merge(dt, cov_dt, by = c("location_id", "year", "age", "sex_id"),
              all = T
  )
  # Efficacy
  dt[, efficacy := ifelse(vimc, NA, efficacy[vaccine == v]$mean)]
  
  # ---- Calcualte relative risk ----
  
  if (vimc) {
    dt <- vimc_rr(dt, strata_params$alpha)
  } else {
    dt <- gbd_rr(dt, strata_params$alpha, strata_params$beta)
    
  }
  
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
  
  browser()
  
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
  load_tables("wpp_input")  # See yov_model.R
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

