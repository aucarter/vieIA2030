
library(tidyverse)

# ---------------------------------------------------------
# xxxxxx
# ---------------------------------------------------------
explore_dalys = function() {
  
  # Example case to explore
  example = list(
    country = "ETH",
    d_v_a   = "TB, routine",
    disease = "TB", 
    vaccine = "BCG",
    activity_type = "routine")
  
  # Load stuff up front
  load_tables("coverage",  # Used in get_temporal_fvps
              "gbd_strata_deaths") 
  
  years = 2000 : 2019
  
  # ---- Params ----
  
  # Duration before death or recovery (in years)
  dur = 3
  
  # Case fatality
  mu = list(
    u = 0.7,   # Untreated
    t = 0.03,  # Treated TB
    r = 0.52)  # Treated MDR-TB
  
  # Vaccination coverage
  v = 0.8 
  
  # Vaccine efficacy
  eps_1 = 0.8  # Reduction in death
  eps_2 = 0.37  # Reduction of TB in u5s
  
  # Treatment rate
  tau = 0.9
  
  # MDR-TB prevalence
  rho = 0.03
  
  # Disability weight of TB case
  omega = 0.333
  
  # ---- Load deaths ----
  
  deaths_observed = gbd_strata_deaths %>%
    # Apply decent D-V-A names...
    left_join(y  = d_v_a_name(),  # NOTE: D, T, P have different names now
              by = c("disease", "vaccine", "activity_type")) %>%
    # Append country 'names'...
    left_join(y  = loc_table[, .(location_id, country = location_iso3)],
              by = "location_id") %>%
    # Sum over age and gender...
    group_by(country, d_v_a, year) %>%
    summarise(deaths_observed = sum(value)) %>%
    ungroup() %>%
    as.data.table() %>%
    # TEMP: Look at a single example...
    filter(country == example$country,
           d_v_a   == example$d_v_a) %>%
    select(-country, -d_v_a) %>%
    # Interpolate missing values...
    full_join(y  = data.table(year = years), 
              by = "year") %>%
    arrange(year) %>%
    mutate(deaths_observed = zoo::na.approx(deaths_observed))
  
  vaccine_coverage = coverage %>%
    left_join(y  = v_at_table,
              by = "v_at_id") %>%
    # TEMP: Look at a single example...
    filter(country       == example$country, 
           vaccine       == example$vaccine,
           activity_type == example$activity_type, 
           year %in% years) %>%
    # Calculate 'total' coverage...
    # total_coverage() %>%
    select(year, coverage) %>%
    arrange(year)
  
  # Vaccine efficacy (from table of vaccine efficacy for GBD diseases)
  vaccine_efficacy = efficacy %>%
    filter(disease == example$disease, 
           vaccine == example$vaccine) %>%
    pull(mean)
  
  # Join everything together
  vaccine_dt = deaths_observed %>%
    inner_join(y  = vaccine_coverage, 
               by = c("year")) %>%
    arrange(year) %>%
    filter(coverage > 0) %>%
    mutate(efficacy = vaccine_efficacy)
  
  # We're now ready to caculate deaths in no vaccine scenario...
  method_1_dt = vaccine_dt %>%
    mutate(deaths_novaccine = deaths_observed / (1 - coverage * efficacy), 
           deaths_averted   = deaths_novaccine - deaths_observed, 
           method = "method 1") %>%
    select(-coverage, -efficacy)
  
  # Compare with alternative method...
  method_2_dt = method_1_dt %>%
    select(year, deaths_observed) %>%
    left_join(y  = load_results_2019(example), 
              by = "year") %>%
    mutate(deaths_novaccine = deaths_observed + deaths_averted, 
           method = "method 2") %>%
    select(all_of(names(method_1_dt)))
  
  plot_dt = rbind(method_1_dt, method_2_dt) %>%
    pivot_longer(cols = -c(year, method), 
                 names_to = "metric") %>%
    arrange(metric, method, year) %>%
    as.data.table()
  
  g = ggplot(plot_dt) +
    aes(x = year, y = value, colour = metric, linetype = method) +
    geom_line()
  
  # Deaths in no vaccine (deaths0) and vaccine (deaths1) scenarios
  deaths0 = method_2_dt[year %in% years, deaths_novaccine]
  deaths1 = method_2_dt[year %in% years, deaths_observed]
  
  # ---- Calculate cases and DALYs ----
  
  # Preallocate
  cases0 = rep(NA, length(years))
  cases1 = rep(NA, length(years))
  dalys0 = rep(NA, length(years))
  dalys1 = rep(NA, length(years))
  
  # Iterate through years
  for (i in seq_along(years)) {
    
    prop_u5 = 0.8  # TODO
    
    # Calculate cases...
    
    # Probability of death per active TB case
    #
    # NOTE: tau and rho should be indexed with i
    mu_y = (1-tau)*mu$u + tau*((1-rho)*mu$t + rho*mu$r)
    
    # Reduction 
    nu_y = (1-v) + v*(1-eps_1) * (prop_u5*(1-eps_2) + (1-prop_u5))
      
    if (i+dur <= length(years)) {
      
      cases0[i] = deaths0[i+dur] / mu_y           # No vaccine scenario
      cases1[i] = (deaths1[i+dur] / mu_y) * nu_y  # Vaccine scenario
      
    } else {
      
      # Multiply current deaths by cases : death ratio from last year
      cases0[i] = deaths0[i] * (cases0[i-1] / deaths0[i-1])  # No vaccine scenario
      cases1[i] = deaths1[i] * (cases1[i-1] / deaths1[i-1])  # Vaccine scenario
    }
    
    # Calculate DALYs...
    
    dalys0[i] = sum(deaths0[1 : i]) + cases0[i] * omega  # No vaccine scenario
    dalys1[i] = sum(deaths1[1 : i]) + cases1[i] * omega  # Vaccine scenario
  }
  
  averted_dt = rbind(
    data.table(year   = years, 
               deaths = deaths0, 
               cases  = cases0, 
               dalys  = dalys0, 
               scenario = "no_vaccine"), 
    data.table(year   = years, 
               deaths = deaths1, 
               cases  = cases1, 
               dalys  = dalys1, 
               scenario = "vaccine"), 
    data.table(year   = years, 
               deaths = deaths0 - deaths1, 
               cases  = cases0  - cases1, 
               dalys  = dalys0  - dalys1, 
               scenario = "averted")) %>%
    pivot_longer(cols = -c(year, scenario), 
                 names_to = "metric") %>%
    arrange(scenario, metric, year) %>%
    mutate(metric = factor(metric, c("deaths", "cases", "dalys"))) %>%
    arrange(metric, year) %>%
    as.data.table()
    
  # ---- Plot ----
  
  g = ggplot(averted_dt) +
    aes(x = year, y = value, colour = scenario) +
    geom_line() +
    facet_wrap(~metric, scales = "free_y")
  
  browser()

}

# ---------------------------------------------------------
# xxxxxx
# ---------------------------------------------------------
load_results_2019 = function(example) {
  
  # Load impact factors from 2019 analysis
  results_2021 = readRDS(file.path(o$pth$main, "impact_factors_2019.rds"))
  
  # Format impact factors
  impact_factors_dt = results_2021 %>%
    # Apply decent D-V-A names...
    left_join(y  = d_v_a_name(), 
              by = c("disease", "vaccine", "activity_type")) %>%
    # Append country 'names'...
    left_join(y  = loc_table[, .(location_id, country = location_iso3)], 
              by = "location_id") %>%
    # Take only what we need...
    select(country, d_v_a, impact_factor) %>%   
    # TEMP: Look at a single example...
    filter(country == example$country, 
           d_v_a   == example$d_v_a) 
  
  # Load FVPs over time
  fvps_dt = get_temporal_fvps() %>%
    # TEMP: Look at a single example...
    filter(country == example$country, 
           d_v_a   == example$d_v_a) 
  
  # Calculate impact using impact factors
  results_dt = fvps_dt %>%
    left_join(y  = impact_factors_dt, 
              by = c("country", "d_v_a")) %>%
    mutate(deaths_averted = impact_factor * fvps) %>%
    select(country, d_v_a, year, deaths_averted)
  
  return(results_dt)
}

