
library(tidyverse)

# ---------------------------------------------------------
# xxxxxx
# ---------------------------------------------------------
explore_dalys = function() {
  
  # Example case to explore
  example = list(
    d_v_a   = "TB, routine",
    country = "ETH")
  
  # Load stuff up front
  load_tables("coverage",  # Used in get_temporal_fvps
              "gbd_strata_deaths") 
  
  years = 2000 : 2021
  
  # ---- Params ----
  
  # Case fatality
  mu = list(
    u = 0.7,   # Untreated
    t = 0.03,  # Treated TB
    r = 0.52)  # Treated MDR-TB
  
  # Treatment rate
  tau = 0.9
  
  # MDR-TB prevalence
  rho = 0.03
  
  # Disability weight of TB case
  omega = 0.333
  
  # ---- Preallocate ----
  
  cases = rep(NA, length(years))
  dalys = rep(NA, length(years))
  
  # ---- Load deaths ----
  
  deaths_dt = load_results_2019(example)
  
  deaths = deaths_dt[year %in% years, deaths_averted]
  
  # gbd_deaths = gbd_strata_deaths %>%
  #   # Apply decent D-V-A names...
  #   left_join(y  = d_v_a_name(),  # NOTE: D, T, P have different names now
  #             by = c("disease", "vaccine", "activity_type")) %>%
  #   # Append country 'names'...
  #   left_join(y  = loc_table[, .(location_id, country = location_iso3)], 
  #             by = "location_id") %>%
  #   # TEMP: Look at a single example...
  #   filter(country == example$country, 
  #          d_v_a   == example$d_v_a) 
    
  
  # ---- Calculate ----
  
  for (i in seq_along(years)) {
    
    # Calculate cases...
    
    if (i+2 <= length(years)) {
      
      # NOTE: tau and rho should bne indexed with i
      cases[i] = deaths[i+2] / ((1-tau)*mu$u + tau*(mu$t*(1-rho) + mu$r*rho))
      
    } else {
      
      # Multiply current deaths by cases : death ratio from last year
      cases[i] = deaths[i] * (cases[i-1] / deaths[i-1])
    }
    
    # Calculate DALYs...
    
    dalys[i] = sum(deaths[1 : i]) + cases[i] * omega
  }
  
  # ---- Plot ----
  
  browser()
  
  plot_dt = data.table(year = years, deaths, cases, dalys) %>%
    pivot_longer(cols = -year, 
                 names_to = "metric") %>%
    mutate(metric = factor(metric, c("deaths", "cases", "dalys"))) %>%
    arrange(metric, year) %>%
    as.data.table()
  
  g = ggplot(plot_dt) +
    aes(x = year, y = value, colour = metric) +
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

