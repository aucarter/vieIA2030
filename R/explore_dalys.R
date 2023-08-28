
library(tidyverse)

# ---------------------------------------------------------
# Calculate DALYs averted for GBD diseases
# ---------------------------------------------------------
explore_dalys = function() {
  
  # Load stuff up front
  load_tables("gbd_strata_deaths", "coverage", "wpp_input") 
  
  # Years to calculate DALYs for
  years = 2000 : 2019
  
  # Diseases for which we use GBD method for deaths averted
  gbd_diseases = disease_table[source == "gbd", disease]
  
  # We'll calculate DALYs for all viable countries
  countries = loc_table$location_iso3 # [57]
  
  # Iterate through GBT diseases 
  for (gbd_disease in gbd_diseases[4]) {
    
    message(" - DALYs averted: ", gbd_disease)
    
    # D-V-A combinations for disease of interest
    d_info = d_v_at_table %>%
      filter(disease == gbd_disease) %>%
      select(disease, vaccine, activity_type) %>%
      as.list()
    
    # Calculate scenario deaths (vax and no vax scenarios)
    x = get_deaths(d_info, countries, years)
    
    # Preallocate non-death disease states
    x = get_states(x, d_info, countries)
    
    # Parameters for computing DALYs for this disease
    p = get_params(d_info, years)
    
    # Function for computing DALYs for this disease
    daly_fn = paste1("dalys", tolower(d_info$disease))
    
    # Calculate DALYs for this disease for each country
    for (i in countries)
      x = get(daly_fn)(x, i, p)
    
    # Unpack and format results
    results = prep_results(x, d_info, countries, years)
    
    # Plot a series of results
    plot_dalys(d_info)
  }
}

# ---------------------------------------------------------
# Calculate DALYs for TB
# ---------------------------------------------------------
dalys_tb = function(x, i, p) {
  
  # Vaccine coverage for this country
  cov_dt = get_cov(p$d_info, i, p$years)
  
  # Population details for this country
  pop_dt = get_pop(i, p$years)
  
  # Deaths in this country over time
  n.0 = x$deaths.0[[i]]$value  # No vaccine scenario
  n.1 = x$deaths.1[[i]]$value  # Vaccine scenario
  
  # Skip trivial case: no impact factor for this country
  if (sum(n.0, na.rm = TRUE) == 0)
    return(x)
  
  # Preallocate for cases and DALYs
  c.0 = c.1 = d.0 = d.1 = rep(NA, length(p$years))
  
  # Iterate through years
  for (y in seq_along(p$years)) {
    
    # Calculate active cases...
    
    # Flag for full or simple calculation (depends on time step)
    use_last_ratio = y + p$dur >= length(p$years)
    
    # In all but the final years...
    if (!use_last_ratio) {
      
      # Vaccination coverage
      v = cov_dt[year == p$years[y], coverage]
      
      # Proportion of population under 5 years old
      u5 = pop_dt[year == p$years[y], prop_u5]
      
      # Probability of death per active TB case
      #
      # NOTE: tau and rho should be indexed with y
      mu_y = (1-p$tau)*p$mu$u + p$tau*((1-p$rho)*p$mu$t + p$rho*p$mu$r)
      
      # Reduction in death from vaccination
      nu_y = (1-v) + v*(1-p$eps1) * (u5*(1-p$eps2) + (1-u5))
      
      # Caculate cases required to cause n deaths
      c.0[y] = n.0[y+p$dur] / mu_y           # No vaccine scenario
      c.1[y] = (n.1[y+p$dur] / mu_y) * nu_y  # Vaccine scenario
    }
    
    # Assumptions needed for last few years
    if (use_last_ratio) {
      
      # Multiply current deaths by cases : death ratio from last year
      c.0[y] = n.0[y] * (c.0[y-1] / n.0[y-1])  # No vaccine scenario
      c.1[y] = n.1[y] * (c.1[y-1] / n.1[y-1])  # Vaccine scenario
    }
    
    # Calculate DALYs...
    
    # Multiply active cases by disability weight and sum past deaths
    d.0[y] = sum(n.0[1 : y]) + c.0[y] * p$omega  # No vaccine scenario
    d.1[y] = sum(n.1[1 : y]) + c.1[y] * p$omega  # Vaccine scenario
  }
  
  # Store estimated number of active cases
  x = append_result(x, i, p, c.0, "cases.0")
  x = append_result(x, i, p, c.1, "cases.1")
  
  # Store total DALYs occuring from deaths and cases
  x = append_result(x, i, p, d.0, "dalys.0")
  x = append_result(x, i, p, d.1, "dalys.1")
  
  return(x)
}

# ---------------------------------------------------------
# Shorthand for storing result as a datatable
# ---------------------------------------------------------
append_result = function(x, i, p, y, metric) {
  
  # Construct results to be stored within x
  result = data.table(country = i, 
                      year    = p$years, 
                      metric  = metric, 
                      value   = y)
  
  # Store within x list at appropriate location
  x[[metric]][[i]] = result
  
  return(x)
}

# ---------------------------------------------------------
# Preallocate disease states for each disease
# ---------------------------------------------------------
get_states = function(x, d_info, countries) {
  
  # Preallocate with list of size n_countries
  l = vector('list', length = length(countries))
  
  # Elements must be named for this preallocation to work
  names(l) = countries
  
  # Preallocate for DALY results for all diseases
  x$dalys.0 = l
  x$dalys.1 = l
  
  # ---- Tuberculosis ----
  
  # Check flag  
  if (d_info$disease == "TB") {
    
    # Preallocate for active TB cases
    x$cases.0 = l
    x$cases.1 = l
  }
  
  # ---- xxx ----
  
  
  
  return(x)
}

# ---------------------------------------------------------
# Disease specific parameters for calculating DALYs
# ---------------------------------------------------------
get_params = function(d_info, years) {
  
  # Initiate list of parameters
  p = list(d_info = d_info, 
           years  = years)
  
  # ---- Tuberculosis ----
  
  # Check flag  
  if (d_info$disease == "TB") {
    
    # Case fatality
    p$mu = list(
      u = 0.7,   # Untreated
      t = 0.03,  # Treated TB
      r = 0.52)  # Treated MDR-TB
    
    # Vaccine efficacy
    p$eps1 = 0.8  # Reduction in death
    p$eps2 = 0.37  # Reduction of TB in u5s
    
    # Duration before death or recovery (in years)
    p$dur = 3
    
    # Treatment rate
    p$tau = 0.9  # TODO: This should be country-specific
    
    # MDR-TB prevalence
    p$rho = 0.03  # TODO: This should be country-specific
    
    # Disability weight of TB case
    p$omega = 0.333
  }
  
  # ---- xxx ----
  
  
  
  return(p)
}

# ---------------------------------------------------------
# Dictionaries for human readable variables
# ---------------------------------------------------------
get_dict = function(d_info) {
  
  # All disease use same scenaio names
  dict = list(
    scenario = c(
      s0 = "No vaccine scenario", 
      s1 = "Vaccine scenario", 
      s2 = "Averted through vaccination"))
  
  # ---- Tuberculosis ----
  
  # Check flag  
  if (d_info$disease == "TB") {
    
    # Metric dictionary for TB
    dict$metric = c(
      deaths = "Deaths from TB", 
      cases  = "Active TB cases", 
      dalys  = "DALYs attributable to TB")
  }
  
  # ---- xxx ----
  
  
  
  return(dict)
}

# ---------------------------------------------------------
# Population by country and year
# ---------------------------------------------------------
get_pop = function(countries, years) {
  
  # Population under 5
  prop_u5_dt = wpp_input %>%
    filter(country %in% countries, 
           year    %in% years,
           age <= 5) %>%
    group_by(country, year) %>%
    summarise(u5 = sum(nx)) %>%
    ungroup() %>%
    as.data.table()
  
  # Total population and proportion under 5
  pop_dt = wpp_input %>%
    filter(country %in% countries, 
           year    %in% years) %>%
    group_by(country, year) %>%
    summarise(pop = sum(nx)) %>%
    ungroup() %>%
    left_join(y  = prop_u5_dt, 
              by = c("country", "year")) %>%
    mutate(prop_u5 = u5 / pop) %>%
    select(-u5) %>%
    as.data.table()
  
  return(pop_dt)
}

# ---------------------------------------------------------
# Vaccine coverage by country and year
# ---------------------------------------------------------
get_cov = function(d_info, countries, years) {
  
  # TODO: I guess we should be using total_coverage here
  
  # Vaccine coverage
  cov_dt = coverage %>%
    left_join(y  = v_at_table,
              by = "v_at_id") %>%
    # Reduce down to what we're interested in...
    filter(country %in% countries, 
           vaccine   == d_info$vaccine,
           activity_type == d_info$activity_type, 
           year %in% years) %>%
    # Calculate 'total' coverage...
    group_by(country, year) %>%
    mutate(pop = fvps / coverage) %>%
    summarise(fvps = sum(fvps), 
              pop  = sum(pop)) %>%
    ungroup() %>%
    mutate(coverage = fvps / pop) %>%
    select(country, year, coverage) %>%
    # total_coverage() %>%
    # Ensure all countires and years considered...
    full_join(y  = full_factorial(countries, years),
              by = c("country", "year")) %>%
    arrange(country, year) %>%
    # Interpolate missing values...
    group_by(country) %>%
    mutate(coverage = zoo::na.approx(coverage, na.rm = FALSE)) %>%
    fill(coverage, .direction = "downup") %>%
    ungroup() %>%
    as.data.table()
  
  
  return(cov_dt)
}

# ---------------------------------------------------------
# Deaths in vaccine sceanrio AND no vaccine scenario
# ---------------------------------------------------------
get_deaths = function(d_info, countries, years) {
  
  # Which method to use for calculating 'no-vaccine' deaths
  use_method = "2"  # OPTIONS: "1" or "2"
  
  # ---- Real life 'vaccine' scenario ----
  
  # Deaths estimated by Global Burden of Disease study
  deaths_observed = gbd_strata_deaths %>%
    # Append country 'names'...
    left_join(y  = loc_table[, .(location_id, country = location_iso3)],
              by = "location_id") %>%
    # Reduce down to what we're interested in...
    filter(country %in% countries, 
           disease == d_info$disease,
           vaccine == d_info$vaccine,
           activity_type == d_info$activity_type, 
           year %in% years) %>% 
    # Sum over age and gender...
    group_by(country, year) %>%
    summarise(deaths_observed = sum(value)) %>%
    ungroup() %>%
    # Esnure all countires and years considered...
    full_join(y  = full_factorial(countries, years),
              by = c("country", "year")) %>%
    arrange(country, year) %>%
    # Interpolate missing values...
    group_by(country) %>%
    mutate(deaths_observed = zoo::na.approx(deaths_observed, na.rm = FALSE)) %>%
    fill(deaths_observed, .direction = "downup") %>%
    ungroup() %>%
    as.data.table()
  
  # ---- Hypothetical 'no vaccine' scenario ----
  
  # Approach 1) Back calculate using vaccine parameters...
  
  # Vaccine coverage and efficacy
  vax_coverage = get_cov(d_info, countries, years)
  vax_efficacy = efficacy[disease == d_info$disease & 
                            vaccine == d_info$vaccine, mean]
  
  # Caculate deaths in no vaccine scenario...
  method_1_dt = deaths_observed %>%
    inner_join(y  = vax_coverage, 
               by = c("country", "year")) %>%
    mutate(deaths_novaccine = deaths_observed / (1 - coverage * vax_efficacy), 
           deaths_averted   = deaths_novaccine - deaths_observed, 
           method = "method 1") %>%
    select(-coverage)
  
  # Approach 2) Use previously computed impact factors...
  
  # Load and join deaths averted (impact factor method)
  method_2_dt = deaths_observed %>%
    left_join(y  = load_results_2019(d_info, countries), 
              by = c("country", "year")) %>%
    mutate(deaths_novaccine = deaths_observed + deaths_averted, 
           method = "method 2")
  
  # Select the result as per user's choice of use_method
  deaths_novaccine = get(paste1("method", use_method, "dt")) %>%
    select(country, year, deaths_novaccine)
  
  # ---- Compare death calculation methods ----
  
  # Bind outcomes from the two methods
  compare_dt = bind_rows(method_1_dt, method_2_dt) %>%
    pivot_longer(cols = -c(country, year, method), 
                 names_to = "metric") %>%
    arrange(metric, method, year) %>%
    as.data.table()
  
  # Plot all countries
  g1 = ggplot(compare_dt) +
    aes(x = year, y = value, colour = country) +
    geom_line(show.legend = FALSE) +
    facet_grid(method~metric)
  
  # Plot single country
  g2 = ggplot(compare_dt[country == countries[1]]) +
    aes(x = year, y = value, colour = metric, linetype = method) +
    geom_line()
  
  # ---- Prepare output ----
  
  # Output in list format
  deaths = list(
    
    # Deaths in 'no vaccine' scenario (aka deaths.0)
    deaths.0 = deaths_novaccine %>% 
      rename(value  = deaths_novaccine) %>% 
      mutate(metric = "deaths.0", .before = value) %>%
      split(.$country),
    
    # Deaths in 'vaccine' scenario (aka deaths.1)
    deaths.1 = deaths_observed %>% 
      rename(value = deaths_observed) %>% 
      mutate(metric = "deaths.1", .before = value) %>%
      split(.$country))
  
  # Sanity check: all countries should have n_years entries
  if (any(lapply(deaths$deaths.0, nrow) != length(years)))
    stop("We should have a value for each country for each year")
  
  return(deaths)
}

# ---------------------------------------------------------
# Full factorial datatable of countries and years
# ---------------------------------------------------------
full_factorial = function(countries, years) {
  
  # Full join to this to ensure all entries are considered
  full_factorial_dt = 
    expand_grid(
      country = countries, 
      year    = years) %>%
    arrange(country, year) %>%
    as.data.table()
  
  return(full_factorial_dt)
}

# ---------------------------------------------------------
# Deaths averted (using impact factors and FVPs)
# ---------------------------------------------------------
load_results_2019 = function(d_info, countries) {
  
  # Load impact factors from 2019 analysis
  results_2021 = readRDS(file.path(o$pth$main, "impact_factors_2019.rds"))
  
  # Multiply impact factors by number of FVPs 
  results_dt = results_2021 %>%
    # Append country 'names'...
    left_join(y  = loc_table[, .(location_id, country = location_iso3)], 
              by = "location_id") %>%
    # Reduce down to what we're interested in...
    filter(country %in% countries, 
           disease == d_info$disease,
           vaccine == d_info$vaccine,
           activity_type == d_info$activity_type) %>% 
    # Apply decent D-V-A names...
    left_join(y  = d_v_a_name(), 
              by = c("disease", "vaccine", "activity_type")) %>%
    select(country, d_v_a, impact_factor) %>%
    # Multiply through by the number vaccinated...
    left_join(y  = get_temporal_fvps(), 
              by = c("country", "d_v_a")) %>%
    mutate(deaths_averted = impact_factor * fvps) %>%
    select(country, year, deaths_averted)
  
  return(results_dt)
}

# ---------------------------------------------------------
# Unpack and format results
# ---------------------------------------------------------
prep_results = function(x, d_info, countries, years) {
  
  # Dictionaries for this disease
  dict = get_dict(d_info)
  
  # Population by country and year
  pop_dt = get_pop(countries, years)
  
  # Unpack results and calculate 'averted'
  results_dt = lapply(x, rbindlist) %>%
    rbindlist() %>%
    separate(metric, c("metric", "scenario")) %>%
    mutate(scenario = paste0("s", scenario)) %>%
    pivot_wider(names_from = scenario) %>%
    # Calculated DALYs averetd...
    mutate(s2 = s0 - s1) %>%
    pivot_longer(cols = c(s0, s1, s2), 
                 names_to = "scenario") %>%
    filter(!is.na(value)) %>%
    # Apply dictionaries for human readable names...
    mutate(scenario = recode(scenario, !!!dict$scenario),
           metric   = recode(metric,   !!!dict$metric), 
           scenario = factor(scenario, dict$scenario),
           metric   = factor(metric,   dict$metric)) %>%
    arrange(scenario, country, metric, year) %>%
    # Results per 100k people...
    left_join(y  = pop_dt[, .(country, year, pop)], 
              by = c("country", "year")) %>%
    filter(!is.na(pop)) %>%
    mutate(value_100k = 1e5 * value / pop) %>%
    select(-pop) %>%
    as.data.table()
  
  # Store this primary output in a list
  results = list(dt = results_dt)
  
  # Averted results only (all countries)
  results$averted = results_dt %>%
    filter(scenario == dict$scenario["s2"]) %>%
    select(-scenario) %>%
    mutate(metric = paste(metric, "averted"), 
           metric = fct_inorder(metric))
  
  # Averted results only (sum over countries)
  results$total = results$averted %>%
    group_by(year, metric) %>%
    summarise(mean_100k = mean(value_100k), 
              total     = sum(value)) %>%
    ungroup() %>%
    arrange(metric, year) %>%
    as.data.table()
  
  # Mean over countries (all metrics)
  results$mean = results_dt %>%
    group_by(year, metric, scenario) %>%
    summarise(mean_100k = mean(value_100k)) %>%
    ungroup() %>%
    arrange(scenario, metric, year) %>%
    as.data.table()
  
  # Save results to file
  save_name = paste0("dalys_", tolower(d_info$disease), ".rds")
  saveRDS(results, file = paste0(o$pth$testing, save_name))
}

# ---------------------------------------------------------
# Plot DALYs and DALYS averted for a given disease
# ---------------------------------------------------------
plot_dalys = function(d_info) {
  
  # Load DALY results for this disease
  results_file = paste0("dalys_", tolower(d_info$disease), ".rds")
  results = readRDS(paste0(o$pth$testing, results_file))
  
  # Select the first country - we'll use this for several example plots
  example_country = results$dt$country[1]
  
  # Dictionaries for this disease
  dict = get_dict(d_info)
  
  # Colours
  cols = list(
    metric   = colour_scheme("brewer::dark2",  n = length(dict$metric)), 
    scenario = colour_scheme("brewer::accent", n = length(dict$scenario))) 
  
  # Total DALYs averted across countries
  g1 = (ggplot(results$total) +
          aes(x = year, y = total, colour = metric) +
          geom_line(size = 2, show.legend = FALSE) + 
          facet_wrap(~metric, scales = "free_y")) %>%
    prettify4(cols = cols$metric, 
              save = c("DALY", "averted total"))
  
  # DALYs averted for an example country
  g2 = (ggplot(results$dt[country == example_country]) +
          aes(x = year, y = value, colour = scenario) +
          geom_line(size = 2) +
          facet_wrap(~metric)) %>%
    prettify4(cols = cols$scenario, 
              save = c("DALY", "scenario", example_country))
  
  # DALYs averted per 100k population
  g3 = (ggplot(results$averted, aes(x = year)) +
          geom_line(mapping = aes(y = value_100k, group = country),
                    colour  = "grey60", 
                    alpha   = 0.5) +
          geom_line(data    = results$total, 
                    mapping = aes(y = mean_100k), 
                    colour  = "dodgerblue1", 
                    size    = 1.5) +
          facet_wrap(~metric, scales = "free_y")) %>%
    prettify4(save = c("DALY", "averted per 100k"))
  
  # All metrics per 100k people
  g4 = (ggplot(results$dt, aes(x = year)) +
          geom_line(mapping = aes(y = value_100k, group = country),
                    colour  = "grey60", 
                    alpha   = 0.5) +
          geom_line(data    = results$mean, 
                    mapping = aes(y = mean_100k), 
                    colour  = "dodgerblue1", 
                    size    = 1.5) +
          facet_grid(metric~scenario, scales = "free_y")) %>%
    prettify4(save = c("DALY", "scenario per 100k"))
}

# ---------------------------------------------------------
# Prettify plots
# ---------------------------------------------------------
prettify4 = function(g, cols = NULL, save = NULL) {
  
  # Apply colours if provided
  if (!is.null(cols))
    g = g + scale_colour_manual(values = cols)
  
  # Prettyify axes
  g = g + 
    scale_x_continuous(
      name   = "Year",
      expand = c(0, 0)) +
    scale_y_continuous(
      name   = "Annual value", 
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05)),
      labels = comma)
  
  # Prettify theme
  g = g + theme_classic() + 
    theme(strip.text    = element_text(size = 18),
          axis.title    = element_text(size = 20),
          axis.text     = element_text(size = 10),
          axis.line     = element_blank(),
          panel.border  = element_rect(size = 1, colour = "black", fill = NA),
          panel.spacing = unit(0.5, "lines"),
          strip.background = element_blank(), 
          legend.title  = element_text(size = 14),
          legend.text   = element_text(size = 12),
          legend.key    = element_blank(),
          legend.key.height = unit(2, "lines"),
          legend.key.width  = unit(2, "lines")) 
  
  # Save plots to file
  if (!is.null(save))
    fig_save(g, dir = "testing", save)
  
  return(g)
}

