
library(tidyverse)

# ---------------------------------------------------------
# Parent function for comparing linear to non-linear approach
# ---------------------------------------------------------
compare_nonlinear = function() {
  
  # ---- Load coverage ----
  
  # Load stuff up front
  load_tables("wpp_input", "coverage")
  
  # Example case to explore
  case = list(
    # d_v_a   = "PCV, routine", 
    country = "ETH")
  
  # Load population size of each country over time
  pop_dt = wpp_input %>%
    group_by(country, year) %>%
    summarise(pop = sum(nx)) %>%
    ungroup() %>%
    as.data.table()
  
  # Load FVPs over time
  fvps_dt = get_temporal_fvps() %>%
    # Cumulative sum of FVPs...
    group_by(country, d_v_a) %>%
    mutate(fvps_cum = cumsum(fvps)) %>%
    ungroup() %>%
    # ... relative to 100k people...
    left_join(y  = pop_dt, 
              by = c("country", "year")) %>%
    mutate(fvps_rel = o$per_person * fvps_cum / pop) %>%
    select(-pop) %>%
    as.data.table() %>%
    # TEMP: Look at a single example...
    filter(country == case$country) 
  # d_v_a   == case$d_v_a)
  
  # ---- Method 1: impact factors ----
  
  # NOTE: Here we are using impact factors as per Austin's first iteration
  
  results_2021a = readRDS(file.path(o$pth$main, "impact_factors_2019.rds"))
  # results_2021b = try_load(o$pth$impact_factors, "impact_dt")
  
  # Load impact factors calculated in step 2
  m1_dt = results_2021a %>%
    # Apply decent D-V-A names...
    left_join(y  = d_v_a_name(), 
              by = c("disease", "vaccine", "activity_type")) %>%
    # Append country 'names'...
    left_join(y  = loc_table[, .(location_id, country = location_iso3)], 
              by = "location_id") %>%
    # Take only what we need
    select(country, d_v_a, impact_factor)
  
  # # Load impact factors calculated in step 2
  # m1b_dt = results_2021b %>%
  #   # Apply decent D-V-A names...
  #   left_join(y  = d_v_a_name(), 
  #             by = c("disease", "vaccine", "activity_type")) %>%
  #   # Append country 'names'...
  #   left_join(y  = loc_table[, .(location_id, country = location_iso3)], 
  #             by = "location_id") %>%
  #   # Take only what we need
  #   select(country, d_v_a, v2 = impact_factor)
  # 
  # m1_dt %<>%
  #   inner_join(y  = m1b_dt, 
  #              by = c("country", "d_v_a"))
  # 
  # g = ggplot(m1_dt) + 
  #   aes(x = impact_factor, y = v2, colour = country) +
  #   geom_point(show.legend = FALSE) +
  #   geom_abline() + 
  #   facet_wrap(~d_v_a)
  
  # Calculate impact using impact factors
  m1_impact = fvps_dt %>%
    select(country, d_v_a, year, fvps, fvps_cum) %>%
    inner_join(y  = m1_dt, 
               by = c("country", "d_v_a")) %>%
    mutate(impact     = impact_factor * fvps, 
           impact_cum = impact_factor * fvps_cum) %>%
    # Tidy up...
    select(country, d_v_a, year, 
           fvps,   fvps_cum, 
           impact, impact_cum) %>%
    mutate(method = "Method 1")
  
  # ---- Method 2: model fitting ----
  
  # NOTE: Here we are fitting to cumulative FVPs (could be non-linear)
  
  # Evaluate best fitting model at cumulative FVPs per population-person
  m2_dt = evaluate_best_model(
    country = case$country, 
    # d_v_a   = case$d_v_a, 
    x = fvps_dt)
  
  m2_impact = fvps_dt %>%
    inner_join(y  = m2_dt, 
               by = c("country", "d_v_a", "fvps_rel")) %>%
    # Population weight for population-level impact...
    left_join(y  = pop_dt, 
              by = c("country", "year")) %>%
    mutate(impact_cum = impact_rel * pop / o$per_person) %>%
    # Revere cumsum to derive annual impact...
    group_by(country, d_v_a) %>%
    mutate(impact = rev_cumsum(impact_cum)) %>%
    ungroup() %>%
    # Tidy up...
    select(country, d_v_a, year, 
           fvps,   fvps_cum, 
           impact, impact_cum) %>%
    mutate(method = "Method 2")
  
  # ---- Data ----
  
  impact_dt = rbind(m1_impact, m2_impact)
  
  # Also load the data - we'll plot fits against this
  vimc_dt = readRDS(paste0(o$pth$testing, "vimc_dt.rds"))
  
  data_dt = vimc_dt %>%
    filter(country %in% unique(impact_dt$country), 
           d_v_a   %in% unique(impact_dt$d_v_a))
  
  # ---- Compare impact ----
  
  # Is this the orimary figure? Cumulative impact over time?
  g = (ggplot(impact_dt) +
         aes(x = year, y = impact_cum, colour = method) +
         geom_line(size = 1.5) +
         geom_point(data = data_dt, colour = "black") +
         facet_wrap(~d_v_a, scales = "free_y")) %>%
    prettify3(save = c("Compare", "year v impact cumulative"))
  
  g1 = (ggplot(impact_dt) +
          aes(x = year, y = impact, colour = method) +
          geom_line(size = 1.5) +
          geom_point(data = data_dt, colour = "black") +
          facet_wrap(~d_v_a, scales = "free_y")) %>%
    prettify3(save = c("Compare", "year v impact"))
  
  g2 = (ggplot(impact_dt) +
          aes(x = fvps, y = impact, colour = method) +
          geom_line(size = 1.5) +
          geom_point(data = data_dt, colour = "black") +
          facet_wrap(~d_v_a, scales = "free")) %>%
    prettify3(save = c("Compare", "FVP v impact"))
  
  g3 = (ggplot(impact_dt) +
          aes(x = fvps_cum, y = impact, colour = method) +
          geom_line(size = 1.5) +
          geom_point(data = data_dt, colour = "black") +
          facet_wrap(~d_v_a, scales = "free")) %>%
    prettify3(save = c("Compare", "FVP cumulative v impact"))
  
  g4 = (ggplot(impact_dt) +
          aes(x = fvps_cum, y = impact_cum, colour = method) +
          geom_line(size = 1.5) +
          geom_point(data = data_dt, colour = "black") +
          facet_wrap(~d_v_a, scales = "free")) %>%
    prettify3(save = c("Compare", "FVP cumulative v impact cumulative"))
}

# ---------------------------------------------------------
# Load coverage data and extract FVPs over time
# ---------------------------------------------------------
get_temporal_fvps = function() {
  
  # Load and format FVPs over time
  fvps_dt = coverage %>%
    # Summarise over age...
    group_by(country, v_at_id, year, sex_id) %>%
    summarise(fvps = sum(fvps)) %>%
    ungroup() %>%
    # Deal with gender groupings...
    mutate(sex_id = paste0("s", sex_id)) %>%
    pivot_wider(names_from  = sex_id, 
                values_from = fvps, 
                values_fill = 0) %>%
    mutate(fvps = ifelse(s1 == 0, s2 + s3, s1)) %>%
    select(country, v_at_id, year, fvps) %>%
    # Apply decent D-V-A names...
    left_join(y  = v_at_table, 
              by = "v_at_id") %>%
    left_join(y  = d_v_at_table, 
              by = c("vaccine", "activity_type")) %>%
    left_join(y  = d_v_a_name(), 
              by = c("disease", "vaccine", "activity_type")) %>%
    # Reduce down to what we're interested in...
    filter(year >= 2000) %>%
    select(country, d_v_a, year, fvps) %>%
    arrange(country, d_v_a, year) %>%
    as.data.table()
  
  return(fvps_dt)
}

# ---------------------------------------------------------
# Apply colour scheme and tidy up axes - impact plots
# ---------------------------------------------------------
prettify3 = function(g, save = NULL) {
  
  # Construct manual colour scheme
  cols = c("gold", "forestgreen")
  
  # Axes label dictionary
  lab_dict = c(
    year       = "Year",
    fvps       = "Fully vaccinated persons (FVPs) in one year",
    fvps_cum   = "Cumulative fully vaccinated persons (FVPs)",
    impact     = "Deaths averted (per FVP)",
    impact_cum = "Cumulative deaths averted (per FVP)")
  
  # Extract info from the plot
  g_info = ggplot_build(g)
  
  # Prettyify axes
  g = g + 
    scale_x_continuous(
      name   = lab_dict[g_info$plot$labels$x], 
      expand = expansion(mult = c(0, 0.05)),
      labels = comma) +
    scale_y_continuous(
      name   = lab_dict[g_info$plot$labels$y], 
      expand = expansion(mult = c(0, 0.05)),
      labels = comma)
  
  # Apply colours
  g = g + scale_colour_manual(values = cols)
  
  # Prettify theme
  g = g + theme_classic() + 
    theme(strip.text    = element_text(size = 10),
          axis.title    = element_text(size = 15),
          axis.text     = element_text(size = 7, angle = 30, hjust = 1),
          axis.line     = element_blank(),
          panel.border  = element_rect(size = 1, colour = "black", fill = NA),
          panel.spacing = unit(0.5, "lines"),
          strip.background = element_blank()) 
  
  # Save plots to file
  if (!is.null(save))
    fig_save(g, dir = "testing", save)
  
  return(g)
}

