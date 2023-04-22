###########################################################
# RESULTS
#
# xxxxxxx
#
###########################################################

# ---------------------------------------------------------
# xxxxxxxx
# Called by: launch.R, main.R (and other launch-style scripts)
# ---------------------------------------------------------
run_results = function() {
  
  # Only continue if specified by do_step
  if (!is.element(4, o$do_step)) return()
  
  message("* Producing results")
  
  # Load tables up front (see db_utils.R)
  load_tables("wpp_input")
  
  # Load impact factors calculated in step 2
  # impact_dt       = try_load(o$pth$impact_factors, "impact_dt")
  scenario_impact = try_load(o$pth$impact_factors, "scenario_impact")
  
  # ---- Reference results ----
  
  message(" - Saving results")
  
  browser() # Tidy up...
  
  ## Save for sharing
  out_dt <- merge(
    draws_dt,
    loc_table[, .(location_id, location_name, location_iso3, region, income_group, gavi73)],
    by = "location_id"
  )
  out_dt <- out_dt[order(location_name, disease, vaccine, activity_type)]
  out_dt[is.na(deaths_averted), deaths_averted := 0]
  
  cohort_dt <- wpp_input[age == 0, .(nx = sum(nx)), by = .(location_id, year)]
  setnames(cohort_dt, "nx", "cohort_size")
  out_dt <- merge(out_dt, cohort_dt, by = c("location_id", "year"))
  
  total_dt <- wpp_input[, .(nx = sum(nx)), by = .(location_id, year)]
  setnames(total_dt, "nx", "total_pop")
  out_dt <- merge(out_dt, total_dt, by = c("location_id", "year"))
  setcolorder(
    out_dt, 
    c(
      "location_name", "location_iso3", "location_id", "year", "disease", 
      "vaccine", "activity_type", "age", "impact_factor", "fvps", "deaths_averted",
      "region", "income_group", "gavi73", "cohort_size", "total_pop", paste0("draw_", 1:200)
    )
  )
  
  # Save results to file
  saveRDS(out_dt, file = paste0(o$pth$results, "reference_results.rds"))
  
  # Upload results if desired
  if (o$results_upload)
    upload_object(out_dt, "ia2030_reference_results")
  
  # ---- Impact by region ----
  
  no_lin_range_cov <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
                                       intro_year = 2025, intro_range = T)
  scen_dt <- merge(
    no_lin_range_cov,
    loc_table[, .(location_id, location_name, location_iso3)],
    by = "location_id"
  )
  scen_dt <- merge(scen_dt, v_at_table, by = "v_at_id")
  write.csv(scen_dt, "outputs/ia2030_cov_trajectories.csv", row.names = F)
  
  
  dt <- scenario_impact$dt
  dt <- merge(dt, loc_table[, .(location_id, region)], by = "location_id")
  baseline_dt <- dt[year == 2019]
  setnames(baseline_dt, "deaths_averted", "baseline_deaths_averted")
  baseline_dt[, year := NULL]
  table_dt <- merge(dt[year %in% 2021:2030], baseline_dt,
                    by = c("location_id", "region", "disease", "vaccine", "activity_type"),
                    all.x = T)
  table_dt[is.na(baseline_deaths_averted), baseline_deaths_averted := 0]
  table_dt[is.na(deaths_averted), deaths_averted := 0]
  table_dt[activity_type %in% c("routine", "combined"), 
           incremental := deaths_averted - baseline_deaths_averted]
  table_totals <- table_dt[, .(total = sum(deaths_averted, na.rm = T),
                               incremental = sum(incremental, na.rm = T)), by = .(region, disease, year)][
                                 order(region, disease, year)
                               ]
  
  # NOTE: Used to produce markdown document
  # write.csv(table_totals, "outputs/detailed_results.csv", row.names = F)
  saveRDS(table_totals, file = paste0(o$pth$results, "detailed_results.rds"))
  
  # ---- xxxxxx ----
  
  dt <- scenario_impact$dt
  dt <- merge(dt, loc_table[, .(location_id, income_group)], by = "location_id")
  global_dt <- dt[, .(deaths_averted = sum(deaths_averted, na.rm = T)), by = .(year, disease, vaccine, activity_type)]
  global_dt[, income_group := "Global"]
  income_group_dt <- dt[, .(deaths_averted = sum(deaths_averted, na.rm = T)), by = .(income_group, year, disease, vaccine, activity_type)]
  all_dt <- rbind(global_dt, income_group_dt)
  baseline_dt <- all_dt[year == 2019]
  setnames(baseline_dt, "deaths_averted", "baseline_deaths_averted")
  baseline_dt[, year := NULL]
  table_dt <- merge(all_dt[year %in% 2021:2030], baseline_dt,
                    by = c("income_group", "disease", "vaccine", "activity_type"), all.x = T)
  table_dt[, incremental := deaths_averted - baseline_deaths_averted]
  table_totals <- table_dt[, .(total = sum(deaths_averted, na.rm = T) / 1e5,
                               incremental = sum(incremental, na.rm = T) / 1e5), by = income_group]
  
  # write.csv(table_totals, "outputs/results_table_income.csv", row.names = F)
  
  # ---- Impact by income ----
  
  dt <- scenario_impact$dt
  dt <- merge(dt, loc_table[, .(location_id, income_group)], by = "location_id")
  baseline_dt <- dt[year == 2019]
  setnames(baseline_dt, "deaths_averted", "baseline_deaths_averted")
  baseline_dt[, year := NULL]
  table_dt <- merge(dt[year %in% 2021:2030], baseline_dt,
                    by = c("location_id", "income_group", "disease", "vaccine", "activity_type"),
                    all.x = T)
  table_dt[is.na(baseline_deaths_averted), baseline_deaths_averted := 0]
  table_dt[is.na(deaths_averted), deaths_averted := 0]
  table_dt[activity_type %in% c("routine", "combined"), 
           incremental := deaths_averted - baseline_deaths_averted]
  table_totals <- table_dt[, .(total = sum(deaths_averted, na.rm = T),
                               incremental = sum(incremental, na.rm = T)), by = .(income_group, disease, year)][
                                 order(income_group, disease, year)]
  
  # NOTE: Used to produce markdown document
  # write.csv(table_totals, "outputs/detailed_results_income.csv", row.names = F)
  saveRDS(table_totals, file = paste0(o$pth$results, "detailed_results_income.rds"))
  
  # ---- Produce markdown results document ----
  
  # Check flag
  if (o$results_markdown) {
    
    # Render document
    #
    # NOTE: Uses file outputs/detailed_results.csv
    rmarkdown::render(input             = paste0(o$pth$code, "results.Rmd"), 
                      # envir             = globalenv(), # globalenv
                      output_dir        = o$pth$figures, 
                      knit_root_dir     = o$pth$figures,
                      intermediates_dir = o$pth$figures, 
                      # clean             = FALSE,  # Save intermediates
                      quiet             = TRUE)
  }
  
  # ---- All diagnostic plots ----
  
  # Check flag
  if (o$plot_diagnostics) {
    
    message(" - Plotting diagnostics")
    
    # Plot parameters of fitted beta distribution to vaccine efficacy (GBD diseases only)
    fig_name = "Uncertainty distributions - GBD diseases"
    plot_gbd_uncertainty_dist(fig_name)  # See plotting.R
    
    # Plot uncertainty draws for all diseases
    fig_name = "Uncertainty draws - All diseases"
    plot_draws(fig_name)  # See plotting.R
    
    # Plot annual totals to check alignment of means
    fig_name = "Uncertainty bounds - Annual total"
    plot_annual_total(fig_name)  # See plotting.R
  }
}

