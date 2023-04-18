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
  # load_tables("wpp_input")
  
  # Load impact factors calculated in step 2
  # impact_dt       = try_load(o$pth$impact_factors, "impact_dt")
  scenario_impact = try_load(o$pth$impact_factors, "scenario_impact")
  
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
  write.csv(table_totals, "outputs/detailed_results.csv", row.names = F)
  
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
  
  write.csv(table_totals, "outputs/results_table_income.csv", row.names = F)
  
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
                                 order(income_group, disease, year)
                               ]
  write.csv(table_totals, "outputs/detailed_results_income.csv", row.names = F)
  
  # ---- Produce markdown results document ----
  
  # Check flag
  if (o$do_markdown) {
    
    # Render document
    #
    # NOTE: Uses file outputs/detailed_results.csv
    rmarkdown::render(paste0(o$pth$code, "results.Rmd"))
  }
}

