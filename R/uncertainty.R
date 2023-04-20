###########################################################
# UNCERTAINTY
#
# xxxxxxx
#
###########################################################

# ---------------------------------------------------------
# xxxxxxxx
# Called by: launch.R, main.R (and other launch-style scripts)
# ---------------------------------------------------------
run_uncertainty = function() {
  
  # Only continue if specified by do_step
  if (!is.element(3, o$do_step)) return()
  
  message("* Generating uncertainty")
  
  # Load tables up front (see db_utils.R)
  load_tables("wpp_input")
  
  # Load impact factors calculated in step 2
  impact_dt       = try_load(o$pth$impact_factors, "impact_dt")
  scenario_impact = try_load(o$pth$impact_factors, "scenario_impact")
  
  # ---- Draws for VIMC diseases ----
  
  message(" - VIMC diseases")
  
  vimc_ui
  vimc_draws <- merge(
    scenario_impact$dt[!(disease %in% efficacy_ui$disease)],
    vimc_ui[, .(disease, location_id, year, sd)],
    by = c("disease", "location_id", "year"),
    all.x = T
  )
  raking_summary <- summarize_raking(impact_dt)
  vimc_draws <- merge(
    vimc_draws,
    raking_summary[, .(vaccine, cv)],
    by = "vaccine",
    all.x = T
  )
  vimc_draws[is.na(sd), sd := deaths_averted * cv]
  vimc_draws_mat <- t(mapply(rnorm, n = 200,  mean = vimc_draws$deaths_averted, sd = vimc_draws$sd))
  colnames(vimc_draws_mat) <- paste0("draw_", 1:200)
  vimc_draws_wide <- cbind(
    vimc_draws[, .(disease, location_id, vaccine, activity_type, impact_factor, year, age, fvps, deaths_averted)],
    vimc_draws_mat
  )
  
  # ---- Draws for non-VIMC diseases ----
  
  message(" - GBD diseases")
  
  # Efficacy
  efficacy_ui
  efficacy_draws <- merge(efficacy_ui, scenario_impact$dt, by = "disease", allow.cartesian = T)
  efficacy_draws[, deaths_averted_draw := deaths_averted * scalar]
  efficacy_draws[, scalar := NULL]
  efficacy_draws_wide <- dcast(efficacy_draws, ... ~ draw, value.var = "deaths_averted_draw")
  
  # ---- Combine and rescale to the mean ----
  
  message(" - Combining draws")
  
  draws_dt <- rbind(efficacy_draws_wide, vimc_draws_wide)
  draws_dt[, draw_mean := rowMeans(draws_dt[, grep("draw", names(draws_dt), value = T), with = F])]
  draws_dt[, mean_diff := deaths_averted - draw_mean]
  draws_dt[, grep("draw", names(draws_dt), value = T) := draws_dt[, grep("draw", names(draws_dt), value = T), with = F] + mean_diff]
  draws_dt[, c("draw_mean", "mean_diff") := NULL]
  
  year_total_draws <- draws_dt[, lapply(.SD, sum), by = .(year), .SDcols = paste0("draw_", 1:200)]
  melt_year_total_draws <- melt(year_total_draws, id.vars = "year")
  year_total_summary <- melt_year_total_draws[, .(mean = mean(value), 
                                                  lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
                                              by = year]

  # Diagnostic plot
  # ggplot(year_total_summary, aes(x = year)) + 
  #   geom_ribbon(aes(ymin = lower, ymax = upper), 
  #               colour = "red", fill = "red", alpha = 0.5) + 
  #   geom_line(aes(y = mean), colour = "red", linewidth = 2) + 
  #   geom_point(data = scenario_impact$year_totals,
  #              mapping = aes(y = total), 
  #              colour = "black")
  
  message(" - Saving results")
  
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
  
  # Other ways to output these results
  # write.csv(out_dt, "outputs/reference_results.csv", row.names = F)
  # upload_object(out_dt, "ia2030_reference_results")
}

