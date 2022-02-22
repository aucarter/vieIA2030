calc_scenario_impact <- function(scenario_dt, impact_dt) {
    scenario_dt <- merge(scenario_dt, v_at_table, by = "v_at_id")
    scenario_dt[vaccine == "Rubella", activity_type := "combined"]
    scenario_dt[, v_at_id := NULL]
    dt <- merge(
        impact_dt[, .(location_id, disease, vaccine, activity_type, impact_factor)],
        scenario_dt[fvps > 0, .(location_id, year, age, vaccine, activity_type, fvps)],
        by = c("location_id", "vaccine", "activity_type"), 
        allow.cartesian = T
    )
    dt[, deaths_averted := fvps * impact_factor]
    year_vaccine_totals <- dt[, .(total = sum(deaths_averted, na.rm = T)), by = .(year, disease, vaccine)]
    year_totals <- year_vaccine_totals[, .(total = sum(total, na.rm = T)), by = .(year)][order(year)]
    return(list(dt = dt, year_vaccine_totals = year_vaccine_totals, 
        year_totals = year_totals))
}