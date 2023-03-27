ui_results <- fread("results/220913_ui_results.csv")
results <- merge(ui_results, v_at_table, by = c("vaccine", "activity_type"))
results[, fvps := NULL]
coverage_21
pop_21

# Recalculate FVPs with new pop and coverage
pop <- merge(pop_21, loc_table[, .(location_iso3, location_id, location_name)], by = "location_iso3")
fvps <- merge(coverage_21, pop, by = c("location_id", "age", "sex_id", "year"))
fvps[, fvps := observed_coverage * pop]

# Use uncertain impact factors to convert to draws of deaths averted
all_dt <- merge(
    results[variable == "impact_factor"],
    fvps,
    by = c("location_name", "age", "v_at_id", "year")
)

for (i in 1:200) {
    all_dt[, paste0("averted_", i) := get(paste0("draw_", i)) * fvps]
}

# Global annual
annual_dt <- all_dt[order(year)][year %in% 2019:2021, lapply(.SD, sum), by = year, .SDcols = paste0("averted_", 1:200)]
apply(annual_dt[, -1], 1, mean)
apply(annual_dt[, -1], 1, quantile, c(0.025, 0.975))
