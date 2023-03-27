loc <- 192
v_at <- 8


ui_results <- fread("results/220913_ui_results.csv")
results <- merge(ui_results, v_at_table, by = c("vaccine", "activity_type"))
results[, fvps := NULL]
coverage_21
pop_21

# Recalculate FVPs with new pop and coverage
pop <- merge(pop_21, loc_table[, .(location_iso3, location_id, location_name)], by = "location_iso3")
fvps <- merge(coverage_21, pop, by = c("location_id", "age", "sex_id", "year"))

# Merge on impact factors
all_dt <- merge(
    results[variable == "impact_factor"],
    fvps[location_id == loc & v_at_id == v_at],
    by = c("location_name", "age", "v_at_id", "year")
)

# Add on counterfactual years
for(i in )
copy_dt <- all_dt[year == 2021]
copy_dt[, year := 2022]
copy_dt[, observed_coverage := 0.3]
all_dt <- rbind(all_dt, copy_dt)


copy_dt <- all_dt[year == 2021]
copy_dt[, year := 2023]
copy_dt[, observed_coverage := 0.9]
all_dt <- rbind(all_dt, copy_dt)

all_dt[, fvps := observed_coverage * pop]

for (i in 1:200) {
    all_dt[, paste0("averted_", i) := get(paste0("draw_", i)) * fvps]
}
rowMeans(all_dt[year %in% 2022:2023, grepl("averted_", names(all_dt)), with = F])
results <- unlist(lapply(all_dt[year %in% 2022:2023, grepl("averted_", names(all_dt)), with = F], diff))
