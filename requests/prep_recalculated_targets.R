## Plot scenarios comparison
library(ggplot2)
new_targets <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T, data_year = 2022)

new_pop_dt <- copy(pop_21)
new_pop_dt <- merge(new_pop_dt, loc_table[, .(location_id, location_iso3)])


results_dt <- fread("https://storage.googleapis.com/vie_ia2030/ia2030_reference_results.csv")
hpv_results <- fread("supp_data/hpv_fix.csv")
hpv_results <- merge(hpv_results, loc_table[, .(location_id, location_iso3)], by = "location_id")

results_dt <- rbind(
    results_dt[!(vaccine == "HPV" & activity_type == "routine")],
    hpv_results,
    fill = T
)


setnames(new_targets, "value", "new_target")


dt <- merge(new_targets, new_pop_dt, by = c("location_id", "year", "sex_id", "age"))

if_dt <- unique(results_dt[, .(location_id, disease, vaccine, activity_type, age, impact_factor)])
if_dt <- merge(if_dt, v_at_table)
if_dt[v_at_id == 22, v_at_id := 17]

dt <- merge(dt, if_dt, by = c("location_id", "v_at_id", "age"))

dt[, new_deaths_averted := new_target * pop * impact_factor]

annual_dt <- dt[order(location_iso3, year, disease), .(target = sum(new_deaths_averted)), by = .(location_iso3, year, disease, v_at_id)]

write.csv(annual_dt[], "results_22b/recalculated_targets.csv", row.names = F)
