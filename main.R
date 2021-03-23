### Fit a model for VIMC mortality reduction
devtools::load_all()

## Predict all and plot results
params <- jsonlite::fromJSON("params.json")
pred_all <- impute_all_rr(params, routine_only = T)
fit_summary <- summarize_fit(pred_all)
plot_strata_fit(pred_all)

## Calculate impact factors and rake to VIMC
impact_factors <- calc_impact_factors(pred_all)
impact_dt <- rake_impact(impact_factors)

## Merge on coverage scenario onto past for full set of FVPs
past_dt <- coverage[year < 2020 & year >= 2000]
setnames(past_dt, "coverage", "value")
future_dt <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T)
fvp_future <- cov2fvp(future_dt)
scenario_dt <- rbind(past_dt, fvp_future)

## Calculate scenario impact and clean up some missing labels
scenario_impact <- calc_scenario_impact(scenario_dt, impact_dt)
temp_d_v_table <- copy(d_v_table)
setnames(temp_d_v_table, "disease", "disease2")
scenario_impact_add <- merge(scenario_impact$dt[is.na(disease)], temp_d_v_table, by = "vaccine", all.x = T)
scenario_impact_add[, disease := disease2]
scenario_impact_add[, c("disease2", "d_v_id") := NULL]
scenario_impact$dt <- rbind(scenario_impact$dt[!is.na(disease)], scenario_impact_add)

## Plot the total by year
plot(scenario_impact$year_totals$year, scenario_impact$year_totals$total,
    ylim = c(0, 6e6), type = "l")

## Save for sharing
scenario_impact$dt[, vimc := ifelse(location_id %in% unique(vimc_impact$location_id), 1, 0)]
scenario_impact$dt[, gbd := ifelse(vaccine %in% c("DTP3", "BCG"), 1, 0)]
scenario_impact$dt[vimc == 1 & gbd == 0, label := "VIMC10 (VIMC locations)"]
scenario_impact$dt[vimc == 0 & gbd == 0, label := "Imputed VIMC10 (non-VIMC locations)"]
scenario_impact$dt[gbd == 1, label := "GBD4 (All locations)"]
out_dt <- merge(
    scenario_impact$dt[, .(location_id, year, disease, vaccine, activity_type, deaths_averted)],
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


write.csv(out_dt, "outputs/reference_results.csv", row.names = F)

## Plot total deaths averted by vaccine over time
pdf("plots/averted_by_vaccine.pdf")
disease_year_dt <- scenario_impact$dt[, .(averted = sum(deaths_averted, na.rm = T)), by = .(disease, year)] 
my_colors1 <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue", "darkgreen"))
my_colors2<- rev(RColorBrewer::brewer.pal(name = "Paired", n = 3))
## By vaccine
gg <- ggplot(disease_year_dt[year %in% 2000:2030], aes(x = year, y = averted / 1e6, fill = disease)) +
    geom_area(color = "white", alpha = 0.8) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions by YoV)") +
    ggtitle("Deaths averted by year of vaccination for IA2030 coverage scenario")
gg
dev.off()

## Plot with VIMC, non-VIMC, and GBD
pdf("plots/averted_by_imputation_group.pdf")
label_year_dt <- scenario_impact$dt[, .(averted = sum(deaths_averted, na.rm = T)), by = .(label, year)] 
gg <- ggplot(label_year_dt[year %in% 2000:2030], aes(x = year, y = averted / 1e6, fill = label)) +
    geom_area(color = "white", alpha = 0.8) +
    scale_fill_manual(values = my_colors2, name = "") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions by YoV)") +
    theme(legend.position = "bottom") +
    ggtitle("Deaths averted by year of vaccination for IA2030 coverage scenario")
gg
dev.off()

## Plot all locations
scen_dt <- merge(scenario_impact$dt, loc_table[, .(location_id, location_name)], by = "location_id")
loc_vacc_year_dt <- scen_dt[, .(averted = sum(deaths_averted, na.rm = T)), by = .(location_name, disease, year)] 
loc_label_year_dt <- scen_dt[, .(averted = sum(deaths_averted, na.rm = T)), by = .(location_name, label, year)] 
pdf("plots/deaths_averted_by_location.pdf", height = 8, width = 10)
for(loc in sort(unique(loc_vacc_year_dt$location_name))) {
    plot_dt1 <- loc_vacc_year_dt[location_name == loc]
    plot_dt2 <- loc_label_year_dt[location_name == loc]
    ## By vaccine
    gg <- ggplot(plot_dt1[year %in% 2000:2030], aes(x = year, y = averted , fill = disease)) +
        geom_area(color = "white", alpha = 0.8) +
        scale_fill_manual(values = my_colors1, name = "Vaccine") +
        theme_bw() + xlab("Year") + ylab("Deaths averted") +
        ggtitle(loc)
    print(gg)

    ## Plot with VIMC, non-VIMC, and GBD
    gg <- ggplot(plot_dt2[year %in% 2000:2030], aes(x = year, y = averted, fill = label)) +
        geom_area(color = "white", alpha = 0.8) +
        scale_fill_manual(values = my_colors2, name = "") +
        theme_bw() + xlab("Year") + ylab("Deaths averted") +
        ggtitle(loc)
    print(gg)
}
dev.off()

## Plot by region
dt <- fread("outputs/v02_reference_results.csv")
dt <- merge(dt, loc_table[, .(location_id, region)])
region_dt <- dt[, .(deaths_averted = sum(deaths_averted)), by = .(region, year)]
pdf("plots/region_plots.pdf")
gg <- ggplot(region_dt, aes(x = year, y = deaths_averted, color = region)) + 
    geom_line() + theme_bw() + xlab("Year") + ylab("Deaths averted") +
    ggtitle("Deaths averted by region")
print(gg)
dev.off()

## Plot by disease in each region
region_d_dt <- dt[, .(deaths_averted = sum(deaths_averted)), by = .(region, year, disease)]
full_dt <- data.table(expand.grid(
    region = unique(region_d_dt$region),
    year = unique(region_d_dt$year),
    disease = unique(region_d_dt$disease)
))
region_d_dt <- merge(region_d_dt, full_dt, all.y = T)
region_d_dt[is.na(deaths_averted), deaths_averted := 0]
my_colors1 <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue", "darkgreen"))
pdf("plots/region_disease_plots.pdf")
for(r in unique(region_d_dt$region)) {
    plot_dt <- region_d_dt[region == r]
    gg <- ggplot(plot_dt[year %in% 2000:2030], aes(x = year, y = deaths_averted , fill = disease)) +
    geom_area(color = "white", alpha = 0.8) +
    scale_fill_manual(values = my_colors1, name = "Vaccine") +
    theme_bw() + xlab("Year") + ylab("Deaths averted") +
    ggtitle(r)
    print(gg)

}
dev.off()

## Impact by region
no_lin_range_cov <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T)
scen_dt <- merge(no_lin_range_cov, loc_table[, .(location_id, location_name, location_iso3)], by = "location_id")
scen_dt <- merge(scen_dt, v_at_table, by = "v_at_id")
write.csv(scen_dt, "outputs/ia2030_cov_trajectories.csv", row.names = F)
no_lin_range <- cov2fvp(no_lin_range_cov)
past_dt <- coverage[year == 2019]
setnames(past_dt, "coverage", "value")
cov_dt <- rbind(past_dt, no_lin_range, fill = T)
scenario_impact <- calc_scenario_impact(cov_dt, impact_dt)
temp_d_v_table <- copy(d_v_table)
setnames(temp_d_v_table, "disease", "disease2")
scenario_impact_add <- merge(scenario_impact$dt[is.na(disease)], temp_d_v_table, by = "vaccine", all.x = T)
scenario_impact_add[, disease := disease2]
scenario_impact_add[, c("disease2", "d_v_id") := NULL]
scenario_impact$dt <- rbind(scenario_impact$dt[!is.na(disease)], scenario_impact_add)
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
write.csv(table_totals, "outputs/detailed_results.csv", row.names = F)

global_dt <- table_totals[, .(deaths_averted = sum(deaths_averted, na.rm = T)), by = .(year, disease, vaccine, activity_type)]
global_dt[, region := "Global"]
region_dt <- table_totals[, .(deaths_averted = sum(deaths_averted, na.rm = T)), by = .(region, year, disease, vaccine, activity_type)]
all_dt <- rbind(global_dt, region_dt)

write.csv(table_totals, "outputs/results_table_region.csv", row.names = F)

## 
no_lin_range <- cov2fvp(gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T))
past_dt <- coverage[year == 2019]
setnames(past_dt, "coverage", "value")
cov_dt <- rbind(past_dt, no_lin_range, fill = T)
dt <- calc_scenario_impact(cov_dt, impact_dt)$dt
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

dt[, (deaths_averted)]


## Impact by income
no_lin_range <- cov2fvp(gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T))
past_dt <- coverage[year == 2019]
setnames(past_dt, "coverage", "value")
cov_dt <- rbind(past_dt, no_lin_range, fill = T)
scenario_impact <- calc_scenario_impact(cov_dt, impact_dt)
temp_d_v_table <- copy(d_v_table)
setnames(temp_d_v_table, "disease", "disease2")
scenario_impact_add <- merge(scenario_impact$dt[is.na(disease)], temp_d_v_table, by = "vaccine", all.x = T)
scenario_impact_add[, disease := disease2]
scenario_impact_add[, c("disease2", "d_v_id") := NULL]
scenario_impact$dt <- rbind(scenario_impact$dt[!is.na(disease)], scenario_impact_add)
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
