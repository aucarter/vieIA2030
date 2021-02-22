### Fit a model for VIMC mortality reduction
devtools::load_all()

## Predict all and plot results
params <- jsonlite::fromJSON("params.json")
pred_all <- impute_all_rr(params)
fit_summary <- summarize_fit(pred_all)
plot_strata_fit(pred_all)

## Calculate impact factors and rake to VIMC
impact_factors <- calc_impact_factors(pred_all)
impact_dt <- rake_impact(impact_factors)

## Merge on coverage scenario
past_dt <- coverage[year < 2020 & year >= 2000]
setnames(past_dt, "coverage", "value")
future_dt <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T)
fvp_future <- cov2fvp(future_dt)
scenario_dt <- rbind(past_dt, fvp_future)
scenario_impact <- calc_scenario_impact(scenario_dt, impact_dt)
temp_d_v_table <- copy(d_v_table)
setnames(temp_d_v_table, "disease", "disease2")
scenario_impact_add <- merge(scenario_impact$dt[is.na(disease)], temp_d_v_table, by = "vaccine", all.x = T)
scenario_impact_add[, disease := disease2]
scenario_impact_add[, c("disease2", "d_v_id") := NULL]
scenario_impact$dt <- rbind(scenario_impact$dt[!is.na(disease)], scenario_impact_add)
plot(scenario_impact$year_totals$year, scenario_impact$year_totals$total,
    ylim = c(0, 6e6), type = "l")

## Save for sharing
scenario_impact$dt[, vimc := ifelse(location_id %in% unique(vimc_impact$location_id), 1, 0)]
scenario_impact$dt[, gbd := ifelse(vaccine %in% c("DTP3", "BCG"), 1, 0)]
scenario_impact$dt[vimc == 1 & gbd == 0, label := "VIMC10 (VIMC locations)"]
scenario_impact$dt[vimc == 0 & gbd == 0, label := "Imputed VIMC10 (non-VIMC locations)"]
scenario_impact$dt[gbd == 1, label := "GBD4 (All locations)"]
out_dt <- merge(
    scenario_impact$dt[, .(location_id, year, disease, vaccine, activity_type, label, deaths_averted)],
    loc_table[, .(location_id, location_name, location_iso3)],
    by = "location_id"
)
out_dt <- out_dt[order(location_name, disease, vaccine, activity_type)]
out_dt <- out_dt[!is.na(deaths_averted)]
write.csv(out_dt, "outputs/v02_reference_results.csv", row.names = F)

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