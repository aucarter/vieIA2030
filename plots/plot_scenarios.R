## Plot scenarios
library(ggplot2)
lin_range <- gen_ia2030_goals(ia2030_dtp_goal, linear = T, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T)
lin_range[, label := "Linear; Intro range"]

lin_no_range <- gen_ia2030_goals(ia2030_dtp_goal, linear = T, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = F)
lin_no_range[, label := "Linear; No intro range"]

no_lin_range <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T)
no_lin_range[, label := "Non-linear; Intro range"]

no_lin_no_range <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = F)
no_lin_no_range[, label := "Non-linear; No intro range"]

plot_dt <- rbindlist(list(lin_range, lin_no_range, no_lin_range, no_lin_no_range))
plot_dt <- merge(plot_dt, data.table(sex_id = 1:3, sex_name = c("Male", "Female", "Both")))
pdf("plots/IA2030_coverage_scenario_options.pdf")
for (v in unique(plot_dt$v_at_id)) {
    if (v == 2) {
        gg <- ggplot(plot_dt[v_at_id == v], aes(x = year, y = value, group = location_id)) +
            geom_line(alpha = 0.15) + facet_wrap(~sex_name) + theme_classic() + ylim(c(0,1)) +
            ggtitle(v_at_table[v_at_id == v]$vaccine) + ylab("Coverage") +
            scale_x_discrete("Year", breaks = seq(2019, 2030, 2), labels = seq(2019, 2030, 2))
        print(gg)
    } else {
        gg <- ggplot(plot_dt[v_at_id == v], aes(x = year, y = value, group = location_id)) +
            geom_line(alpha = 0.15) + facet_wrap(~label) + theme_classic() + ylim(c(0,1)) +
            ggtitle(v_at_table[v_at_id == v]$vaccine) + ylab("Coverage") +
            scale_x_discrete("Year", breaks = seq(2019, 2030, 2), labels = seq(2019, 2030, 2))
        print(gg)
    }

}
dev.off()

add_dt <- rbindlist(lapply(unique(plot_dt$label), function(l) {
    coverage[year < 2019 & v_at_id %in% unique(plot_dt$v_at_id)][, label := l]
}))
setnames(add_dt, "coverage", "value")
plot_dt2 <- rbind(plot_dt, add_dt, fill = T)
plot_dt2 <- merge(plot_dt2, v_at_table[, .(v_at_id, vaccine)])
plot_dt2[, year := as.integer(as.character(year))]
pdf("plots/IA2030_coverage_scenario_by_loc.pdf")
for (l in unique(plot_dt2$location_id)) {
    gg <- ggplot(plot_dt2[location_id == l], aes(x = year, y = value, color = vaccine)) +
        geom_line() + facet_wrap(~label) + theme_classic() + ylim(c(0,1)) +
        ggtitle(loc_table[location_id == l]$location_name) + ylab("Coverage") +
        scale_x_discrete("Year", breaks = seq(2000, 2030, 5), labels = seq(2000, 2030, 5))
    print(gg)

}
dev.off()

## Plot impact of each scenario
lin_range_impact <- calc_scenario_impact(cov2fvp(lin_range), impact_dt)$year_totals
lin_range_impact[, label := "Linear; Intro range"]

lin_no_range_impact <- calc_scenario_impact(cov2fvp(lin_no_range), impact_dt)$year_totals
lin_no_range_impact[, label := "Linear; No intro range"]

no_lin_range_impact <- calc_scenario_impact(no_lin_range, impact_dt)$year_totals
no_lin_range_impact[, label := "Non-linear; Intro range"]

no_lin_no_range_impact <- calc_scenario_impact(no_lin_no_range, impact_dt)$year_totals
no_lin_no_range_impact[, label := "Non-linear; No intro range"]
plot_dt <- rbindlist(list(lin_range_impact, lin_no_range_impact, no_lin_range_impact, no_lin_no_range_impact))

pdf("plots/impact_by_scenario.pdf")
gg <- ggplot(plot_dt, aes(x = year, y = total / 1e6, color = label)) + geom_line() +
    theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2)) +
    xlab("Year") + ylab("Deaths averted (in millions by YoV)") +
    ggtitle("Deaths averted by year of vaccination for IA2030 coverage scenarios")
gg
dev.off()

## Total number split by introduced and not
lin_range_impact <- calc_scenario_impact(lin_range, impact_dt)$dt
lin_range_impact[, label := "Linear; Intro range"]

lin_no_range_impact <- calc_scenario_impact(lin_no_range, impact_dt)$dt
lin_no_range_impact[, label := "Linear; No intro range"]

no_lin_range_impact <- calc_scenario_impact(no_lin_range, impact_dt)$dt
no_lin_range_impact[, label := "Non-linear; Intro range"]

no_lin_no_range_impact <- calc_scenario_impact(no_lin_no_range, impact_dt)$dt
no_lin_no_range_impact[, label := "Non-linear; No intro range"]
dt <- rbindlist(list(lin_range_impact, lin_no_range_impact, no_lin_range_impact, no_lin_no_range_impact))
intro_dt <- dt[, .(intro = ifelse(min(year) == 2020, 0, 1)), by = .(location_id, vaccine, activity_type)]
dt <- merge(dt, intro_dt)
total_dt <- dt[year > 2020, .(total_deaths_averted = sum(deaths_averted, na.rm = T)), by = .(intro, label)]
write.csv(total_dt, "total_averted_by_scenario_intro.csv", row.names = F)

total_dt[, .(total = sum(total_deaths_averted)), by = label]

## Plot incremental impact
lin_range_impact <- calc_scenario_impact(cov2fvp(lin_range), impact_dt)$year_totals
lin_range_impact[, label := "Linear; Intro range"]

lin_no_range_impact <- calc_scenario_impact(cov2fvp(lin_no_range), impact_dt)$year_totals
lin_no_range_impact[, label := "Linear; No intro range"]

no_lin_range_impact <- calc_scenario_impact(cov2fvp(no_lin_range), impact_dt)$year_totals
no_lin_range_impact[, label := "Non-linear; Intro range"]

no_lin_no_range_impact <- calc_scenario_impact(cov2fvp(no_lin_no_range), impact_dt)$year_totals
no_lin_no_range_impact[, label := "Non-linear; No intro range"]
plot_dt <- rbindlist(list(lin_range_impact, lin_no_range_impact, no_lin_range_impact, no_lin_no_range_impact))

dt19 <- coverage[ year == 2019]
impact_19 <- calc_scenario_impact(dt19, impact_dt)$year_totals

plot_dt[, total := total - impact_19$total]

pdf("plots/incremental_impact_by_scenario.pdf")
gg <- ggplot(plot_dt, aes(x = year, y = total / 1e6, color = label)) + geom_line() +
    theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 2)) +
    xlab("Year") + ylab("Incremental deaths averted (in millions by YoV)") +
    ggtitle("Incremental deaths averted by year of vaccination for IA2030 coverage scenarios")
gg
dev.off()