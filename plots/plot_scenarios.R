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
pdf("plots/IA2030_coverage_scenario_options.pdf")
for(v in unique(plot_dt$vaccine_id)) {
    gg <- ggplot(plot_dt[vaccine_id == v], aes(x = year, y = value, group = location_id)) +
        geom_line(alpha = 0.15) + facet_wrap(~label) + theme_classic() +
        ggtitle(vaccine_table[vaccine_id == v]$vaccine_long) + ylab("Coverage") +
        scale_x_discrete("Year", breaks = seq(2019, 2030, 2), labels = seq(2019, 2030, 2))
    print(gg)
}
dev.off()
