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
