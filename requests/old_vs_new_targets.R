## Plot scenarios comparison
library(ggplot2)

old_targets <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T, new_data = F)

old_targets[, version := "Old"]

new_targets <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T, data_year = 2022)

new_targets[, version := "New"]

plot_dt <- rbindlist(list(old_targets, new_targets))
plot_dt <- plot_dt[!(v_at_id %in% c(14, 23))]


## Plot example location
ex_dt <- plot_dt[location_id == 5]
ex_dt <- merge(ex_dt, v_at_table[, .(v_at_id, vaccine)], by = "v_at_id")
pdf("plots/coverage_example.pdf", width = 13, height = 9)
gg <- ggplot(ex_dt, aes(x = year, y = value, linetype = version)) +
    geom_line() + theme_classic() + ylim(c(0,1)) +
    ggtitle("Angola IA2030 target coverage") + ylab("Coverage") +
    scale_x_continuous("Year", breaks = seq(2020, 2030, 5), labels = seq(2020, 2030, 5)) +
    theme(text = element_text(size=20), legend.title = element_blank(), panel.spacing = unit(2, "lines")) +
    facet_wrap(.~vaccine)
print(gg)
dev.off()


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


# Grab old pop
load_tables("wpp_input")
both_dt <- wpp_input[, .(nx = sum(nx)), .(country, age, year)]
both_dt[, sex_id := 3]
old_pop_dt <- rbind(wpp_input, both_dt, fill = T)
setnames(old_pop_dt, c("nx", "country"), c("old_pop", "location_iso3"))
old_pop_dt[, c("mx", "fx", "mig") := NULL]

setnames(new_targets, "value", "new_target")
new_targets[, version := NULL]
# setnames(old_targets, "value", "old_target")
# old_targets[, version := NULL]

dt <- merge(new_targets, new_pop_dt, by = c("location_id", "year", "sex_id", "age"))
# dt <- merge(old_targets, dt, by = c("location_id", "v_at_id", "year", "sex_id", "age"))

if_dt <- unique(results_dt[, .(location_id, disease, vaccine, activity_type, age, impact_factor)])
if_dt <- merge(if_dt, v_at_table)

dt <- merge(dt, if_dt, by = c("location_id", "v_at_id", "age"))

dt <- merge(dt, old_pop_dt, by = c("location_iso3", "year", "sex_id", "age"))

dt[, new_deaths_averted := new_target * pop * impact_factor]
sum(dt[year > 2020]$new_deaths_averted)

dt[, old_deaths_averted := old_target * pop * impact_factor]
sum(dt[year > 2020]$old_deaths_averted)

annual_dt <- dt[, .(New = sum(new_deaths_averted), Old = sum(old_deaths_averted)), by = .(year, disease)]
annual_dt[, diff := Old - New]
melt_dt <- melt(annual_dt, id.vars = c("year", "disease"))

my_colors1 <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue", "darkgreen"))
my_colors1[1] <- "hotpink"

ggplot(annual_dt, aes(x = year, y = diff, fill = disease)) + 
  geom_bar(stat = 'identity', position = 'stack') + theme_bw() +
  scale_fill_manual(values = my_colors1, name = "Disease") +
  labs(x = "Year", y = "Difference in target deaths averted")


out_target_dt <- dt[, .(target = sum(new_deaths_averted)), by = .(disease, year)]

out_target_dt <- merge(out_target_dt, global_dt[, .(disease, year, observed_deaths_averted)], all = T)

out_target_dt <- out_target_dt[year > 2019][order(disease, year)]
