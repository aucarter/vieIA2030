# Make reference results with HPV fix
dt <-  fread("outputs/reference_results.csv")
hpv_results <- fread("supp_data/hpv_fix.csv")
hpv_results <- merge(hpv_results, loc_table[, .(location_id, location_iso3)], by = "location_id")
dt <- rbind(
    dt[!(vaccine == "HPV" & activity_type == "routine")],
    hpv_results,
    fill = T
)

# Calculate mean impact factors among draw-level impact factors
mean_impact_factors <- impact_factors[, .(impact_factor_new = mean(pred_deaths_averted_rate, na.rm = T))
    , by = .(location_id, disease, vaccine, activity_type)]

# Merge onto results, keeping all results
merge_dt <- merge(
    mean_impact_factors, 
    unique(dt[, .(location_id, disease, vaccine, activity_type, impact_factor)]), 
    by = c("location_id", "disease", "vaccine", "activity_type"),
    all.y = T
)

# Calculate multplicative scalar between old and new impact factor
merge_dt[, scalar := impact_factor / impact_factor_new]

impact_factors2 <- merge(
    impact_factors,
    merge_dt[, .(location_id, disease, vaccine, activity_type, scalar)],
    by = c("location_id", "disease", "vaccine", "activity_type")
)

impact_factors2[, impact_factor := pred_deaths_averted_rate * scalar]
dt2 <- merge(
    dt[, .(location_id, year, disease, vaccine, activity_type, age, fvps)],
    impact_factors2[, .(location_id, draw, disease, vaccine, activity_type, impact_factor)],
    by = c("location_id", "disease", "vaccine", "activity_type"), allow.cartesian = T
)

dt2[, deaths_averted := fvps * impact_factor]
mean_dt <- dt2[, .(mean = mean(deaths_averted, na.rm = T)), by = .(year, location_id, disease, vaccine, activity_type, age)]

mean_dt[ , .(total = sum(mean, na.rm = T), ), by = .(year)]
sum(mean_dt[year %in% 2021:2030]$mean)

quantile(dt2[year %in% 2021:2030, .(total = sum(deaths_averted)), by = .(draw)]$total, c(0.025, 0.975), na.rm = T)
mean(dt2[year %in% 2021:2030, .(total = sum(deaths_averted)), by = .(draw)]$total, na.rm = T)


old_dt <-  fread("outputs/reference_results.csv")
old_draws <- melt(old_dt, id.vars = "year", measure.vars = grep("draw", names(dt), value = T), variable.name = "draw")
old_annual_draws <- old_draws[year %in% 2021:2030, .(total = sum(value, na.rm = T)), by = .(draw)]
old_annual_draws[, Version := "Original"]
new_annual_draws <- dt2[year %in% 2021:2030, .(total = sum(deaths_averted)), by = .(draw)]
new_annual_draws[, Version := "Updated"]

plot_dt <- rbind(old_annual_draws, new_annual_draws)

pdf("ui_comp.pdf", width = 8, height = 5)
gg <- ggplot(plot_dt, aes(x = total  / 1e6, fill = Version)) + geom_density(alpha = 0.5) +
    xlim(c(0, max(plot_dt$total) / 1e6)) +
    theme_bw() + xlab("Total deaths averted (in millions)") + ylab("Density") +
    ggtitle("Updated uncertainty for total deaths averted 2021-2030 under IA2030 targets")
print(gg)
dev.off()

old_draws <- melt(old_dt, id.vars = c("year", "income_group"), measure.vars = grep("draw", names(dt), value = T), variable.name = "draw")
old_annual_draws <- old_draws[year %in% 2021:2030, .(total = sum(value, na.rm = T)), by = .(draw, income_group)]
old_annual_draws[, Version := "Original"]

dt3 <- merge(dt2, loc_table[, .(location_id, income_group)], by = "location_id")
new_annual_draws <- dt3[year %in% 2021:2030, .(total = sum(deaths_averted)), by = .(draw, income_group)]
new_annual_draws[, Version := "Updated"]

plot_dt <- rbind(old_annual_draws, new_annual_draws)

pdf("ui_comp_income_group.pdf", width = 8, height = 5)
gg <- ggplot(plot_dt, aes(x = total  / 1e6, fill = Version)) + geom_density(alpha = 0.5) +
    theme_bw() + xlab("Total deaths averted (in millions)") + ylab("Density") +
    facet_wrap(.~income_group, scales = "free") +
    expand_limits(x = 0) +
    ggtitle("Updated uncertainty for total deaths averted 2021-2030 under\nIA2030 targets by region")
print(gg)
dev.off()

named_dt <- merge(dt2, loc_table[, .(location_id, location_name)], by = "location_id")
named_dt[, location_id := NULL]
melt_dt <- melt(named_dt, id.vars = c("location_name", "disease", "vaccine", "activity_type",
    "year", "age", "draw", "fvps"))
melt_dt[, draw := paste0("draw_", draw)]
out_dt <- dcast(
    data = melt_dt, 
    formula = location_name + disease + vaccine + activity_type + year + age + variable + fvps ~ draw, 
    value.var = "value"
)
write.csv(out_dt, 'results/220913_ui_results.csv', row.names = F)


write.csv(out_dt[location_name == "Botswana"], 'results/220913_BWA_ui_results.csv', row.names = F)
