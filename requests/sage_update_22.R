library(data.table); library(ggplot2)
library(dplyr); library(data.table)

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
both_dt <- wpp_input[year %in% 2019:2022, .(nx = sum(nx)), .(location_id, age, year)]
both_dt[, sex_id := 3]
old_pop_dt <- rbind(wpp_input, both_dt, fill = T)
setnames(old_pop_dt, "nx", "old_pop")
old_pop_dt[, c("mx", "fx", "mig") := NULL]
old_pop_dt <- merge(old_pop_dt, loc_table[, .(location_id, location_iso3)], by = "location_id")
old_pop_dt[, location_id := NULL]

new_pop_dt <- copy(pop_21)
setnames(new_pop_dt, "pop", "new_pop")
pop_dt <- merge(old_pop_dt, new_pop_dt, by = c("location_iso3", "year", "age", "sex_id"))

# Merge on observed coverage
results_dt2 <- merge(results_dt, v_at_table, by = c("vaccine", "activity_type"))

coverage <- copy(coverage_22)
coverage[, fvps := NULL]
coverage[, sex_id := NULL]
coverage[v_at_id == 17, v_at_id := 22]
dt <- merge(
    coverage[!is.na(observed_coverage)], 
    results_dt2[year %in% 2019:2022, .(location_id, year, disease, v_at_id, age, impact_factor, fvps, deaths_averted, cohort_size)], 
    by = c("location_id", "year", "v_at_id", "age"),
    all.y = T)
dt[, sex_id := 3]
dt <- merge(dt, loc_table[, .(location_id, location_iso3)], by = "location_id")

# Use both sexes pop for old and only girls for new with HPV
girls_pop_dt <- pop_dt[sex_id == 2]
setnames(girls_pop_dt, "new_pop", "girl_pop")
girls_pop_dt[, sex_id := 3]
pop_dt <- merge(
    pop_dt, 
    girls_pop_dt[, .(location_iso3, year, age, sex_id, girl_pop)],
    by = c("location_iso3", "year", "age", "sex_id")
)

# Merge on populations
dt[, cohort_size := NULL]
dt <- merge(dt, pop_dt, by = c("location_iso3", "year", "age", "sex_id"), all.x = T)
setnames(dt, c("fvps", "deaths_averted"), c("target_fvps", "target_deaths_averted"))
dt[disease == "HPV", new_pop := girl_pop]
dt[, girl_pop := NULL]

dt[, target_coverage := target_fvps / old_pop]
dt[, target_fvps := target_coverage * new_pop]
dt[, target_deaths_averted := target_fvps * impact_factor]
dt[is.na(observed_coverage), observed_coverage := 0]
dt[, observed_fvps := new_pop * observed_coverage]
dt[, observed_deaths_averted := observed_fvps * impact_factor]

# Scale with 2019 scalar
# scalar_dt <- dt[year == 2019]
# scalar_dt[, scalar := observed_deaths_averted / target_deaths_averted]
# dt <- merge(dt, scalar_dt[, .(location_iso3, year, disease, scalar)], 
#     by = c("location_iso3", "year", "disease"))
# dt[, target_deaths_averted := target_deaths_averted * scalar]
# dt[, scalar := NULL]

out_dt <- merge(dt, loc_table[, .(location_iso3, location_name, region, income_group)])

write.csv(out_dt, "results_22/updated_reference_results_22.csv", row.names = F)



dt <- fread("results_22/updated_reference_results_22.csv")
dt <- dt[order(disease)]
dt[, disease := as.factor(disease)]

# Global deaths averted target and observed by disease
global_dt <- dt[, .(target_deaths_averted = sum(target_deaths_averted),
    observed_deaths_averted = sum(observed_deaths_averted)), by = .(disease, year)]
global_dt[, gap := observed_deaths_averted - target_deaths_averted]
global_dt[, region := "Global"]

global_target <- global_dt[, .(target = sum(target_deaths_averted)), by = year]

# Regional deaths averted target and observed by disease
region_dt <- dt[, .(target_deaths_averted = sum(target_deaths_averted),
    observed_deaths_averted = sum(observed_deaths_averted)), by = .(region, disease, year)]
region_dt[, gap := observed_deaths_averted - target_deaths_averted]

region_target <- region_dt[, .(target = sum(target_deaths_averted)), by = .(year, region)]

# Save regional and global
combined_dt <- rbind(global_dt, region_dt)
write.csv(combined_dt, "results_22/aggregate_updated_reference_results_22.csv")

## Plots
my_colors1 <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue", "darkgreen"))
my_colors1[1] <- "hotpink"

pdf("results_22/updated_reference_results_22.pdf")

# Global averted
gg <- ggplot() + 
    geom_bar(data = global_dt, position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = observed_deaths_averted / 1e6, fill = disease)) +
    geom_line(data = global_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    geom_point(data = global_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") +
    ggtitle("Observed global deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2019:2022) +
    scale_color_manual(name = "", values = c("IA2030\ntarget" = "black"))
print(gg)

# Regional averted
gg <- ggplot() + 
    geom_bar(data = region_dt, position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = observed_deaths_averted / 1e6, fill = disease)) +
    geom_line(data = region_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    geom_point(data = region_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") +
    ggtitle("Observed regional deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2019:2022) +
    scale_color_manual(name = "", values = c("IA2030\ntarget" = "black")) +
    facet_wrap(.~region, scales = "free_y")
print(gg)

# Global gap
gg <- ggplot() + 
    geom_bar(data = global_dt, position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = gap / 1e3, fill = disease)) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + ylab("Difference between observed and target deaths averted (in thousands)") + xlab("Year") +
    ggtitle("Global gaps in deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2019:2022) +
    geom_hline(yintercept = 0)
print(gg)

# Regional gap
gg <- ggplot() + 
    geom_bar(data = region_dt, position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = gap / 1e3, fill = disease)) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + ylab("Difference between observed and target deaths averted (in thousands)") + xlab("Year") +
    ggtitle("Regional gaps in deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2019:2022) +
    geom_hline(yintercept = 0) +
    facet_wrap(.~region, scales = "free_y")
print(gg)

dev.off()


temp <- global_dt[, .(total_observed = sum(observed_deaths_averted), 
    total_target = sum(target_deaths_averted), 
    gap = sum(gap)), by = (year)]
temp[, change := (total_target - total_observed) / total_target * 100]

prop.table(global_dt[year == 2022, .(observed_deaths_averted)])

temp <- region_dt[, .(total_observed = sum(observed_deaths_averted), 
    total_target = sum(target_deaths_averted), 
    gap = sum(gap)), by = .(year, region)]
temp[, change := (total_target - total_observed) / total_target * 100]


annual_dt <- combined_dt[, .(target_deaths_averted = sum(target_deaths_averted),
    observed_deaths_averted = sum(observed_deaths_averted)), by = .(region, year)]

write.csv(annual_dt, "results_22/region_annual_updated_reference_results_22.csv", row.names = F)

## Country-specific outputs
dt <- merge(dt, v_at_table)
l_d_t_dt <- dt[, .(target_deaths_averted = sum(target_deaths_averted),
    observed_deaths_averted = sum(observed_deaths_averted)), by = .(location_iso3, year, disease, vaccine)]

write.csv(l_d_t_dt, "results_22/observed_target_22_location_disease.csv", row.names = F)

