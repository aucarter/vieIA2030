## Paper results
dt <- fread("outputs/reference_results.csv")

# Annual
year_total_draws <- dt[year %in% 2021:2030, lapply(.SD, sum), by = .(year), .SDcols = paste0("draw_", 1:200)]
total_draws <- dt[year %in% 2021:2030][,year := 9999][, lapply(.SD, sum), by = .(year), .SDcols = paste0("draw_", 1:200)]
bound_dt <- rbind(year_total_draws, total_draws)
melt_year_total_draws <- melt(bound_dt, id.vars = "year")
year_total_summary <- melt_year_total_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = year]
year_total_summary[, column_text := paste0(round(mean  / 1e6, 1), " (", round(lower / 1e6, 1), "-", round(upper / 1e6, 1), ")")]
write.csv(year_total_summary, "outputs/paper/annual_numbers.csv", row.names = F)

# Historical table
year_total_draws <- dt[year %in% 2011:2020, lapply(.SD, sum), by = .(year), .SDcols = paste0("draw_", 1:200)]
total_draws <- dt[year %in% 2011:2020][,year := 9999][, lapply(.SD, sum), by = .(year), .SDcols = paste0("draw_", 1:200)]
bound_dt <- rbind(year_total_draws, total_draws)
melt_year_total_draws <- melt(bound_dt, id.vars = "year")
historical_summary <- melt_year_total_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = year]
historical_summary[, column_text := paste0(round(mean  / 1e6, 1), " (", round(lower / 1e6, 1), "-", round(upper / 1e6, 1), ")")]
write.csv(historical_summary, "outputs/paper/historical_numbers.csv", row.names = F)

# By disease
dt <- dt[order(disease)]
dt[, disease := as.factor(disease)]
disease_draws <- dt[year %in% 2021:2030, lapply(.SD, sum), by = .(disease), .SDcols = paste0("draw_", 1:200)]
melt_disease_draws <- melt(disease_draws, id.vars = "disease")
disease_summary <- melt_disease_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = disease]
disease_summary[, column_text := paste0(round(mean  / 1e6, 1), " (", round(lower / 1e6, 1), "-", round(upper / 1e6, 1), ")")]
disease_summary[, percent := round(100 * mean / sum(mean), 1)]
write.csv(disease_summary, "outputs/paper/disease_summary.csv", row.names = F)

# By income
income_group_draws <- dt[year %in% 2021:2030, lapply(.SD, sum), by = .(income_group), .SDcols = paste0("draw_", 1:200)]
melt_income_group_draws <- melt(income_group_draws, id.vars = "income_group")
income_group_summary <- melt_income_group_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = income_group]
income_group_summary[, column_text := paste0(round(mean  / 1e6, 1), " (", round(lower / 1e6, 1), "-", round(upper / 1e6, 1), ")")]
income_group_summary[, percent := round(100 * mean / sum(mean), 1)]
write.csv(income_group_summary, "outputs/paper/income_group_summary.csv", row.names = F)

# By WHO region
region_draws <- dt[year %in% 2021:2030, lapply(.SD, sum), by = .(region), .SDcols = paste0("draw_", 1:200)]
melt_region_draws <- melt(region_draws, id.vars = "region")
region_summary <- melt_region_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = region]
region_summary[, column_text := paste0(round(mean  / 1e6, 1), " (", round(lower / 1e6, 1), "-", round(upper / 1e6, 1), ")")]
region_summary[, percent := round(100 * mean / sum(mean), 1)]
write.csv(region_summary, "outputs/paper/region_summary.csv", row.names = F)

# By Gavi
gavi73_draws <- dt[year %in% 2021:2030 & gavi73 == 1, lapply(.SD, sum),, .SDcols = paste0("draw_", 1:200)]
total_draws <- dt[year %in% 2021:2030, lapply(.SD, sum),, .SDcols = paste0("draw_", 1:200)]
prop <- unlist(gavi73_draws / total_draws)
gavi73_summary <- data.table(mean = mean(prop), 
    lower = quantile(prop, 0.05), upper = quantile(prop, 0.95))
write.csv(gavi73_summary, "outputs/paper/gavi73_summary.csv", row.names = F)
