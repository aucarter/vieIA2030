library(data.table); library(ggplot2)

dt  <- fread("outputs/reference_results.csv")

# Add decade
dt[, decade := "2001-2010"]
dt[year > 2010, decade := "2011-2020"]
dt[year > 2020, decade := "2021-2030"]

dt <- dt[disease == "PCV"]

# Pathogen decade
disease_draws <- dt[, lapply(.SD, sum), by = .(disease, decade), .SDcols = paste0("draw_", 1:200)]
melt_disease_draws <- melt(disease_draws, id.vars = c("disease", "decade"))
disease_summary <- melt_disease_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = .(disease, decade)]
full_dt <- data.table(expand.grid(disease = unique(disease_summary$disease), decade = unique(disease_summary$decade)))
disease_summary <- merge(full_dt, disease_summary, by = c("disease", "decade"), all.x = T)
disease_summary[is.na(mean), c("mean", "lower", "upper") := 0]
disease_summary[, column_text := paste0(round(mean  / 1e6, 1), " (", round(lower / 1e6, 1), "-", round(upper / 1e6, 1), ")")]
cast_disease_summary <- dcast(disease_summary, disease ~ decade, value.var = "column_text")
write.csv(cast_disease_summary, "requests/pneumo/disease_decade_summary.csv", row.names = F)
# Pathogen year
disease_draws <- dt[, lapply(.SD, sum), by = .(disease, year), .SDcols = paste0("draw_", 1:200)]
melt_disease_draws <- melt(disease_draws, id.vars = c("disease", "year"))
disease_summary <- melt_disease_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = .(disease, year)]
full_dt <- data.table(expand.grid(disease = unique(disease_summary$disease), year = unique(disease_summary$year)))
disease_summary <- merge(full_dt, disease_summary, by = c("disease", "year"), all.x = T)
disease_summary[is.na(mean), c("mean", "lower", "upper") := 0]
disease_summary[, column_text := paste0(round(mean  / 1e5, 1), " (", round(lower / 1e5, 1), "-", round(upper / 1e5, 1), ")")]
cast_disease_summary <- dcast(disease_summary, disease ~ year, value.var = "column_text")
write.csv(cast_disease_summary, "requests/pneumo/disease_year_summary.csv", row.names = F)
# Region
region_draws <- dt[, lapply(.SD, sum), by = .(region, decade), .SDcols = paste0("draw_", 1:200)]
melt_region_draws <- melt(region_draws, id.vars = c("region", "decade"))
region_summary <- melt_region_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = .(region, decade)]
full_dt <- data.table(expand.grid(region = unique(region_summary$region), decade = unique(region_summary$decade)))
region_summary <- merge(full_dt, region_summary, by = c("region", "decade"), all.x = T)
region_summary[is.na(mean), c("mean", "lower", "upper") := 0]
region_summary[, column_text := paste0(round(mean  / 1e5, 1), " (", round(lower / 1e5, 1), "-", round(upper / 1e5, 1), ")")]
cast_region_summary <- dcast(region_summary, region ~ decade, value.var = "column_text")
write.csv(cast_region_summary, "requests/pneumo/region_decade_summary.csv", row.names = F)
# Income group
income_group_draws <- dt[, lapply(.SD, sum), by = .(income_group, decade), .SDcols = paste0("draw_", 1:200)]
melt_income_group_draws <- melt(income_group_draws, id.vars = c("income_group", "decade"))
income_group_summary <- melt_income_group_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = .(income_group, decade)]
full_dt <- data.table(expand.grid(income_group = unique(income_group_summary$income_group), decade = unique(income_group_summary$decade)))
income_group_summary <- merge(full_dt, income_group_summary, by = c("income_group", "decade"), all.x = T)
income_group_summary[is.na(mean), c("mean", "lower", "upper") := 0]
income_group_summary[, column_text := paste0(round(mean  / 1e6, 1), " (", round(lower / 1e6, 1), "-", round(upper / 1e6, 1), ")")]
cast_income_group_summary <- dcast(income_group_summary, income_group ~ decade, value.var = "column_text")
write.csv(cast_income_group_summary, "requests/pneumo/income_group_decade_summary.csv", row.names = F)
# Gavi
gavi73_draws <- dt[, lapply(.SD, sum), by = .(gavi73, decade), .SDcols = paste0("draw_", 1:200)]
melt_gavi73_draws <- melt(gavi73_draws, id.vars = c("gavi73", "decade"))
gavi73_summary <- melt_gavi73_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = .(gavi73, decade)]
full_dt <- data.table(expand.grid(gavi73 = unique(gavi73_summary$gavi73), decade = unique(gavi73_summary$decade)))
gavi73_summary <- merge(full_dt, gavi73_summary, by = c("gavi73", "decade"), all.x = T)
gavi73_summary[is.na(mean), c("mean", "lower", "upper") := 0]
gavi73_summary[, column_text := paste0(round(mean  / 1e5, 1), " (", round(lower / 1e5, 1), "-", round(upper / 1e5, 1), ")")]
cast_gavi73_summary <- dcast(gavi73_summary, gavi73 ~ decade, value.var = "column_text")
write.csv(cast_gavi73_summary, "requests/pneumo/gavi73_decade_summary.csv", row.names = F)