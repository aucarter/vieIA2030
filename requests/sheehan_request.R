library(data.table)
dt <- fread("outputs/detailed_results.csv")
dt <- dt[order(disease)]
dt[, disease := as.factor(disease)]
disease_year_dt <- dt[, .(deaths_averted = sum(total, na.rm = T)), by = .(disease, year)] 
load("data/disease_table.rda")
out_dt <- merge(disease_table[, .(disease, disease_long)], disease_year_dt)
out_dt[, disease := NULL]
setnames(out_dt, "disease_long", "disease")
write.csv(out_dt, "requests/global_yov_by_disease.csv", row.names = F)
