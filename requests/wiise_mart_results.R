library(data.table)
dt <- fread("https://storage.googleapis.com/vie_ia2030/ia2030_reference_results.csv")
out_dt <- dt[, .(deaths_averted = sum(deaths_averted)), by = .(location_name, location_iso3, location_id, year, disease, vaccine)]
write.csv(out_dt, "results/IA2030_results_location_disease_year_deaths_averted.csv", row.names = F)
