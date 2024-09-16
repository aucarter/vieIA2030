dt <- fread("results_22b/updated_reference_results_22b.csv")
out_dt <- dt[year == 2022, .(location_iso3, disease, observed_fvps, observed_deaths_averted)]
out_dt <- out_dt[, .(fvps = sum(observed_fvps, na.rm = T), deaths_averted = sum(observed_deaths_averted, na.rm = T)), by = .(location_iso3, disease)]
write.csv(out_dt, "~/Downloads/ia2030_financing_dashboard.csv", row.names = F)
