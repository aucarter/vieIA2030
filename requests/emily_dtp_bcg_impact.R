### Fit a model for VIMC mortality reduction
devtools::load_all()

## Predict all and plot results
params <- jsonlite::fromJSON("params.json")
pred_all <- impute_all_rr(params, routine_only = T)
fit_summary <- summarize_fit(pred_all)
plot_strata_fit(pred_all)

## Calculate impact factors and rake to VIMC
impact_factors <- calc_impact_factors(pred_all)
impact_dt <- rake_impact(impact_factors)

## FVPs
total_fvps <- coverage[year %in% 2000:2019, .(total_fvps = sum(fvps,  na.rm = T)),
    by = .(location_id, v_at_id)]

## Subset to DTP and BCG
out_dt <- impact_dt[disease %in% c("D", "T", "P", "TB"), .(location_id, v_at_id, disease, impact_factor)]
out_dt <- merge(out_dt, total_fvps, by = c("location_id", "v_at_id"))
out_dt[, v_at_id := NULL]
out_dt <- merge(loc_table[, .(location_iso3, location_id)], out_dt, by = "location_id")
out_dt[, location_id := NULL]
out_dt[, total_deaths_averted := total_fvps * impact_factor]

## Merge on missing locs
full_table <- data.table(expand.grid(
    location_iso3 = loc_table$location_iso3,
    disease = c("D", "T", "P", "TB")
))
out_dt <- merge(full_table, out_dt, by = c("location_iso3", "disease"), all.x = T)
out_dt[is.na(total_fvps), total_fvps := 0]
out_dt[is.na(total_deaths_averted), total_deaths_averted := 0]

## Save
write.csv(out_dt, "requests/dtp_bcg_impact.csv", row.names = F)
