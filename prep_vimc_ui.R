vimc_ui[, cv := ifelse(deaths_impact_mean == 0, 0, sd / deaths_impact_mean)]
load_tables(c("vimc_impact"))
vimc_impact <- merge(vimc_impact[deaths_averted != 0], d_v_at_table[, .(d_v_at_id, disease)])
tot_vimc_impact <- vimc_impact[, .(tot_deaths_averted = sum(abs(deaths_averted), na.rm = T)), by = .(disease, location_id, year)]
vimc_merged <- merge(
    vimc_impact,
    tot_vimc_impact,
    by = c("location_id", "disease", "year")
)

vimc_w_ui <- merge(
    vimc_merged, 
    vimc_ui[, .(location_id, disease, year, deaths_impact_mean, sd)], 
    by = c("location_id", "disease", "year"),
    all.x = T
)

vimc_w_ui[, prop := abs(deaths_averted) / tot_deaths_averted]
setnames(vimc_w_ui, "sd", "total_sd")
vimc_w_ui[, sd := prop * total_sd]
# This is for Rubella in 2000
if (unique(vimc_w_ui[is.na(sd)]$disease) != "Rubella" | 
    unique(vimc_w_ui[is.na(sd)]$year) != 2000) {
    stop("Check which SDs are NA")
} else {
    vimc_w_ui[is.na(sd), sd := abs(deaths_averted)]
}

vimc_draws <- gen_draws(vimc_w_ui[deaths_averted != 0]$deaths_averted, vimc_w_ui[deaths_averted != 0]$sd, 200)

vimc_draws <- merge(
    scenario_impact$dt[!(disease %in% efficacy_ui$disease)],
    vimc_ui[, .(disease, location_id, year, sd)],
    by = c("disease", "location_id", "year"),
    all.x = T
)
raking_summary <- summarize_raking(impact_dt)
vimc_draws <- merge(
    vimc_draws,
    raking_summary[, .(vaccine, cv)],
    by = "vaccine",
    all.x = T
)
vimc_draws[is.na(sd), sd := deaths_averted * cv]
vimc_draws_mat <- t(mapply(rnorm, n = 200,  mean = vimc_draws$deaths_averted, sd = vimc_draws$sd))
colnames(vimc_draws_mat) <- paste0("draw_", 1:200)


vimc_draws_wide <- cbind(
    vimc_draws[, .(disease, location_id, vaccine, activity_type, impact_factor, year, age, fvps, deaths_averted)],
    vimc_draws_mat
)
