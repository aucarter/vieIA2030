load_tables("gbd_strata_deaths_ui")

gbd_draws <- gen_draws(gbd_strata_deaths_ui$value, gbd_strata_deaths_ui$sd, 200)

gbd_draws_wide <- cbind(
    gbd_strata_deaths_ui[, .(location_id, disease, sex_id, age, year)],
    gbd_draws
)

saveRDS(gbd_draws_wide, "temp/gbd_draws.rds")
