
vimc_pth = system.file("extdata", "vimc_estimates.rds", package = "vieIA2030")

# Prep VIMC vaccine impact estimates
vimc_dt = readRDS(vimc_pth) %>%
  left_join(y  = d_v_at_table, 
            by = c("disease", "vaccine", "activity_type")) %>%
  select(d_v_at_id, country, year, age, deaths_averted) %>%
  arrange(country, d_v_at_id, age, year)

saveRDS(vimc_dt, "inst/shiny/vimc.rds")

# Upload file to database
upload_object(vimc_dt, "vimc_impact")

