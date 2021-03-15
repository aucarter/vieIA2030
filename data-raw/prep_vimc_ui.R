cross_all_2019_iso <- readRDS("supp_data/cross_all_2019_iso.rds")
vimc_ui <- cross_all_2019_iso %>%
  filter(time %in% as.character(2000:2030)) %>%
  select(
    disease, location_iso3 = country, year = time, contains("deaths_impact")
  ) %>%
  mutate(
    year = as.integer(year), 
    iqr = deaths_impact_q3 - deaths_impact_q1,
    sd = iqr / diff(qnorm(c(0.25, 0.75)))
  )
usethis::use_data(vimc_ui, overwrite = TRUE)