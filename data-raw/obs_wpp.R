file <- system.file("extdata", "obs_wpp_demo.csv", package = "vieIA2030")
obs_wpp  <- fread(file) %>%
  filter(!(variant == "Medium variant" & year == 2020)) %>%
  select(-c(variant)) %>%
  filter(year %in% 1980:2095) %>%
  rename(wpp_location_code = location_code) %>%
  right_join(loc_table, by = "wpp_location_code") %>%
  select(-c(wpp_location_code)) %>%
  filter(!is.na(year)) %>%
  mutate(
    deaths_both = deaths_both * 1e03,
    deaths_male = deaths_male * 1e03,
    deaths_female = deaths_female * 1e03,
    births = births * 1e03
  ) %>%
  select(-c(gbd_alt_name, wpp_name, location_name, location_iso3))

mydb <- open_connection()
DBI::dbWriteTable(mydb, "obs_wpp", obs_wpp, overwrite = TRUE)
DBI::dbDisconnect(mydb)