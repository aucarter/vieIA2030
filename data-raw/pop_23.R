# Read in new WPP
pop_path <- "supp_data/data_export_WUENIC_REF_POPULATIONS.csv"

pop_23 <- fread(pop_path) %>%
    filter(YEAR >= 2000) %>%
    select(COUNTRY,  YEAR, AGE_GROUP, VALUE_RAW, GENDER) %>%
    rename(location_iso3 = COUNTRY, year = YEAR, age = AGE_GROUP, pop = VALUE_RAW, 
        sex = GENDER) %>%
    filter(grepl("Y", age)) %>%
    mutate(age = as.integer(gsub("Y", "", age))) %>%
    mutate(sex_id = case_when(
        sex == "MALE" ~ 1,
        sex == "FEMALE" ~ 2,
        sex == "BOTH" ~ 3
    )) %>%
    filter(location_iso3 %in% loc_table$location_iso3) %>%
    select(-sex)

usethis::use_data(pop_23, overwrite = TRUE)
