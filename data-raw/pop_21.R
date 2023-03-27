# Read in new WPP
pop_path <- "supp_data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv"
pop_path2 <- "supp_data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv"
pop_21 <- fread(pop_path)[Time >= 2000] %>%
    rbind(fread(pop_path2)) %>%
    select(ISO3_code,  Time, AgeGrp, PopTotal, PopFemale, PopMale) %>%
    rename(location_iso3 = ISO3_code, year = Time, age = AgeGrp, pop = PopTotal, 
        female_pop = PopFemale, male_pop = PopMale) %>%
    mutate(age = as.integer(gsub("\\+", "", age)), pop = pop * 1000, 
        female_pop = female_pop * 1000, male_pop = male_pop * 1000) %>%
    melt(id.vars = c("location_iso3", "year", "age"), value.name = "pop") %>%
    mutate(sex_id = case_when(
        variable == "male_pop" ~ 1,
        variable == "female_pop" ~ 2,
        variable == "pop" ~ 3
    )) %>%
    filter(location_iso3 %in% loc_table$location_iso3) %>%
    select(-variable)

usethis::use_data(pop_21, overwrite = TRUE)