get_x <- function(fl, rn, sx, my) {
  if (rn == 1) {
    sh <- "ESTIMATES"
    rh <- "A17:DE18122"
  } else {
    sh <- "MEDIUM VARIANT"
    rh <- "A17:DE20672"
  }

  x <- readxl::read_excel(fl, sheet = sh, range = rh) %>%
    mutate(sex_name = sx) %>%
    select(
      -c("Index", "Variant", "Region, subregion, country or area *", "Notes",
          "Type", "Parent code")
    ) %>%
    rename(
      year_id = `Reference date (as of 1 July)`,
      location_code = `Country code`
    ) %>%
    gather(age, val, -location_code, -sex_name, -year_id) %>%
    filter(year_id > 1979 & year_id < my) %>%
    mutate(age = as.numeric(age), val = val * 1000) %>%
    group_by(location_code, year_id, sex_name, age) %>%
    summarise(nx = sum(val)) %>%
    ungroup()

  return(x)
}

get_pop <- function(f1, f2) {
  pm1 <- get_x(f1, 1, "Male", 2020)
  pm2 <- get_x(f1, 2, "Male", 2097)
  pf1 <- get_x(f2, 1, "Female", 2020)
  pf2 <- get_x(f2, 2, "Female", 2097)
  wpppop <- rbind(pm1, pm2, pf1, pf2) %>%
    rename(wpp_location_code = location_code)

  return(wpppop)
}

f1 <- "supp_data/WPP_input_pop/WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"
f2 <- "supp_data/WPP_input_pop/WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"

wpp_input_pop <- get_pop(f1, f2) %>%
    left_join(
        data.table(sex_id = 1:2, sex_name = c("Male", "Female")),
        by = "sex_name"
    ) %>%
    select(-c("sex_name"))


save(wpp_input_pop, file = "inst/extdata/wpp_input_pop.RData")
