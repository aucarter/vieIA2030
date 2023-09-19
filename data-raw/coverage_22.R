library(dplyr); library(data.table)
wuenic_2022_path <- "~/Downloads/coverage-data.xlsx"
unicef_2022_path <- "~/Downloads/hpv2022rev_ctry.xls" # This is just for HPV

# Prep non-special antigens
input_dt <- readxl::read_xlsx(wuenic_2022_path,  sheet = "Data")
coverage_dt <- input_dt %>%
    filter(COVERAGE_CATEGORY_DESCRIPTION == "WHO/UNICEF Estimates of National Immunization Coverage") %>%
    select(CODE, YEAR, ANTIGEN, COVERAGE) %>%
    rename(location_iso3 = CODE, year = YEAR, wuenic_name = ANTIGEN, coverage = COVERAGE) %>%
    mutate(observed_coverage = coverage / 100) %>%
    inner_join(wuenic21_vaccine_table, by = "wuenic_name") %>%
    select(-wuenic_name, -coverage) %>%
    as.data.table()

# MenA and JE (first pull doses and then official_coverage)
add_coverage_dt <- input_dt %>%
    filter(COVERAGE_CATEGORY_DESCRIPTION == "Official coverage") %>%
    select(CODE, YEAR, ANTIGEN, COVERAGE) %>%
    rename(location_iso3 = CODE, year = YEAR, wuenic_name = ANTIGEN, coverage = COVERAGE) %>%
    mutate(observed_coverage = coverage / 100) %>%
    filter(!is.na(observed_coverage)) %>%
    inner_join(wuenic21_vaccine_table[vaccine %in% c("JE", "MenA")], by = "wuenic_name") %>%
    select(-wuenic_name, -coverage) %>%
    as.data.table()

add_coverage_dt2 <- input_dt %>%
    filter(COVERAGE_CATEGORY_DESCRIPTION == "Administrative coverage") %>%
    select(CODE, YEAR, ANTIGEN, COVERAGE) %>%
    rename(location_iso3 = CODE, year = YEAR, wuenic_name = ANTIGEN, coverage = COVERAGE) %>%
    mutate(admin_coverage = coverage / 100) %>%
    filter(!is.na(admin_coverage)) %>%
    inner_join(wuenic21_vaccine_table[vaccine %in% c("JE", "MenA")], by = "wuenic_name") %>%
    select(-wuenic_name, -coverage) %>%
    as.data.table()

add_coverage_dt <- merge(
    add_coverage_dt,
    add_coverage_dt2,
    by = c("location_iso3", "year", "vaccine"),
    all = T
)
add_coverage_dt[is.na(observed_coverage), observed_coverage := admin_coverage]
add_coverage_dt[, admin_coverage := NULL]
add_coverage_dt[observed_coverage > 1, observed_coverage := 1]

# Pull in UNICEF version of the HPV coverage data
hpv_cov <- readxl::read_xls(unicef_2022_path) %>%
    filter(`vaccine_code` == "PRHPVC_F") %>%
    select(`iso3c`, year, value, `vaccine_code`)  %>%
    rename(location_iso3 = `iso3c`, wuenic_name = `vaccine_code`) %>%
    mutate(observed_coverage = value / 100) %>%
    inner_join(wuenic21_vaccine_table, by = "wuenic_name") %>%
    select(-wuenic_name, -value) %>%
    as.data.table()


coverage_22 <- rbindlist(list(coverage_dt, add_coverage_dt, hpv_cov), fill = T)
coverage_22 <- merge(coverage_22, v_at_table[activity_type == "routine"], all.x = T)

coverage_22 <- merge(coverage_22, loc_table[, .(location_iso3, location_id)], by = "location_iso3", all.x = T)

coverage_22 <- coverage_22[!is.na(observed_coverage)]

coverage_22[, fvps := NA]

load_tables("coverage")
age_sex_dt <- unique(coverage[, .(v_at_id, age, sex_id)])
age_sex_dt <- rbind(age_sex_dt, data.table(v_at_id = 23, age = 0, sex_id = 3))
coverage_22 <- merge(coverage_22, age_sex_dt, by = "v_at_id", all.x = T, allow.cartesian = T)

coverage_22[, c("location_iso3", "vaccine", "activity_type") := NULL]

coverage_22 <- coverage_22[!is.na(location_id)]

usethis::use_data(coverage_22, overwrite = TRUE)
