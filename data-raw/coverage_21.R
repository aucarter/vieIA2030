library(dplyr); library(data.table)
wuenic_2021_path <- "supp_data/coverage--2021.xlsx"
unicef_2021_path <- "supp_data/wuenic2021rev_hpv-estimates.xlsx" # This is just for HPV

# Prep non-special antigens
input_dt <- readxl::read_xlsx(wuenic_2021_path,  sheet = "Data")
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
hpv_cov <- readxl::read_xlsx(unicef_2021_path,  sheet = "hpv-estimates") %>%
    filter(`vaccine-code` == "PRHPVC_F") %>%
    select(`iso-code`, year, coverage, `vaccine-code`)  %>%
    rename(location_iso3 = `iso-code`, wuenic_name = `vaccine-code`) %>%
    mutate(observed_coverage = coverage / 100) %>%
    inner_join(wuenic21_vaccine_table, by = "wuenic_name") %>%
    select(-wuenic_name, -coverage) %>%
    as.data.table()


coverage_21 <- rbindlist(list(coverage_dt, add_coverage_dt, hpv_cov), fill = T)
coverage_21 <- merge(coverage_21, v_at_table[activity_type == "routine"], all.x = T)

coverage_21 <- merge(coverage_21, loc_table[, .(location_iso3, location_id)], by = "location_iso3", all.x = T)

coverage_21 <- coverage_21[!is.na(observed_coverage)]

coverage_21[, fvps := NA]

load_tables("coverage")
coverage_21 <- merge(coverage_21, unique(coverage[, .(v_at_id, age, sex_id)]), by = "v_at_id")

coverage_21[, c("location_iso3", "vaccine", "activity_type") := NULL]

usethis::use_data(coverage_21, overwrite = TRUE)
