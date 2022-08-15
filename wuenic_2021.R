wuenic_2021_path <- "supp_data/coverage--2021.xlsx"
unicef_2021_path <- "supp_data/wuenic2021rev_hpv-estimates.xlsx" # This is just for HPV

# Prep non-special antigens
input_dt <- readxl::read_xlsx(wuenic_2021_path,  sheet = "Data")
coverage_dt <- input_dt %>%
    filter(COVERAGE_CATEGORY_DESCRIPTION == "WHO/UNICEF Estimates of National Immunization Coverage") %>%
    select(CODE, YEAR, ANTIGEN, COVERAGE) %>%
    rename(location_iso3 = CODE, year = YEAR, wuenic_name = ANTIGEN, coverage = COVERAGE) %>%
    mutate(observed_coverage = coverage / 100) %>%
    filter(year > 2018) %>%
    inner_join(wuenic21_vaccine_table, by = "wuenic_name") %>%
    select(-wuenic_name, -coverage) %>%
    as.data.table()

# MenA and JE (first pull doses and then official_coverage)
special_antigens <- c("JE", "MenA")
add_coverage_dt <- input_dt %>%
    filter(COVERAGE_CATEGORY_DESCRIPTION == "Official coverage") %>%
    select(CODE, YEAR, ANTIGEN, COVERAGE) %>%
    rename(location_iso3 = CODE, year = YEAR, wuenic_name = ANTIGEN, coverage = COVERAGE) %>%
    mutate(observed_coverage = coverage / 100) %>%
    filter(year > 2018 & !is.na(observed_coverage)) %>%
    inner_join(wuenic21_vaccine_table[vaccine %in% special_antigens], by = "wuenic_name") %>%
    select(-wuenic_name, -coverage) %>%
    as.data.table()


# Pull in UNICEF version of the HPV coverage data
hpv_cov <- readxl::read_xlsx(unicef_2021_path,  sheet = "hpv-estimates") %>%
    filter(`vaccine-code` == "PRHPVC_F") %>%
    select(`iso-code`, year, coverage, `vaccine-code`)  %>%
    rename(location_iso3 = `iso-code`, wuenic_name = `vaccine-code`) %>%
    mutate(observed_coverage = coverage / 100) %>%
    filter(year > 2018) %>%
    inner_join(wuenic21_vaccine_table, by = "wuenic_name") %>%
    select(-wuenic_name, -coverage) %>%
    as.data.table()


coverage_dt <- rbindlist(list(coverage_dt, add_coverage_dt, hpv_cov), fill = T)

results_dt <- fread("https://storage.googleapis.com/vie_ia2030/ia2030_reference_results.csv")
hpv_results <- fread("supp_data/hpv_fix.csv")
hpv_results <- merge(hpv_results, loc_table[, .(location_id, location_iso3)], by = "location_id")

results_dt <- rbind(
    results_dt[!(vaccine == "HPV" & activity_type == "routine")],
    hpv_results,
    fill = T
)

dt <- merge(
    coverage_dt[!is.na(observed_coverage)], 
    results_dt[year %in% 2019:2021, .(location_iso3, year, disease, vaccine, age, impact_factor, fvps, deaths_averted, cohort_size)], 
    by = c("location_iso3", "year", "vaccine"),
    all.y = T)
dt[location_iso3 %in% special_antigens, cohort_size := target_pop]
dt[, observed_fvps := observed_coverage * cohort_size]
dt[, observed_deaths_averted := impact_factor * observed_fvps]
dt[, lapply(.SD, sum, na.rm = T), by = .(year), .SDcols = c("deaths_averted", "observed_deaths_averted")]

# Read in new WPP
pop_path <- "supp_data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv"
pop_dt <- fread(pop_path)[Time %in% 2019:2021] %>%
    select(ISO3_code,  Time, AgeGrp, PopTotal) %>%
    rename(location_iso3 = ISO3_code, year = Time, age = AgeGrp, new_pop = PopTotal) %>%
    mutate(age = as.integer(gsub("\\+", "", age)), new_pop = new_pop * 1000)

dt <- merge(dt, pop_dt, by = c("location_iso3", "year", "age"), all.x = T)

setnames(dt, c("fvps", "deaths_averted"), c("target_fvps", "target_deaths_averted"))

dt[location_iso3 %in% special_antigens, new_pop := target_pop]
dt[, target_coverage := target_fvps / cohort_size]
dt[, new_pop_target_fvps := new_pop * target_coverage]
dt[, new_pop_observed_fvps := new_pop * observed_coverage]
dt[, new_pop_target_deaths_averted := new_pop_target_fvps * impact_factor]
dt[, new_pop_observed_deaths_averted := new_pop_observed_fvps * impact_factor]
setnames(dt, "cohort_size", "old_pop")

# Reset 2019
dt[year == 2019, new_pop_observed_fvps := new_pop_target_fvps]
dt[year == 2019, new_pop_observed_deaths_averted := new_pop_target_deaths_averted]

out_dt <- dt[, .(location_iso3, year, age, vaccine, disease, target_coverage, 
    observed_coverage, old_pop, new_pop, target_fvps, observed_fvps, 
    target_deaths_averted, observed_deaths_averted, new_pop_target_fvps,
    new_pop_observed_fvps, new_pop_target_deaths_averted, 
    new_pop_observed_deaths_averted)]

out_dt <- merge(out_dt, loc_table[, .(location_iso3, location_name, region, income_group)])

write.csv(out_dt, "results/updated_reference_results_21.csv", row.names = F)
