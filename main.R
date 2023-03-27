### Fit a model for VIMC mortality reduction
devtools::load_all()
## Predict all and plot results
params <- jsonlite::fromJSON("params.json")

# LHS
n_draws <- 200
input_names <- c("efficacy", "strata_deaths_averted", "haqi", "strata_deaths",
    "betas")
draw_idx <- data.table::data.table(
    round(lhs::randomLHS(n_draws, length(input_names)) * n_draws + 0.5)
)
names(draw_idx) <- input_names
run_num <- 1:n_draws
# TODO: Make this a function rather than putting into global env
draw_idx <<- cbind(run_num, draw_idx)

vimc_draws_wide <- readRDS("temp/vimc_draws.rds")

for (i in 89:max(draw_idx$run_num)) {
    message("Run #:", i)
    pred_all <- impute_all_rr(params, routine_only = T, run = i)
}

# fit_summary <- summarize_fit(pred_all)
# plot_strata_fit(pred_all)

## Calculate impact factors and rake to VIMC
pred_all <- rbindlist(
    parallel::mclapply(list.files("averted_pred", pattern = "rds"), function(f) {
        split <- strsplit(f, "_")[[1]]
        id <- as.integer(split[1])
        print(id)
        draw <- as.integer(gsub(".rds", "", split[2]))
        dt <- readRDS(file.path("averted_pred", f))
        dt <- dt[(year - age) %in% 2000:2030, .(total_averted = sum(averted, na.rm = T)), 
            by = .(location_id)]
        dt[, draw := draw]
        dt[, d_v_at_id := id]
        return(dt[, .(location_id, draw, d_v_at_id, total_averted)])
    }, mc.cores = parallel::detectCores())
, fill = T)

impact_factors <- calc_impact_factors(pred_all)
impact_dt <- rake_impact(impact_factors)

## Calculate scenario impact 
scenario_dt <- get_scen_fvps()
scenario_impact <- calc_scenario_impact(scenario_dt, impact_dt)

## Generate draws
# Efficacy
efficacy_ui
efficacy_draws <- merge(efficacy_ui, scenario_impact$dt, by = "disease", allow.cartesian = T)
efficacy_draws[, deaths_averted_draw := deaths_averted * scalar]
efficacy_draws[, scalar := NULL]
efficacy_draws_wide <- dcast(efficacy_draws, ... ~ draw, value.var = "deaths_averted_draw")

# VIMC
vimc_ui
vimc_draws <- merge(
    scenario_impact$dt[!(disease %in% efficacy_ui$disease)],
    vimc_ui[, .(disease, location_id, year, sd)],
    by = c("disease", "location_id", "year"),
    all.x = T
)
raking_summary <- summarize_raking(impact_dt)
vimc_draws <- merge(
    vimc_draws,
    raking_summary[, .(vaccine, cv)],
    by = "vaccine",
    all.x = T
)
vimc_draws[is.na(sd), sd := deaths_averted * cv]
vimc_draws_mat <- t(mapply(rnorm, n = 200,  mean = vimc_draws$deaths_averted, sd = vimc_draws$sd))
colnames(vimc_draws_mat) <- paste0("draw_", 1:200)
vimc_draws_wide <- cbind(
    vimc_draws[, .(disease, location_id, vaccine, activity_type, impact_factor, year, age, fvps, deaths_averted)],
    vimc_draws_mat
)

# Combine and rescale to the mean
draws_dt <- rbind(efficacy_draws_wide, vimc_draws_wide)
draws_dt[, draw_mean := rowMeans(draws_dt[, grep("draw", names(draws_dt), value = T), with = F])]
draws_dt[, mean_diff := deaths_averted - draw_mean]
draws_dt[, grep("draw", names(draws_dt), value = T) := draws_dt[, grep("draw", names(draws_dt), value = T), with = F] + mean_diff]
draws_dt[, c("draw_mean", "mean_diff") := NULL]

year_total_draws <- draws_dt[, lapply(.SD, sum), by = .(year), .SDcols = paste0("draw_", 1:200)]
melt_year_total_draws <- melt(year_total_draws, id.vars = "year")
year_total_summary <- melt_year_total_draws[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = year]
year_total_summary
scenario_impact$year_totals


## Save for sharing
out_dt <- merge(
    draws_dt,
    loc_table[, .(location_id, location_name, location_iso3, region, income_group, gavi73)],
    by = "location_id"
)
out_dt <- out_dt[order(location_name, disease, vaccine, activity_type)]
out_dt[is.na(deaths_averted), deaths_averted := 0]

cohort_dt <- wpp_input[age == 0, .(nx = sum(nx)), by = .(location_id, year)]
setnames(cohort_dt, "nx", "cohort_size")
out_dt <- merge(out_dt, cohort_dt, by = c("location_id", "year"))

total_dt <- wpp_input[, .(nx = sum(nx)), by = .(location_id, year)]
setnames(total_dt, "nx", "total_pop")
out_dt <- merge(out_dt, total_dt, by = c("location_id", "year"))
setcolorder(
    out_dt, 
    c(
        "location_name", "location_iso3", "location_id", "year", "disease", 
        "vaccine", "activity_type", "age", "impact_factor", "fvps", "deaths_averted",
        "region", "income_group", "gavi73", "cohort_size", "total_pop", paste0("draw_", 1:200)
    )
)
write.csv(out_dt, "outputs/reference_results.csv", row.names = F)

upload_object(out_dt, "ia2030_reference_results")

## Impact by region
no_lin_range_cov <- gen_ia2030_goals(ia2030_dtp_goal, linear = F, no_covid_effect = 2022, 
    intro_year = 2025, intro_range = T)
scen_dt <- merge(
    no_lin_range_cov,
    loc_table[, .(location_id, location_name, location_iso3)],
    by = "location_id"
)
scen_dt <- merge(scen_dt, v_at_table, by = "v_at_id")
write.csv(scen_dt, "outputs/ia2030_cov_trajectories.csv", row.names = F)
dt <- scenario_impact$dt
dt <- merge(dt, loc_table[, .(location_id, region)], by = "location_id")
baseline_dt <- dt[year == 2019]
setnames(baseline_dt, "deaths_averted", "baseline_deaths_averted")
baseline_dt[, year := NULL]
table_dt <- merge(dt[year %in% 2021:2030], baseline_dt,
    by = c("location_id", "region", "disease", "vaccine", "activity_type"),
    all.x = T)
table_dt[is.na(baseline_deaths_averted), baseline_deaths_averted := 0]
table_dt[is.na(deaths_averted), deaths_averted := 0]
table_dt[activity_type %in% c("routine", "combined"), 
    incremental := deaths_averted - baseline_deaths_averted]
table_totals <- table_dt[, .(total = sum(deaths_averted, na.rm = T),
    incremental = sum(incremental, na.rm = T)), by = .(region, disease, year)][
        order(region, disease, year)
    ]
write.csv(table_totals, "outputs/detailed_results.csv", row.names = F)

## 
dt <- scenario_impact$dt
dt <- merge(dt, loc_table[, .(location_id, income_group)], by = "location_id")
global_dt <- dt[, .(deaths_averted = sum(deaths_averted, na.rm = T)), by = .(year, disease, vaccine, activity_type)]
global_dt[, income_group := "Global"]
income_group_dt <- dt[, .(deaths_averted = sum(deaths_averted, na.rm = T)), by = .(income_group, year, disease, vaccine, activity_type)]
all_dt <- rbind(global_dt, income_group_dt)
baseline_dt <- all_dt[year == 2019]
setnames(baseline_dt, "deaths_averted", "baseline_deaths_averted")
baseline_dt[, year := NULL]
table_dt <- merge(all_dt[year %in% 2021:2030], baseline_dt,
    by = c("income_group", "disease", "vaccine", "activity_type"), all.x = T)
table_dt[, incremental := deaths_averted - baseline_deaths_averted]
table_totals <- table_dt[, .(total = sum(deaths_averted, na.rm = T) / 1e5,
    incremental = sum(incremental, na.rm = T) / 1e5), by = income_group]
write.csv(table_totals, "outputs/results_table_income.csv", row.names = F)


## Impact by income
dt <- scenario_impact$dt
dt <- merge(dt, loc_table[, .(location_id, income_group)], by = "location_id")
baseline_dt <- dt[year == 2019]
setnames(baseline_dt, "deaths_averted", "baseline_deaths_averted")
baseline_dt[, year := NULL]
table_dt <- merge(dt[year %in% 2021:2030], baseline_dt,
    by = c("location_id", "income_group", "disease", "vaccine", "activity_type"),
    all.x = T)
table_dt[is.na(baseline_deaths_averted), baseline_deaths_averted := 0]
table_dt[is.na(deaths_averted), deaths_averted := 0]
table_dt[activity_type %in% c("routine", "combined"), 
    incremental := deaths_averted - baseline_deaths_averted]
table_totals <- table_dt[, .(total = sum(deaths_averted, na.rm = T),
    incremental = sum(incremental, na.rm = T)), by = .(income_group, disease, year)][
        order(income_group, disease, year)
    ]
write.csv(table_totals, "outputs/detailed_results_income.csv", row.names = F)

