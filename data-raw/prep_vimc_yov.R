yov_dt <- as.data.table(
    readRDS(("supp_data/interim-update-202007wue_summary.rds"))
)

# Subset to the correct touchstone
yov_dt <- yov_dt[touchstone == "201910gavi",
    .(country, year, vaccine, model, activity_type, coverage, fvps,
    deaths_averted, deaths_averted_rate)]

# Take the mean of the deaths_averted rate across models
yov_mean_dt <- yov_dt[,
    lapply(.SD, mean),
    by = .(country, year, vaccine, activity_type),
    .SDcols = c("coverage", "fvps", "deaths_averted", "deaths_averted_rate")
]

upload_object(yov_mean_dt, "vimc_yov_impact")
