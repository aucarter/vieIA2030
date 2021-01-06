yov_dt <- as.data.table(
    readRDS(("supp_data/interim-update-202007wue_summary.rds"))
)
yov_dt <- yov_dt[touchstone == "201910gavi",
    .(country, year, vaccine, model, activity_type, coverage, fvps, 
    deaths_averted, deaths_averted_rate)]
