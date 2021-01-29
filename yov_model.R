yov_dt <- load_tables("vimc_yov_impact")[[1]]
model_dt <- merge(yov_dt, gbd_cov, by = c("location_id", "year"))
vacc <- "Measles"
vacc_dt <- model_dt[vaccine == vacc]
fit <- lm(deaths_averted_rate ~ haqi + sdi, data = vacc_dt)
summary(fit)
