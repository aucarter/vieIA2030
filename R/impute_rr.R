impute_vacc_rr <- function(vacc, dt) {
    print(vacc)
    fit <- glm(
        rr ~ haqi + sdi + year + splines::bs(age, knots = c(2, 5, 10, 25)),
        data = dt[vaccine_short == vacc],
        family = "binomial"
    )
    forecast_cov <- forecast_gbd_cov()
    pred_dt <- merge(
        forecast_cov[, idx := .I],
        CJ(
            age = seq(
                min(dt[vaccine_short == vacc]$age),
                max(dt[vaccine_short == vacc]$age)
            ),
            idx = 1:nrow(forecast_cov)
        ),
        by = "idx"
    )[, idx := NULL]
    pred_dt[, pred := predict(fit, pred_dt)]
    pred_dt[, pred_rr := exp(pred) / (exp(pred) + 1)]
    pred_dt[, pred := NULL]
    pred_dt[, vaccine_short := vacc]
    out_dt <- merge(
        pred_dt,
        dt[, .(location_id, age, year, vaccine_short, rr, coverage, deaths_obs,
               vaccine_deaths_averted)],
        all.x = T
    )

    return(out_dt)
}

impute_rr <- function() {
    ## Pull in VIMC data
    vimc_dt <- prep_vimc_rr_data()

    ## Pull in GBD data
    gbd_dt <- prep_gbd_rr_data()

    dt <- rbind(vimc_dt, gbd_dt, fill = T)

    ## Simple model for VIMC imputation
    pred_all <- rbindlist(
        lapply(
            unique(dt[!is.na(vaccine_short)]$vaccine_short),
            impute_vacc_rr,
            dt
        )
    )

    return(pred_all)
}
