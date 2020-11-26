### Fit a model for VIMC mortality reduction

## Pull in data
dt <- prep_vimc_rr_data()

pdf("plots/vimc_rr_scatters.pdf")
lapply(c("haqi", "sdi"), scatter_rr, dt = dt)
dev.off()

## Simple model for VIMC imputation
library(splines)
impute_vacc_rr <- function(vacc) {
    print(vacc)
    fit <- glm(
        rr ~ haqi + sdi + year + bs(age, knots = c(2, 5, 10, 25)),
        data = dt[vaccine_short == vacc],
        family = "binomial"
    )
    pred_dt <- merge(
        dt[is.na(rr), .(location_id, year, haqi, sdi)][, idx := .I],
        CJ(
            age = seq(
                min(dt[vaccine_short == vacc]$age),
                max(dt[vaccine_short == vacc]$age)
            ),
            idx = 1:nrow(dt[is.na(rr), .(location_id, year, haqi, sdi)])
        ),
        by = "idx"
    )[, idx := NULL]
    pred_dt[, pred := predict(fit, pred_dt)]
    pred_dt[, rr := exp(pred) / (exp(pred) + 1)]
    pred_dt[, pred := NULL]
    pred_dt[, vaccine_short := vacc]

    return(pred_dt[, .(location_id, age, year, vaccine_short, rr)])
}
pred_all <- rbindlist(
    lapply(unique(dt[!is.na(vaccine_short)]$vaccine_short), impute_vacc_rr)
)
rr_dt <- rbind(
    dt[!is.na(rr), .(location_id, age, year, vaccine_short, rr)],
    pred_all,
    fill = T
)
rr_dt[, averted_scen := vimc_averted_scen(deaths_obs, coverage, rr)]

length(which(is.na(dt$rr)))

## Pull in GBD data
gbd_dt <- prep_gbd_data()


## Project mortality rates conditional on future coverage and covariates
prep_mx <- function(coverage, fit, covariates) {
    #TODO: Figure out how to translate coverage, model, and covariates into mx
    mx <- NULL
    return(mx)
}