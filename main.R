### Fit a model for VIMC mortality reduction

## Build database if necessary
gen_db()

## Pull in VIMC data
dt <- prep_vimc_rr_data()

## Simple model for VIMC imputation
pred_all <- rbindlist(
    lapply(
        unique(dt[!is.na(vaccine_short)]$vaccine_short),
        vimc_impute_vacc_rr,
        dt
    )
)
pred_all[, sq_error := (pred_rr - rr)**2]
pred_all[is.na(rr), rr := pred_rr]
pred_all[, averted := vimc_averted_scen(deaths_obs, coverage, rr)]
pred_all[, averted_diff := abs(vaccine_deaths_averted - averted)]

# Note these are all spots where the RR was less than 0
View(pred_all[!is.na(averted) & averted_diff > 1e-3])

## Project mortality rates conditional on future coverage and covariates
prep_mx <- function(coverage, fit, covariates) {
    #TODO: Figure out how to translate coverage, model, and covariates into mx
    mx <- NULL
    return(mx)
}