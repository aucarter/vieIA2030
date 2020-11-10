### Fit a model for VIMC mortality reduction

## Pull in data
dt <- prep_vimc_rr_data()

## Scatter against covariates
scatter_rr <- function(x_var) {
    gg <- ggplot(dt, aes(x = get(x_var), y = rr)) +
        geom_point(size = 0.1, alpha = 0.2) +
        facet_wrap(~vaccine_short, scales = "free_y") +
        theme_bw() +
        ggtitle(paste(x_var, "vs mortality reduction by vaccine")) +
        xlab(xvar)

    return(gg)
}

library(ggplot2)
pdf("plots/vimc_rr_scatters.pdf")
lapply(c("haqi", "sdi", "coverage"), scatter_rr)
dev.off()

## Simple model
library(lme4)
fit_vaccine_rr <- function(vacc) {
    fit <- lmer(
        rr ~ 1 + coverage + sdi + haqi + (1 | location_id),
        data = dt[vaccine_short == vacc]
    )
    coefs <- data.table(t(fixef(fit)))
    coefs[, vaccine_short := vacc]
    return(coefs)
}
fit_dt <- rbindlist(lapply(unique(dt$vaccine_short), fit_vaccine_rr))


## Project mortality rates conditional on future coverage and covariates
prep_mx <- function(coverage, fit, covariates) {
    mx <- NULL
    return(mx)
}