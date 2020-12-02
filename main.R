### Fit a model for VIMC mortality reduction

## Build database if necessary
gen_db()

pred_all <- impute_rr()
pred_all[, sq_error := (pred_rr - rr)**2]
pred_all[is.na(rr), rr := pred_rr]
pred_all[, averted := get_averted_scen(deaths_obs, coverage, rr)]
pred_all[!is.na(vaccine_deaths_averted), averted := vaccine_deaths_averted]
pred_all[, averted_diff := abs(vaccine_deaths_averted - averted)]
vacc_year_dt <- pred_all[, .(averted = sum(averted, na.rm = T)), by = .(vaccine_short, year)] 
my_colors <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue"))
gg <- ggplot(vacc_year_dt[year %in% 2000:2019], aes(x = year, y = averted, fill = vaccine_short)) +
    geom_area(color = "white", alpha = 0.8) +
    scale_fill_manual(values = my_colors) +
    theme_bw()
gg
# Note these are all spots where the RR was less than 0
View(pred_all[!is.na(averted) & averted_diff > 1e-3])

## Project mortality rates conditional on future coverage and covariates
prep_mx <- function(coverage, fit, covariates) {
    #TODO: Figure out how to translate coverage, model, and covariates into mx
    mx <- NULL
    return(mx)
}