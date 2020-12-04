### Fit a model for VIMC mortality reduction

## Build database if necessary
gen_db()

## Predict all and investigate results
alpha_vals <- seq(0.1, 1, 0.1)
diag_list <- lapply(alpha_vals, function(a) {
    pred_all <- impute_rr(alpha = a)
    pred_all[, sq_error := (pred_rr - rr)**2]
    mse <- mean(pred_all$sq_error, na.rm = T)
    # pred_all[is.na(rr), rr := pred_rr]
    pred_all[, averted := get_averted_scen(deaths_obs, coverage, pred_rr, a)]
    # pred_all[!is.na(vaccine_deaths_averted), averted := vaccine_deaths_averted]
    pred_all[, averted_diff := abs(vaccine_deaths_averted - averted)]
    tot_averted_error <- sum(pred_all$averted_diff, na.rm = T)
    return(list(mse, tot_averted_error))
})
plot(alpha_vals, unlist(lapply(diag_list, `[[`, 1)))
plot(alpha_vals, unlist(lapply(diag_list, `[[`, 2)))

a <- 0.5
pred_all <- impute_rr(alpha = a, beta = 0.8)
pred_all[, sq_error := (pred_rr - rr)**2]
mse <- mean(pred_all$sq_error, na.rm = T)
# pred_all[is.na(rr), rr := pred_rr]
pred_all[, averted := get_averted_scen(deaths_obs, coverage, pred_rr, a)]
# pred_all[!is.na(vaccine_deaths_averted), averted := vaccine_deaths_averted]
pred_all[, averted_diff := abs(vaccine_deaths_averted - averted)]
tot_averted_error <- sum(pred_all$averted_diff, na.rm = T)


## Plot total deaths averted by vaccine over time
vacc_year_dt <- pred_all[, .(averted = sum(averted, na.rm = T)), by = .(vaccine_short, year)] 
my_colors <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue"))
gg <- ggplot(vacc_year_dt[year %in% 2000:2019], aes(x = year, y = averted, fill = vaccine_short)) +
    geom_area(color = "white", alpha = 0.8) +
    scale_fill_manual(values = my_colors) +
    theme_bw()
gg

# Note these are all spots where the RR was less than 0
View(pred_all[!is.na(averted) & averted_diff > 1e-3])

