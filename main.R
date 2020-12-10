### Fit a model for VIMC mortality reduction

devtools::load_all()
## Build database if necessary
gen_db()

## Predict all and investigate results
b <- 0.8
a <- 0.5
pred_all <- impute_rr(alpha = a, beta = b)

## Add future coverage by fixing at 2019 levels
last_dt <- copy(pred_all[year == 2019,
                         .(age, location_id, vaccine_short, coverage)])
future_dt <- rbindlist(lapply(2020:2030, function(y) {
    dt <- copy(last_dt)
    dt[, year := y]
}))
setnames(future_dt, "coverage", "scen_coverage")
scen_dt <- merge(pred_all, future_dt, 
    by = c("year", "age", "location_id", "vaccine_short"), all.x = T)
scen_dt[!is.na(scen_coverage), coverage := scen_coverage]
future_deaths <- all_deaths[year > 2019, .(deaths_obs_future = sum(deaths)),
                         by = .(age, year, location_id)]
scen_dt <- merge(scen_dt, future_deaths, 
    by = c("year", "age", "location_id"), all.x = T)
scen_dt[!is.na(deaths_obs_future), deaths_obs := deaths_obs_future]
scen_dt[, averted := get_averted_scen(deaths_obs, coverage, pred_rr, a)]
scen_dt[, vimc := ifelse(location_id %in% unique(vimc_impact$location_id), 1, 0)]
scen_dt[, gbd := ifelse(vaccine_short %in% c("D", "T", "P", "BCG"), 1, 0)]
scen_dt[vimc == 1 & gbd == 0, label := "VIMC10 (VIMC locations)"]
scen_dt[vimc == 0 & gbd == 0, label := "Imputed VIMC10 (non-VIMC locations)"]
scen_dt[gbd == 1, label := "GBD4 (All locations)"]

## Plot total deaths averted by vaccine over time
vacc_year_dt <- scen_dt[, .(averted = sum(averted, na.rm = T)), by = .(vaccine_short, year)] 
my_colors1 <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue"))
my_colors2<- rev(RColorBrewer::brewer.pal(name = "Paired", n = 3))
## By vaccine
gg <- ggplot(vacc_year_dt[year %in% 2000:2030], aes(x = year, y = averted / 1e6, fill = vaccine_short)) +
    geom_area(color = "white", alpha = 0.8) +
    scale_fill_manual(values = my_colors1, name = "Vaccine") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") 
gg

## Plot with VIMC, non-VIMC, and GBD
label_year_dt <- scen_dt[, .(averted = sum(averted, na.rm = T)), by = .(label, year)] 
gg <- ggplot(label_year_dt[year %in% 2000:2030], aes(x = year, y = averted / 1e6, fill = label)) +
    geom_area(color = "white", alpha = 0.8) +
    scale_fill_manual(values = my_colors2, name = "") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") 
gg

## Plot all locations
scen_dt <- merge(scen_dt, loc_table[, .(location_id, location_name)], by = "location_id")
loc_vacc_year_dt <- scen_dt[, .(averted = sum(averted, na.rm = T)), by = .(location_name, vaccine_short, year)] 
loc_label_year_dt <- scen_dt[, .(averted = sum(averted, na.rm = T)), by = .(location_name, label, year)] 
pdf("plots/deaths_averted_by_location.pdf", height = 8, width = 10)
for(loc in sort(unique(loc_vacc_year_dt$location_name))) {
    plot_dt1 <- loc_vacc_year_dt[location_name == loc]
    plot_dt2 <- loc_label_year_dt[location_name == loc]
    ## By vaccine
    gg <- ggplot(plot_dt1[year %in% 2000:2030], aes(x = year, y = averted , fill = vaccine_short)) +
        geom_area(color = "white", alpha = 0.8) +
        scale_fill_manual(values = my_colors1, name = "Vaccine") +
        theme_bw() + xlab("Year") + ylab("Deaths averted") +
        ggtitle(loc)
    print(gg)

    ## Plot with VIMC, non-VIMC, and GBD
    gg <- ggplot(plot_dt2[year %in% 2000:2030], aes(x = year, y = averted, fill = label)) +
        geom_area(color = "white", alpha = 0.8) +
        scale_fill_manual(values = my_colors2, name = "") +
        theme_bw() + xlab("Year") + ylab("Deaths averted") +
        ggtitle(loc)
    print(gg)
}
dev.off()

## VIMC locations map
map_locations(unique(vimc_impact$location_iso3), "VIMC Locations")

## Optimal alpha investigation
alpha_vals <- seq(0.1, 1, 0.1)
plot_alpha <- function(alpha_vals) {
    dt <- data.table(expand.grid(x = seq(0, 1, 0.01), alpha = alpha_vals))
    dt[, y := x ** alpha]
    gg <- ggplot(dt, aes(x = x, y = y, color = as.factor(alpha))) + 
        geom_line() + theme_bw() + coord_equal()
    print(gg)
}
plot_alpha(alpha_vals)
diag_list <- lapply(alpha_vals, function(a) {
    pred_all <- impute_rr(alpha = a, beta = b)
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
plot(alpha_vals, unlist(lapply(diag_list, `[[`, 2)), type = "l", 
    xlab = "alpha", ylab = "Total error")
