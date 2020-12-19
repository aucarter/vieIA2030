### Fit a model for VIMC mortality reduction

devtools::load_all()
## Build database if necessary
gen_db()

## Predict all and investigate results
b <- 0.8
a <- 0.8
pred_all <- impute_rr(alpha = a, beta = b)
pred_all[, averted := get_averted_scen(deaths_obs, coverage, pred_rr, a)]
pred_all[, error := vaccine_deaths_averted - averted]

loc_dt <- pred_all[location_id == 126 &
    !(vaccine_short %in% c("D", "T", "P", "BCG"))
]
loc_dt[, vaccine_deaths_averted := as.numeric(vaccine_deaths_averted)]
melt_dt <- melt(
    loc_dt[, .(year, age, vaccine_short, coverage, vaccine_deaths_averted, averted)],
    id.vars = c("year", "age", "vaccine_short", "coverage")
)
gg <- ggplot(melt_dt, aes(x = coverage, color = variable, y = value)) +
    geom_point(size = 0.1) +
    facet_wrap(~vaccine_short, scales = "free_y")
gg

gg <- ggplot(loc_dt, aes(x = vaccine_deaths_averted, y = averted, color = age + 1)) +
    geom_point() + facet_wrap(~vaccine_short, scales = "free") +
            viridis::scale_color_viridis(
            option = "viridis", 
            direction = -1, 
            trans = "log10") +
    geom_abline(slope = 1) + expand_limits(x = 0)
gg
pdf("plots/vacc_fit.pdf")
for(v in unique(pred_all[!(vaccine_short %in% c("D", "T", "P", "BCG"))]$vaccine_short)) {
    print(v)
    plot_dt <- pred_all[vaccine_short == v & vaccine_deaths_averted > 0 & averted > 0]
    min_val <- min(c(plot_dt$vaccine_deaths_averted, plot_dt$averted))
    gg <- ggplot(plot_dt, aes(x = vaccine_deaths_averted, y = averted, color = age + 1)) +
        geom_point() +
                viridis::scale_color_viridis(
                option = "viridis",
                direction = -1,
                trans = "log10") +
        geom_abline(slope = 1) + expand_limits(x = min_val, y = min_val) +
        scale_x_continuous(trans='log10') +
        scale_y_continuous(trans='log10') + 
        coord_fixed() + ggtitle(v) + theme_bw() +
        xlab("Observed") + ylab("Predicted")
    print(gg)
}
dev.off()

pred_all[, value := vaccine_deaths_averted / mx]
dt <- merge(pred_all, loc_table[, .(location_id, region)], by = "location_id")
gg <- ggplot(dt[age == 1 & !(vaccine_short %in% c("D", "T", "P", "BCG"))], aes(x = coverage, y = value, color = region)) +
    geom_point(size = 0.1, alpha = 0.5) +
    facet_wrap(~vaccine_short, scales = "free_y") + 
    theme_bw()
gg



calc_mse <- function(dt) {
    dt[, averted := get_averted_scen(deaths_obs, coverage, pred_rr, a)]
    dt[, error := vaccine_deaths_averted - averted]
    mse <- mean(dt$error^2, na.rm = T)

    return(mse)
}
mse <- calc_mse(pred_all)


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

## Save for sharing
out_dt <- merge(
    scen_dt[, .(location_id, year, age, vaccine_short, label, averted)],
    loc_table[, .(location_id, location_name, location_iso3)],
    by = "location_id"
)
out_dt <- merge(out_dt, vaccine_table[, .(vaccine_short, vaccine_long)], by = "vaccine_short")
out_dt <- out_dt[order(location_name, year, age, vaccine_long),
    .(location_name, location_iso3, location_id, year, age, vaccine_long, vaccine_short, averted)]
out_dt <- out_dt[!is.na(averted)]
# write.csv(out_dt, "outputs/v01_reference_results.csv", row.names = F)

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
    mse <- lapply(split(pred_all, pred_all$vaccine_short), calc_mse)
    return(mse)
})
dt <- data.table(alpha = alpha_vals, rbindlist(diag_list))
melt_dt <- melt(dt, id.vars = "alpha")[!is.na(value)]
gg <- ggplot(melt_dt, aes(x = alpha, y = value)) + geom_line() + facet_wrap(~variable, scales = "free_y")
gg
plot(alpha_vals, diag_list, type = "l", xlab = "alpha",
    ylab = "Mean squared error")
