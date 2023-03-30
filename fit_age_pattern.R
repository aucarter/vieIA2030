#' Prep an age pattern for each disease-vaccine-activity type by fitting a 
#' spline to the age-specific cohort proportions of deaths averted, or deaths in
#' the case of the GBD diseases.
#' 

devtools::load_all()
library(data.table);library(lme4); library(ggplot2); library(mgcv)

vimc_dt <- db_pull("vimc_impact")  # Short hand for load_tables?
gbd_dt <- db_pull("gbd_strata_deaths")  # Short hand for load_tables?
gbd_dt <- gbd_dt[, .(deaths_averted = sum(value)), by = .(location_id, d_v_at_id, age, year)]

dt <- rbind(vimc_dt, gbd_dt)
dt[, cohort := year - age]
dt <- dt[deaths_averted > 0]
cohort_total <- dt[, .(cohort_total = sum(deaths_averted), cohort_n = .N),
    by = .(location_id, d_v_at_id, cohort)]
dt <- merge(dt, cohort_total)
dt[, cohort_prop := ifelse(cohort_total == 0, 0, deaths_averted / cohort_total)]

all_pred_dt <- data.table()

pdf("plots/age_pattern_fit.pdf")
for (ID in unique(dt$d_v_at_id)) {
    id_dt <- dt[d_v_at_id == ID]
    age_range <- range(id_dt$age)
    id_dt <- rbind(
        id_dt,
        data.table(
            expand.grid(
                location_id = unique(id_dt$location_id),
                cohort = unique(id_dt$cohort),
                age = c(age_range[2] + 1),
                cohort_prop = 1e-2
            )
        ), fill = T
    )
    fit <- gam(
        log(cohort_prop) ~ s(age, k = 5, bs = "bs", m = c(3, 2)),
        data = id_dt, method = "REML"
    )
    pred_dt <- data.table(age = seq(age_range[1], age_range[2]), cohort = 0)
    pred_dt[, pred := exp(predict(fit, newdata = pred_dt))]
    pred_dt[, norm_pred := pred / sum(pred)]
    pred_dt[, d_v_at_id := ID]
    all_pred_dt <- rbind(all_pred_dt, pred_dt)

    melt_dt <- melt(pred_dt[, .(age, pred, norm_pred)], id.vars = c("age"))

    # Plot
    gg <- ggplot(id_dt[cohort_n > 2], aes(x = age, y = cohort_prop)) +
        geom_point(size = 0.3, alpha = 0.3) +
        geom_line(data = melt_dt, aes(x = age, y = value, color= variable)) +
        ggtitle(
            paste0(
                d_v_at_table[d_v_at_id == ID]$disease, " - ",
                d_v_at_table[d_v_at_id == ID]$activity_type
            )
        )
    print(gg)
}
dev.off()


age_pattern_dt <- all_pred_dt[, .(d_v_at_id, age, norm_pred)]
setnames(age_pattern_dt, "norm_pred", "age_pattern")

write.csv(age_pattern_dt, "results/age_pattern.csv", row.names = F)
