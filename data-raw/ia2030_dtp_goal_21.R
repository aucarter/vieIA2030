# Note: This is using a version of coverage21 before all merges of id tables  ... need to resolve
dtp1_target <- function(dtp1_19, dtp3_19, surv_19, surv_30) {
    if (dtp1_19 >= 0.99) {
        dtp1_target <- dtp1_19
    } else {
        zero_dose_19 <- (1 - dtp1_19) * surv_19
        zero_dose_30 <- zero_dose_19 / 2
        dtp1_target <- round(1 - (zero_dose_30 / surv_30), 2)
    }
    # Smaller of 5% or DTP3 dropout
    dropout <- min((dtp1_19 - dtp3_19) / dtp1_19, 0.05)
    target <- round(dtp1_target * (1 - dropout), 2)
    return(target)
}


vdtp1_target <- Vectorize(dtp1_target)

dtp1 <- coverage_21[vaccine == "DTP1" & year == 2019]
setnames(dtp1, "observed_coverage", "dtp1_19")

dtp3 <- coverage_21[vaccine == "DTP3" & year == 2019]
setnames(dtp3, "observed_coverage", "dtp3_19")

surv_19 <- pop_21[age == 0 & year == 2019 & sex_id == 3]
setnames(surv_19, "pop", "surv_19")

surv_30 <- pop_21[age == 0 & year == 2030 & sex_id == 3]
setnames(surv_30, "pop", "surv_30")

dt <- merge(
    dtp1[, .(location_iso3, dtp1_19)], 
    dtp3[, .(location_iso3, dtp3_19)], 
    by = "location_iso3"
)
dt <- merge(
    dt, 
    surv_19[, .(location_iso3, surv_19)], 
    by = "location_iso3"
)
dt <- merge(
    dt, 
    surv_30[, .(location_iso3, surv_30)], 
    by = "location_iso3"
)

dt[, target := vdtp1_target(dtp1_19, dtp3_19, surv_19, surv_30)]
dt <- merge(dt, loc_table[, .(location_id, location_iso3)])
dt <- merge(dt, ia2030_dtp_goal[, .(location_id, value)], by = "location_id")

pdf("plots/target_comp.pdf")
plot(dt$value, dt$target, pch = 19, xlim = 0:1, ylim = 0:1, xlab = "old", ylab = "new")
abline(a = 0, b = 1)
dev.off()


ia2030_dtp_goal_21 <- dt[, .(location_iso3, target)]
ia2030_dtp_goal_21[, year := 2030]
setnames(ia2030_dtp_goal_21, "target", "value")
usethis::use_data(ia2030_dtp_goal_21, overwrite = T)
