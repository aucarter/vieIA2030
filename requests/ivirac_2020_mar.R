devtools::load_all()

# Vaccine efficacy densities
eff_dt <- merge(efficacy[, .(disease, mean)], efficacy_ui)
eff_dt <- merge(eff_dt, disease_table[, .(disease, disease_long)])
eff_dt[, val := mean * scalar]
eff_dt[, val := val / max(val)]
gg <- ggplot(eff_dt, aes(x =  val, color = disease_long)) + geom_density() +
    theme_bw() + theme(legend.position = "bottom") +
    labs(color = "") + xlab("") + ylab("Density") + ggtitle("Vaccine efficacy densities")
png("plots/efficacy_ui.png")
print(gg)
dev.off()

# VIMC ui
vimc_ui[disease == "Measles" & location_id == 55]



# Results comparison
# Income group
dt <- fread("outputs/reference_results.csv")
summary_dt <- rbindlist(lapply(unique(dt$income_group), function(d) {
    collapse_dt <- colSums(dt[income_group == d, paste0("draw_", 1:200)])
    mean <- mean(collapse_dt)
    lower <- quantile(collapse_dt, 0.025)
    upper <- quantile(collapse_dt, 0.975)
    data.table(
        income_group = d, mean = mean, lower = lower, upper = upper
    )
}))
summary_dt[, rel_lower := lower / mean]
summary_dt[, rel_upper := upper / mean]
gg <- ggplot(summary_dt, aes(y = income_group, yend = income_group, x = rel_lower, xend = rel_upper)) + 
    geom_segment() + xlim(c(0, max(summary_dt$rel_upper))) + theme_bw()
gg

## Disease
summary_dt <- rbindlist(lapply(unique(dt$disease), function(d) {
    collapse_dt <- colSums(dt[disease == d, paste0("draw_", 1:200)])
    mean <- mean(collapse_dt)
    lower <- quantile(collapse_dt, 0.025)
    upper <- quantile(collapse_dt, 0.975)
    data.table(
        disease = d, mean = mean, lower = lower, upper = upper
    )
}))
summary_dt[, rel_lower := lower / mean]
summary_dt[, rel_upper := upper / mean]
gg <- ggplot(summary_dt, aes(y = disease, yend = disease, x = rel_lower, xend = rel_upper)) + 
    geom_segment() + xlim(c(0, max(summary_dt$rel_upper))) + theme_bw()
gg
