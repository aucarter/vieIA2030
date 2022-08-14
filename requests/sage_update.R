library(data.table); library(ggplot2)
dt <- fread("results/updated_reference_results_21.csv")
dt <- dt[order(disease)]
dt[, disease := as.factor(disease)]

# Global deaths averted target and observed by disease
global_dt <- dt[, .(target_deaths_averted = sum(new_pop_target_deaths_averted),
    observed_deaths_averted = sum(new_pop_observed_deaths_averted)), by = .(disease, year)]
global_dt[, gap := observed_deaths_averted - target_deaths_averted]
global_dt[, region := "Global"]

global_target <- global_dt[, .(target = sum(target_deaths_averted)), by = year]

# Regional deaths averted target and observed by disease
region_dt <- dt[, .(target_deaths_averted = sum(new_pop_target_deaths_averted),
    observed_deaths_averted = sum(new_pop_observed_deaths_averted)), by = .(region, disease, year)]
region_dt[, gap := observed_deaths_averted - target_deaths_averted]

region_target <- region_dt[, .(target = sum(target_deaths_averted)), by = .(year, region)]

# Save regional and global
combined_dt <- rbind(global_dt, region_dt)
write.csv(combined_dt, "results/aggregate_updated_reference_results_21.csv")

## Plots
my_colors1 <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), c("darkblue", "darkgreen"))
my_colors1[1] <- "hotpink"

pdf("results/updated_reference_results_21.pdf")

# Global averted
gg <- ggplot() + 
    geom_bar(data = global_dt, position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = observed_deaths_averted / 1e6, fill = disease)) +
    geom_line(data = global_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    geom_point(data = global_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") +
    ggtitle("Observed global deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2019:2021) +
    scale_color_manual(name = "", values = c("IA2030\ntarget" = "black"))
print(gg)

# Regional averted
gg <- ggplot() + 
    geom_bar(data = region_dt, position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = observed_deaths_averted / 1e6, fill = disease)) +
    geom_line(data = region_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    geom_point(data = region_target, aes(x = year, y = target / 1e6, color = "IA2030\ntarget")) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") +
    ggtitle("Observed regional deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2019:2021) +
    scale_color_manual(name = "", values = c("IA2030\ntarget" = "black")) +
    facet_wrap(.~region, scales = "free_y")
print(gg)

# Global gap
gg <- ggplot() + 
    geom_bar(data = global_dt[year %in% 2020:2021], position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = gap / 1e3, fill = disease)) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + ylab("Difference between observed and target deaths averted (in thousands)") + xlab("Year") +
    ggtitle("Global gaps in deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2020:2021) +
    geom_hline(yintercept = 0)
print(gg)

# Regional gap
gg <- ggplot() + 
    geom_bar(data = region_dt[year %in% 2020:2021], position = "stack", stat = "identity", width = 0.5,
    aes(x = year, y = gap / 1e3, fill = disease)) +
    scale_fill_manual(values = my_colors1, name = "Disease") +
    theme_bw() + ylab("Difference between observed and target deaths averted (in thousands)") + xlab("Year") +
    ggtitle("Regional gaps in deaths averted by disease compared to the IA2030 targets") +
    scale_x_continuous(breaks = 2020:2021) +
    geom_hline(yintercept = 0) +
    facet_wrap(.~region, scales = "free_y")
print(gg)

dev.off()

