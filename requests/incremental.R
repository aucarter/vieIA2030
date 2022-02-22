
routine_19 <- sum(
    scenario_impact$dt[year == 2019 & activity_type == "routine"]$deaths_averted,
    na.rm = T)
year_dt <- scenario_impact$year_totals
plot_dt <- rbind(
    year_dt[year > 2019][, Measure := 'Total'],
    copy(year_dt)[year > 2019][, c("total", "Measure") := .(total - routine_19, 'Incremental')]
)
pdf("plots/incremental_impact.pdf")
gg <- ggplot(plot_dt, aes(x = year, y = total / 1e6, color = Measure)) + geom_line() +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") +
    ggtitle("IA2030 impact measured by total and incremental")
gg
dev.off()
write.csv(plot_dt, "requests/incremental_total_impact.csv", row.names = F)
plot_dt[year > 2020, .(total = sum(total)), by = "Measure"]
