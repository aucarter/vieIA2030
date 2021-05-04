library(data.table); library(ggplot2)

dt  <- fread("outputs/reference_results.csv")

top10_locs <- dt[year > 2020 & disease == "Measles", .(total = sum(deaths_averted)), by = .(location_name)][rev(order(total))][1:10]$location_name
top10_dt <- dt[year > 2020 & disease == "Measles" & location_name %in% top10_locs, .(total = sum(deaths_averted)), by = .(year)]
top10_dt[, Group := "Measles top 10"]

not_top10_dt <- dt[year > 2020 & disease == "Measles" & !(location_name %in% top10_locs), .(total = sum(deaths_averted)), by = .(year)]
not_top10_dt[, Group := "Measles other"]

not_measles_dt <- dt[year > 2020 & disease != "Measles", .(total = sum(deaths_averted)), by = .(year)]
not_measles_dt[, Group := "Not measles"]

dt <- rbindlist(list(top10_dt, not_top10_dt, not_measles_dt))

my_colors <- RColorBrewer::brewer.pal(name = "Paired", n = 3)
gg1 <- ggplot(dt, aes(x = year, y = total / 1e6 , fill = Group)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = my_colors, name = "Group") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") +
    scale_x_continuous(breaks = scales::pretty_breaks())
print(gg1)

gg2 <- ggplot(dt[Group != "Not measles"], aes(x = year, y = total / 1e6 , fill = Group)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = my_colors, name = "Group") +
    theme_bw() + xlab("Year") + ylab("Deaths averted (in millions)") +
    scale_x_continuous(breaks = scales::pretty_breaks())
print(gg2)

pdf("requests/measles_top10.pdf")
print(gg1)
print(gg2)
dev.off()