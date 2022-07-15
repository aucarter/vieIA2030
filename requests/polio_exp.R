library(data.table); library(ggplot2)
dt <- fread("data-raw/polio_inc.csv")

subset_dt <- dt[(iso3 == "USA" & year <= 1954) | (iso3 == "ITA" & year <= 1960)]

cols <- c("#8b0000", "#003758")
names(cols) <- unique(dt$iso3)

dt$iso3 <- as.factor(dt$iso3)
png("plots/polio_data.png")
gg <- ggplot(subset_dt, aes(x = year, y = rate_100k, color = iso3)) +
    geom_smooth(method = "glm", formula = y~x,
                      method.args = list(family = gaussian(link = 'log'))) +
    scale_color_manual(values = cols) +
    geom_vline(xintercept = c(1954, 1960), color = c("#003758", "#8b0000")) +
    xlab("Year") + ylab("Rate (per 100k)") + theme_bw() +
    ggtitle("Reported polio cases prior to vaccination") + 
    geom_point(data = dt) + theme(legend.position = "bottom") +
    ylim(c(0, 45))
gg
dev.off()
