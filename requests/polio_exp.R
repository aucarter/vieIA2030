library(data.table); library(ggplot2)
dt <- fread("data-raw/polio_inc.csv")

gg <- ggplot(dt, aes(x = year, y = rate_100k, color = iso3)) + geom_point() +
    geom_smooth() +
    geom_vline(xintercept = c(1954, 1960))
gg
