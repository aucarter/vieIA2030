library(tidyverse)
library(data.table)

plot_age_year <- function(dt) {
  gg <- ggplot(dt, aes(x = year, y = age, fill = value)) + 
    geom_tile() +
    xlab("Year") + scale_x_continuous(breaks=seq(2000, 2040, 20)) +
    ylab("Age") +
    viridis::scale_fill_viridis(
      option = "viridis", 
      direction = -1) + 
    theme_minimal() +
    coord_fixed() +
    theme(text = element_text(size = 15), legend.position = "none") +
    facet_wrap(.~measure)
  print(gg)
}

dt <- data.table(expand.grid(age = 0:50, year = 2000:2050))
dt[, value := 0]

period_dt <- copy(dt)
period_dt[year == 2020, value := 1]
period_dt[, measure := "By period"]

cohort_dt <- copy(dt)
cohort_dt[year - age == 2005, value := 1]
cohort_dt[, measure := "By cohort"]

vacc_dt <- copy(dt)
vacc_dt[year > 2006 & ((year - age) %in%  c(2002:2007, 1995)), value := 1]
vacc_dt[, measure := "By year of vaccination"]

all_dt <- rbindlist(list(period_dt, cohort_dt, vacc_dt))
plot_age_year(all_dt)
