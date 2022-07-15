library(data.table); library(ggplot2); library(readxl)

dt  <- fread("outputs/reference_results.csv")
locs <- read_xlsx("~/Downloads/List of African countries.xlsx") %>% select(ISO_code)


draw_cols <- paste0("draw_", 1:200)
afr_dt <- dt[location_iso3 %in% locs$ISO_code & year > 2000]
afr_dt[, decade := "2001-2010"]
afr_dt[year > 2010, decade := "2011-2020"]
afr_dt[year > 2020, decade := "2021-2030"]
afr_dt <- rbind(afr_dt, dt[location_iso3 %in% locs$ISO_code][, decade := "2001-2030"])

decade_dt <- afr_dt[, lapply(.SD, sum), by = .(decade), .SDcols = draw_cols]
melt_dt <- melt(decade_dt, id.vars = c("decade"))
decade_summary <- melt_dt[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = .(decade)]

write.csv(decade_summary, "requests/africa_decade_summary.csv", row.names = F)

decade_dt <- afr_dt[, lapply(.SD, sum), by = .(decade, location_iso3), .SDcols = draw_cols]

melt_dt <- melt(decade_dt, id.vars = c("decade", "location_iso3"))
decade_summary <- melt_dt[, .(mean = mean(value), 
    lower = quantile(value, 0.05), upper = quantile(value, 0.95)),
    by = .(decade, location_iso3)]

decade2 <- decade_summary[, .(mean = sum(mean)), by = decade]
