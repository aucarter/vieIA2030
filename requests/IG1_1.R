library(data.table)
dt <- fread("outputs/reference_results.csv")
region_dt <- dt[, .(deaths_averted = sum(deaths_averted)), by = .(year, region, disease)]
global_dt <- dt[, .(deaths_averted = sum(deaths_averted)), by = .(year, disease)]
global_dt[, region := "Global"]
out_dt <- rbind(region_dt, global_dt)

total_dt <- out_dt[, .(deaths_averted = sum(deaths_averted)), by = .(year, region)]
total_dt[, disease := "TOT"]
out_dt <- rbind(out_dt, total_dt)
out_dt <- out_dt[year %in% c(2019, 2020)]


full_table <- data.table(
    expand.grid(
        year = 2019:2020,
        region = unique(out_dt$region),
        disease = unique(out_dt$disease)
    )
)

out_dt <- merge(full_table, out_dt, by = c("year", "region", "disease"), all = T)

out_dt <- merge(disease_table[, .(disease, disease_long)], out_dt, by = "disease", all = T)

out_dt[disease == "TOT", disease_long := "Total"]

out_dt[is.na(deaths_averted), deaths_averted := 0]

write.csv(out_dt, "requests/IG1_1_data.csv", row.names = F)
