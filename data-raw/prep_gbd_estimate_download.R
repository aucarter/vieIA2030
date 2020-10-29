dt <- rbindlist(
    lapply(
        list.files(
            "/Users/austincarter/Downloads/GBD2019_Results",
            full.names =  T
        ),
        fread
    )
)
dt <- dt[age_name != "25 plus"]
dt <- dt[, .(location_name, sex_id, age_name, cause_name, metric_id, year, val,
             upper, lower)]
dt[metric_id == 3,
    c("val", "upper", "lower") := .(val / 1e5, upper / 1e5, lower / 1e5)]

dt[cause_name == "pertussis", cause_name := "Pertussis"]
data(loc_table)
gbd_loc_map <- loc_table
setnames(gbd_loc_map, "country_name", "location_name")
dt <- merge(dt, gbd_loc_map)
dt[, c("location_name", "country_iso3") := NULL]

age_map <- data.table(
    age_name = c(
        "<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
        "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
        "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 plus"
    ),
    age = c(0, 1, seq(5, 80, 5))
)
gbd_estimates <- merge(dt, age_map)
gbd_estimates[, age_name := NULL]
setnames(gbd_estimates, "val", "value")

save(gbd_estimates, file = "inst/extdata/gbd19_estimates.RData")
