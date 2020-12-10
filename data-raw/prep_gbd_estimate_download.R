dt <- rbindlist(
    lapply(
        list.files(
            "supp_data/GBD2019_Results",
            full.names =  T
        ),
        fread
    )
)
dt <- dt[age_name != "25 plus"]
## Ignore uncertainty (for now)
dt <- dt[, .(location_name, sex_id, age_name, cause_name, metric_id, year, val)]
dt[metric_id == 1, val := val]

dt[cause_name == "pertussis", cause_name := "Pertussis"]

# Convert ages
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

## Subset to DTP & TB
cause_subset <- c("Tuberculosis", "Diphtheria", "Tetanus", "Pertussis")
gbd_estimates <- gbd_estimates[cause_name %in% cause_subset]

## Subset to rate
gbd_estimates <- gbd_estimates[metric_id == 3]
gbd_estimates[, metric_id := NULL]

## Merge on associated vaccine info
gbd_estimates <- merge(
    gbd_estimates,
    vaccine_table[, .(vaccine_id, cause_name)],
    by = "cause_name"
)

## Merge on location_id
gbd_estimates <- merge(
    gbd_estimates,
    loc_table[, .(location_name, location_id)],
    by = "location_name"
)

gbd_estimates[, c("location_name", "cause_name") := NULL]

save(gbd_estimates, file = "inst/extdata/gbd19_estimates.RData")
