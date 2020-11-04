# Prep GBD 2019 VPD burden estimates

## Load data
load(system.file("extdata", "gbd19_estimates.RData", package = "vieIA2030"))

## Subset to DTP & TB
cause_subset <- c("Tuberculosis", "Diphtheria", "Tetanus", "Pertussis")
gbd_estimates <- gbd_estimates[cause_name %in% cause_subset]

## Subset to rate
gbd_estimates <- gbd_estimates[metric_id == 3]
gbd_estimates[, metric_id := NULL]

## Ignore uncertainty (for now)
gbd_estimates[, c("upper", "lower") := NULL]

## Merge on associated vaccine info
data(vaccine_table)
gbd_estimates <- merge(
    gbd_estimates,
    vaccine_table[, .(vaccine_id, cause_name)]
)

usethis::use_data(gbd_estimates, overwrite = TRUE)