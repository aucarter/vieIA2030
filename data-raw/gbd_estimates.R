# Prep GBD 2019 VPD burden estimates

## Load data
load(system.file("extdata", "gbd19_estimates.RData", package = "vieIA2030"))

usethis::use_data(gbd_estimates, overwrite = TRUE)