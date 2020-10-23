message("Prepping demography...")
source("data-raw/demog.R")

message("Prepping vaccine coverage...")
source("data-raw/coverage.R")

if (file.exists("inst/extdata/vimc_estimates.csv")) {
    message("Prepping VIMC impact estimates...")
    source("data-raw/vimc_impact.R")
} else {
    warning("Add VIMC estimates to inst/extdata")
}

message("Done!")