#' Generates the SQLite database with input data
gen_db <- function() {
    if (!file.exists("vieIA2030.db")) {
        message("No database found -- building now")

        message("   - Prepping vaccine coverage...")
        source("data-raw/coverage.R")

        message("   - Prepping WPP inputs...")
        source("data-raw/wpp_input.R")

        message("   - Prepping WPP observed...")
        source("data-raw/obs_wpp.R")

        if (file.exists("inst/extdata/vimc_estimates.csv")) {
            message("Prepping VIMC impact estimates...")
            source("data-raw/vimc_impact.R")
        } else {
            warning("Add VIMC estimates to inst/extdata")
        }

        message("Done!")
    }
}
