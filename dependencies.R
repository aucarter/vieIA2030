###########################################################
# DEPENDENCIES
#
# Deal with all package dependencies in one place.
#
###########################################################

# Clear global environment
rm(list = ls())

# ---- R version check ----

# NOTE: This R version check can also be done within the description.dcf file...
#       See https://r-pkgs.org/description.html for details

# # R versions for which this project has been tested and is stable
# stable_versions = "4.1.0"
# 
# # R versions for which this project is stable (as a string)
# stable_str = paste(stable_versions, collapse = ", ")
# 
# # Get details of R version currently running
# version_info = R.Version()
# 
# # Construct version number from list details
# version_num = paste0(version_info$major, ".",  version_info$minor)
# 
# # Throw an error if this R version is unsuitable
# if (!version_num %in% stable_versions)
#   stop("This software is stable with R version(s): ", stable_str,
#        " (currently running ", version_num, ")")

# ---- Install and/or load R packages with devtools ----

message("* Installing required R packages")

# Check whether devtools itself has been installed
devtools_installed = "devtools" %in% rownames(installed.packages())

# If not, install it
if (!devtools_installed) 
  install.packages("devtools")

# Load devtools
# library(devtools)

# Install packages as defined in DESCRIPTION DCF file
devtools::install_deps()

# Load all functions defined within ./R/ directory
devtools::load_all()

# ---- Redefine or unmask particular functions ----

# # Unmask certain functions otherwise overwritten
# select  = dplyr::select
# filter  = dplyr::filter
# rename  = dplyr::rename
# recode  = dplyr::recode
# count   = dplyr::count
# union   = dplyr::union
# predict = stats::predict

# ---- Clean up ----

# Tidy up console after package loading
if (interactive()) clf()  # Close figures
if (interactive()) clc()  # Clear console

