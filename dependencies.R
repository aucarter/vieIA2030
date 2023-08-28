###########################################################
# DEPENDENCIES
#
# Deal with all package dependencies in one place.
#
###########################################################

# Clear global environment
rm(list = ls())

# ---- Install and/or load R packages with devtools ----

message("* Installing required packages")

# Install devtools if it isn't already
if (!("devtools" %in% installed.packages()[, 1]))
  install.packages("devtools")

# Install packages as defined in DESCRIPTION DCF file
# devtools::install_deps()

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

