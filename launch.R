###########################################################
# LAUNCH
#
# Main launch function for WHO vaccine impact estimates 
# IA2030 pipeline.
#
###########################################################

# Set working directory to sourced file
if (interactive()) setwd(getSrcDirectory(function() {}))

# Load all required packages and functions
source("dependencies.R")

message("Running VIE IA2030 pipeline")

# Set options (see options.R)
o = set_options(do_step = 2)

# Other possible pre-steps: Generate database

# Step 1) Calculate and impute relative risk
run_relative_risk()  # See relative_risk.R

# Step 2) Impact factors
run_impact_factors()  # See impact_factors.R

# Step 3) Generate uncertainty draws
run_uncertainty()  # See uncertainty.R

# Step 4) Produce results
run_results()  # See results.R

# Finish up
message("* Finished!")

