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
o = set_options(do_step = 1)

# Step 1) xxxx
run_step1(o)  # See step1.R

# # Step 2) Run all scenarios
# run_scenarios(o)  # See scenarios.R
# 
# # Step 3) Operate on array scenarios
# run_arrays(o)  # See array.R
# 
# # Step 4) Plot results
# run_results(o)  # See results.R

# Finish up
message("* Finished!")

