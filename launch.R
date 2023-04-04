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

# Other possible pre-steps: Generate database

# Step 1) Calculate and impute relative risk
impute_all_rr()  # See rr.R

# Step 2) xxx
run_step2()  # See step2.R

# # Step 3) Operate on array scenarios
# run_arrays(o)  # See array.R
# 
# # Step 4) Plot results
# run_results(o)  # See results.R

# Finish up
message("* Finished!")

