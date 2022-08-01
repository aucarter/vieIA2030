# Organize all draw-level inputs
vaccine_efficacy <- 
deaths_averted <- 
gbd_deaths <- 
gbd_cov <- 

# Sort them (consider location level)

# LHS by location
n_draws <- 200
input_names <- c("efficacy", "strata_deaths_averted", "haqi", "strata_deaths",
    "betas")
draw_idx <- data.table::data.table(
    round(lhs::randomLHS(n_draws, length(input_names)) * n_draws + 0.5)
)
names(draw_idx) <- input_names
run_num <- 1:n_draws
draw_idx <- cbind(run_num, draw_idx)

#
