# Organize all draw-level inputs
vaccine_efficacy <- efficacy_ui
deaths_averted <- vimc_ui
gbd_deaths <- gbd_estimates_ui
gbd_cov <- gbd_cov_ui
betas <- rbindlist(lapply(list.files("betas", full.names = T), function(f) {
    id <- as.integer(gsub(".rds", "", strsplit(f, "/")[[1]][2]))
    b <- as.data.table(readRDS(f))[, d_v_at_id := id]
}), fill = T, use.names = T)

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
# TODO: Make this a function rather than putting into global env
draw_idx <<- cbind(run_num, draw_idx)

#
