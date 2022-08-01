gen_draws <- function(mean, sd, n) {
    set.seed(1)
    draws_mat <- t(mapply(rnorm, n = n,  mean = mean, sd = sd))
    colnames(draws_mat) <- paste0("draw_", 1:n)
    return(draws_mat)
}