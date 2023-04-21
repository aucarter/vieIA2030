


# ---------------------------------------------------------
# Objective function to minimise
# ---------------------------------------------------------
obj_fn = function(par, data) {
  
  # Extract fitting parameters
  a = exp(par[1])
  b = exp(par[2])
  
  mean_diff = abs(a / (a + b) - data$mean)
  
  l_diff = abs(qbeta(0.05, a, b) - data$lower)
  u_diff = abs(qbeta(0.95, a, b) - data$upper)
  
  y = sum(c(20 * mean_diff, l_diff, u_diff))
  
  return(y)
}

# ---------------------------------------------------------
# Draw samples for beta distribution
# ---------------------------------------------------------
draw_fn = function(x, n) 
  rbeta(n, x$p1, x$p2) / (x$p1 / (x$p1 + x$p2))

# dt = tidyr::expand_grid(
#     p1 = seq(log(1), log(10), length.out = 100), 
#     p2 = seq(log(1), log(10), length.out = 100), 
#     y  = NA) %>%
#     as.data.table()

n_vaccines = nrow(efficacy)

# Initiate matrix (vaccine x beta distribution parameters)
opt_pars = matrix(NA, nrow = n_vaccines, ncol = 2)

for (i in 1 : n_vaccines) {
  
  # Vaccine efficacy details
  v = efficacy[i, .(mean, lower, upper)]
  
  # for (i in 1 : nrow(dt)) {
  #     dt$y[[i]] = obj_fn(unlist(dt[i, .(p1, p2)]), v)
  # }
  # 
  # g = ggplot(dt, aes(x = p1, y = p2, colour = y, fill = y)) + geom_point()
  
  # Determine ...
  opt_result = optim(par    = c(1, 1),     # Starting point
                     fn     = obj_fn,      # Objective function
                     data   = as.list(v),  # Additonal arguments for obj_fn
                     lower  = log(1), 
                     upper  = log(10),
                     method = "L-BFGS-B")
  
  opt_pars[i, ] = exp(opt_result$par)
}

# Convert optimal parameters to datatable
pars_dt = opt_pars %>%
  as_named_dt(c("p1", "p2")) %>%
  mutate(disease = efficacy$disease)

# Draw samples from beta distribution using these optimal parameters
efficacy_ui = pars_dt %>%
  # Sample from beta distribution...
  split(rownames(.)) %>%
  lapply(draw_fn, n = o$n_draws) %>%
  # Wrangle into long format dt...
  as_named_dt(efficacy$disease) %>%
  mutate(draw = paste0("draw_", 1 : o$n_draws)) %>%
  tidyr::pivot_longer(cols = -draw, 
                      names_to  = "disease", 
                      values_to = "scaler") %>%
  arrange(disease) %>%
  as.data.table()

# Plot density of samples against efficacy
g = ggplot(efficacy, aes(x = disease)) + 
  geom_violin(data = efficacy_ui, aes(y = scaler), 
              colour = "blue") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2) + 
  geom_point(aes(y = mean), size = 3)

# Save figure to file
fig_save(g, "efficacy draws")

# pdf("plots/efficacy_dists.pdf")
# curve(dbeta(x, p1, p2), 0, 1, main = efficacy[i,]$disease)
# abline(v = d[1], col = "green")
# abline(v = p1 / (p1 + p2), col = "green", lty = "dashed")
# abline(v = d[2], col = "red")
# abline(v = qbeta(0.05, p1, p2), col = "red", lty = "dashed")
# abline(v = d[3], col = "blue")
# abline(v = qbeta(0.95, p1, p2), col = "blue", lty = "dashed")
# dev.off()

# Save package data
usethis::use_data(efficacy_ui, overwrite = TRUE)

