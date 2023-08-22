

library(tidyverse)
library(stats4)
library(AICcmodavg)
# library(gsubfn)

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
explore_vimc = function() {
  
  # ---- Population size ----
  
  load_tables("wpp_input")
  
  pop_dt = wpp_input %>%
    group_by(country, year) %>%
    summarise(pop = sum(nx)) %>%
    ungroup() %>%
    as.data.table()
  
  # ---- Load VIMC impact ----
  
  load_tables("vimc_impact")
  
  impact_dt = vimc_impact %>%
    left_join(y  = d_v_at_table, 
              by = "d_v_at_id") %>%
    group_by(country, disease, vaccine, year) %>%
    summarise(impact = sum(deaths_averted)) %>%
    ungroup() %>%
    # left_join(y  = d_v_table, 
    #           by = c("disease", "vaccine")) %>%
    mutate(d_v = paste0(disease, "_", vaccine)) %>%
    select(country, d_v, year, impact) %>%
    # Cumulative sum impact...
    group_by(country, d_v) %>%
    mutate(impact_cum = cumsum(impact)) %>%
    ungroup() %>%
    # ... relative to 100k people...
    left_join(y  = pop_dt, 
              by = c("country", "year")) %>%
    mutate(impact_rel = 1e5 * impact_cum / pop) %>%
    select(-pop) %>%
    as.data.table()
  
  # ---- Load VIMC coverage ----
  
  # Path to VIMC coverage datatable
  coverage_file = system.file("extdata", "vimc_coverage.csv", package = "vieIA2030")
  
  # Extract coverage
  coverage_dt = fread(coverage_file) %>%
    group_by(country, disease, vaccine, year) %>%
    summarise(fvps   = sum(fvps_adjusted)) %>%
    # cohort = sum(cohort_size)) %>%
    ungroup() %>%
    # left_join(y  = d_v_table, 
    #           by = c("disease", "vaccine")) %>%
    # mutate(coverage = fvps / cohort) %>%
    mutate(d_v = paste0(disease, "_", vaccine)) %>%
    select(country, d_v, year, fvps) %>%
    # Cumulative sum FVPs...
    group_by(country, d_v) %>%
    mutate(fvps_cum = cumsum(fvps)) %>%
    ungroup() %>%
    # ... relative to 100k people...
    left_join(y  = pop_dt, 
              by = c("country", "year")) %>%
    mutate(fvps_rel = 1e5 * fvps_cum / pop) %>%
    select(-pop) %>%
    as.data.table()
  
  # ---- Plot stuff ----
  
  vimc_dt = impact_dt %>%
    inner_join(y  = coverage_dt, 
               by = c("country", "d_v", "year")) %>%
    filter(fvps > 0, impact > 0)
  
  g1 = ggplot(vimc_dt) + 
    aes(x = fvps, y = impact, colour = country) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~d_v, scales = "free")
  
  g2 = ggplot(vimc_dt) + 
    aes(x = fvps_cum, y = impact_cum, colour = country) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~d_v, scales = "free")
  
  g3a = ggplot(vimc_dt[!is.na(fvps_rel), ]) + 
    aes(x = fvps_rel, y = impact_rel, colour = country) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~d_v, scales = "free")
  
  g3b = ggplot(vimc_dt[!is.na(fvps_rel), ]) + 
    aes(x = fvps_rel, y = impact_rel, colour = country) +
    geom_line(show.legend = FALSE) +
    facet_wrap(~d_v, scales = "free")
  
  # ---- Best stats model ----
  
  # Set of statistical models / functions we want to test
  fns = list(
    # exp  = function(x, a)       y = exp(a*x),
    lin  = function(x, a, b)    y = a*x + b,
    quad = function(x, a, b, c) y = a*x^2 + b*x + c)
  
  # Good examples: NGA, GIN, ETH
  
  cases = vimc_dt %>%
    select(country, d_v) %>%
    unique() %>%
    mutate(id = 1 : n())
  
  aic = coef = list()
  
  pb = start_progress_bar(nrow(cases))
  
  for (id in cases$id) {
    case = cases[id, ]
    
    result = best_model(fns, vimc_dt, case$country, case$d_v)
    
    aic[[id]]  = result$aic
    coef[[id]] = result$coef
    
    setTxtProgressBar(pb, id)
  }
  
  close(pb)
  
  browser()
    
  # Ultimately we want to use AIC, for which we need maximum likelihood, 
  # but MLE is sensitive to starting point. Here we'll use ASD to get us to 
  # a decent point in parameter space.
  
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
best_model = function(fns, vimc_dt, country, d_v) {
  
  data_dt = prep_data(vimc_dt, country, d_v)
  
  if (nrow(data_dt) == 0)
    stop("No data to fit to")
  
  # Declare x and y values for which we want to determine a relationship
  x = data_dt$x
  y = data_dt$y
  
  start = prep_start(fns, x, y)
  
  fit = run_mle(fns, start, x, y)
  
  result = calc_aic(fit, country, d_v)
  
  return(result)
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
prep_data = function(vimc_dt, country, d_v) {
  
  data_dt = vimc_dt %>%
    filter(country == !!country, 
           d_v     == !!d_v,
           !is.na(fvps_rel)) %>%
    select(x_real = impact_rel, 
           y_real = fvps_rel) %>%
    mutate(x_max = max(x_real), 
           y_max = max(y_real), 
           x = x_real / x_max, 
           y = y_real / y_max)
  
  return(data_dt)
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
prep_start = function(fns, x, y) {
  
  # Points to evaluate when plotting
  x_eval = seq(0, max(x), length.out = 100)
  
  # Let's start with any old points
  start = list(
    lin  = list(s = 1, a = 1, b = 1),
    quad = list(s = 1, a = 1, b = 1, c = 1))
  
  # Define an objective function to minimise - sum of squares
  obj_fn = function(fn, ...) {
    
    # Squared difference
    diff_sq = (y - fns[[fn]](x, ...)) ^ 2
    
    # The sum of the squared difference
    obj_val = list(y = sum(diff_sq))
    
    return(obj_val)
  }
  
  # Define model-specific calls to objective function
  asd_fn = list(
    lin  = function(p, args) obj_fn("lin",  a = p[1], b = p[2]),
    quad = function(p, args) obj_fn("quad", a = p[1], b = p[2], c = p[3]))
  
  # Initiate list for storing plotting datatables
  plot_list = list()
  
  # Iterate through stats models
  for (fn in names(fns)) {
    
    # Number of parameters for this model
    n_pars = length(start[[fn]]) - 1
    
    # Run ASD optimisation algorithm
    optim = asd(
      fn   = asd_fn[[fn]],
      x0   = rep(1, n_pars),
      args = NULL,
      lb   = rep(0, n_pars), 
      ub   = rep(Inf, n_pars),
      max_iters  = 10000, 
      plot_iters = NULL, 
      verbose    = FALSE)
    
    # Construct plotting datatable for this model
    plot_args = c(list(x_eval), as.list(optim$x))
    plot_list[[fn]] = data.table(
      x = x_eval,
      y = do.call(fns[[fn]], as.list(plot_args)), 
      fn = fn)
    
    # Overwrite starting point with optimal parameters
    start[[fn]] = c(1, optim$x) %>%
      setNames(names(start[[fn]])) %>%
      as.list()
  }
  
  # Squash all models into one datatable
  plot_dt = rbindlist(plot_list)
  
  # Plot the quality of fit for each model starting point
  g_asd = ggplot(plot_dt, aes(x = x, y = y)) +
    geom_line(aes(colour = fn)) + 
    geom_point(data = data.table(x = x, y = y),
               colour = "black")
  
  return(start)
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
run_mle = function(fns, start, x, y) {
  
  # Points to evaluate when plotting
  x_eval = seq(0, max(x), length.out = 100)
  
  model = list(
    lin  = function(s, a, b)    likelihood(s, a*x + b),
    quad = function(s, a, b, c) likelihood(s, a*x^2 + b*x + c))
  
  likelihood = function(s, y_pred)
    ll = -sum(dnorm(x = y, mean = y_pred, sd = s, log = TRUE))
  
  # Initiate list for storing plotting datatables
  fit = plot_list = list()
  
  # Iterate through stats models
  for (fn in names(fns)) {
    
    fit[[fn]] = mle(model[[fn]], start = start[[fn]], lower = list(s = 1e-6))
    
    coef = fit[[fn]]@coef[-1]
    
    # Construct plotting datatable for this model
    plot_args = c(list(x_eval), as.list(coef))
    plot_list[[fn]] = data.table(
      x = x_eval,
      y = do.call(fns[[fn]], as.list(plot_args)), 
      fn = fn)
  }
  
  # Squash all models into one datatable
  plot_dt = rbindlist(plot_list)
  
  # Plot the quality of fit for each model
  g_mle = ggplot(plot_dt, aes(x = x, y = y)) +
    geom_line(aes(colour = fn)) + 
    geom_point(data = data.table(x = x, y = y),
               colour = "black")
  
  return(fit)
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
calc_aic = function(fit, country, d_v) {
  
  aic = sapply(fit, function(x) AIC(x)) %>%
    as.list() %>%
    as.data.table() %>%
    mutate(country = country, 
           d_v     = d_v, 
           .before = 1)
  
  coef = unlist(sapply(fit, function(x) x@coef[-1]))
  coef = data.table(var   = names(coef), 
                    value = coef) %>%
    separate(var, c("fn", "coef")) %>%
    mutate(country = country, 
           d_v     = d_v, 
           .before = 1)
  
  # aic_best = names(fns)[which.min(aic)]
  # aic_vals = paste(sort(round(aic)), collapse = " vs ")
  # 
  # aic_msg = paste0(aic_best, " (AIC: ", aic_vals, ")")
  
  return(list(aic = aic, coef = coef))
}

