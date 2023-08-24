

library(tidyverse)
library(stats4)    # MLE algorithm

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
explore_vimc = function() {
  
  r2_threshold = 0.9
  
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
  
  fns      = fn_set()
  fns_dict = fn_set(dict = TRUE)
  
  cases = vimc_dt %>%
    filter(!is.na(fvps_rel)) %>%
    select(country, d_v) %>%
    unique() %>%
    mutate(id = 1 : n())
  
  coef = aic = r2 = list()
  
  pb = start_progress_bar(max(cases$id))
  
  for (id in cases$id) {
    case = filter(cases, id == !!id)
    
    result = get_best_model(fns, vimc_dt, case$country, case$d_v)
    
    coef[[id]] = result$coef
    aic[[id]]  = result$aic
    r2[[id]]   = result$r2
    
    setTxtProgressBar(pb, id)
  }
  
  close(pb)
  
  coef_dt = rbindlist(coef) # %>%
    # mutate(fn = recode(fn, !!!fns_dict))
  
  saveRDS(coef_dt, paste0(o$pth$testing, "coef.rds")) 
  
  aic_dt = rbindlist(aic, fill = TRUE)
  r2_dt  = rbindlist(r2)
  
  saveRDS(aic_dt, paste0(o$pth$testing, "aic.rds"))
  saveRDS(r2_dt,  paste0(o$pth$testing, "r2.rds"))
  
  r2_threshold = 0.9
  
  best_dt = aic_dt %>%
    left_join(y  = r2_dt, 
              by = c("country", "d_v")) %>%
    mutate(lin_r2_pass = lin_r2 > r2_threshold, 
           lin = ifelse(lin_r2_pass, -Inf, lin)) %>%
    pivot_longer(cols = names(fns),
                 names_to = "fn") %>%
    filter(!is.na(value)) %>%
    mutate(# fn = recode(fn, !!!fns_dict), 
           value = round(value)) %>%
    group_by(country, d_v) %>%
    slice_min(value, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(country, d_v, fn) %>%
    as.data.table()
  
  saveRDS(best_dt, paste0(o$pth$testing, "best_model.rds"))
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
fn_set = function(dict = FALSE) {
  
  # Set of statistical models / functions we want to test
  out = list(
    lin  = function(x, a, b)    y = a*x + b,
    quad = function(x, a, b, c) y = a*x^2 + b*x + c)
  
  # Alternative functionality - return dictionary
  if (dict == TRUE)
    out = c(
      lin  = "Linear", 
      quad = "Quadratic")
  
  return(out)
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
get_best_model = function(fns, vimc_dt, country, d_v) {
  
  # Reduce data down to what we're interested in
  data_dt = prep_data(vimc_dt, country, d_v)
  
  # Do not fit if insufficient data
  if (nrow(data_dt) <= 3)
    return()
  
  # Declare x and y values for which we want to determine a relationship
  x = data_dt$x
  y = data_dt$y
  
  # Use optim algorithm to get good starting point for MLE
  start = prep_start(fns, x, y)
  
  # Run MLE from this starting point
  fit = run_mle(fns, start, x, y)
  
  # Determine AICc value for model suitability
  result = model_quality(fit, country, d_v, x, y)
  
  return(result)
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
prep_data = function(vimc_dt, country, d_v) {
  
  data_dt = vimc_dt %>%
    filter(country == !!country, 
           d_v     == !!d_v) %>%
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
    
    # Attempt to fit MLE model using prevriously determined start point
    fit_result = tryCatch(
      expr  = mle(minuslogl = model[[fn]], 
                  start = start[[fn]], 
                  lower = list(s = 1e-6), 
                  nobs  = length(y)), 
      
      # Return trivial if MLE failed
      error   = function(e) NULL,
      warning = function(w) NULL)
    
    if (!is.null(fit_result)) {
      
      # Store fit and extract coefficients for plotting
      fit[[fn]] = fit_result
      coef      = fit_result@coef[-1]
      
      # Construct plotting datatable for this model
      plot_args = c(list(x_eval), as.list(coef))
      plot_list[[fn]] = data.table(
        x = x_eval,
        y = do.call(fns[[fn]], as.list(plot_args)), 
        fn = fn)
    }
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
model_quality = function(fit, country, d_v, x, y) {
  
  if (length(fit) == 0)
    return()
  
  # ---- Extract coefficients ----
  
  coef = unlist(lapply(fit, function(x) x@coef[-1]))
  coef = data.table(var   = names(coef), 
                    value = coef) %>%
    separate(var, c("fn", "coef")) %>%
    mutate(country = country, 
           d_v     = d_v, 
           .before = 1)
  
  # ---- AICc ----
  
  aic = sapply(fit, function(x) AICc(x)) %>%
    as.list() %>%
    as.data.table() %>%
    mutate(country = country, 
           d_v     = d_v, 
           .before = 1)
  
  # ---- R squared ----
  
  if ("lin" %in% names(fit)) {
    
    lin_coef = as.list(fit$lin@coef[-1])
    lin_y    = lin_coef$a * x + lin_coef$b
    
    r2 = data.table(lin_r2 = cor(y, lin_y) ^ 2) %>%
      mutate(country = country, 
             d_v     = d_v, 
             .before = 1)
    
  } else {
    
    r2 = NULL
  }
  
  return(list(coef = coef, aic = aic, r2 = r2))
}

# ---------------------------------------------------------
# Use AICc rather than AIC to reduce overfitting
# ---------------------------------------------------------
AICc = function(x) {
  
  # See en.wikipedia.org/wiki/Akaike_information_criterion
  
  # Sample size
  n = x@nobs
  
  # Number of parameters
  k = length(x@details$par)
  
  # The usual AIC term
  aic_term = stats::AIC(x)
  
  # An additional penalty term for small sample size
  pen_term = (2*k^2 + 2*k) / (n - k - 1)
  
  # Sum these terms
  aicc = aic_term + pen_term
  
  return(aicc)
}

# ---------------------------------------------------------
# xxx
# ---------------------------------------------------------
plot_best_model = function() {
  
  fns      = fn_set()
  fns_dict = fn_set(dict = TRUE)
  
  best_dt = readRDS(paste0(o$pth$testing, "best_model.rds"))
  coef_dt = readRDS(paste0(o$pth$testing, "coef.rds"))
  
  # ---- Plot function count ----
  
  plot_count = function(var, fig = "count", focus = "quad", ord = "n") {
    
    fn_ord = c(focus, setdiff(unique(best_dt$fn), focus))
    
    count_dt = best_dt %>% 
      rename(var = !!var) %>% 
      # Number and proportion of each fn...
      count(var, fn) %>%
      group_by(var) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(p = n / total) %>%
      # Set appropriate plotting order...
      rename(val = !!ord) %>%
      select(var, fn, val) %>%
      pivot_wider(names_from  = fn, 
                  values_from = val, 
                  values_fill = 0) %>%
      arrange_at(fn_ord) %>%
      pivot_longer(cols = -var, 
                   names_to  = "fn", 
                   values_to = "val") %>%
      mutate(var = fct_inorder(var)) %>%
      as.data.table()
    
    if (fig == "count") {
      
      g = ggplot(count_dt[val > 0]) + 
        aes(x = var, y = val, fill = fn) + 
        geom_col() + 
        coord_flip()
    }
    
    if (fig == "density") {
      
      g = ggplot(count_dt[fn == focus]) + 
        aes(x = val) +
        geom_bar()
    }
    
    return(g)
  }
  
  g1a = plot_count("d_v", ord = "n")
  g1b = plot_count("d_v", ord = "p")
  g2a = plot_count("country", fig = "count")
  g2b = plot_count("country", fig = "density")
  
  # ---- xxxxxxxx ----
  
  best_coef = coef_dt %>%
    inner_join(y  = best_dt, 
               by = c("country", "d_v", "fn")) %>%
    mutate(par = as.list(setNames(value, coef))) %>% 
    group_by(country, d_v, fn) %>% 
    summarise(par = list(par)) %>% 
    ungroup() %>% 
    as.data.table()
  
  eval_fn = function(a) {
    y = do.call(what = fn_set()[[a$fn]], 
                args = c(list(x = x), a$par))
  }
  
  x = seq(0, 1, length.out = 100)
  y = t(apply(best_coef, 1, eval_fn))
  
  best_fit = best_coef %>%
    cbind(y) %>%
    select(-fn, -par) %>%
    pivot_longer(cols = -c(country, d_v), 
                 names_to  = "v", 
                 values_to = "y") %>%
    mutate(v = str_remove(v, "^V"), 
           x = x[as.numeric(v)]) %>%
    select(country, d_v, x, y) %>%
    as.data.table()
  
  g = ggplot(best_fit) + 
    aes(x = x, y = y, colour = country) + 
    geom_line() + 
    facet_wrap(~d_v)
}

