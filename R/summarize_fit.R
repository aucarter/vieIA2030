###########################################################
# SUMMARIZE FIT
#
# xxx
#
###########################################################

# ---------------------------------------------------------
# xxx
# Called by: xxx
# ---------------------------------------------------------
summarize_fit <- function(pred_all) {
    
    summary <- pred_all[strata_deaths_averted > 0,
                        .(
                            mse = mean((averted - strata_deaths_averted)**2) / mean(strata_deaths_averted**2),
                            min_rr = min(pred_rr)
                        ),
                        by = .(d_v_at_id)]
    summary <- merge(summary, d_v_at_table)
    
    return(summary)
}

# ---------------------------------------------------------
# xxx
# Called by: xxx
# ---------------------------------------------------------
summarize_raking <- function(impact_dt) {
    
    summary <- impact_dt[!is.na(raking_factor) > 0 & pred_deaths_averted_rate != 0,
                         .(  median = median(raking_factor),
                             mean = mean(raking_factor),
                             min = min(raking_factor),
                             max = max(raking_factor),
                             sd = sd(raking_factor),
                             cv = sd(raking_factor) / mean(raking_factor)
                         ),
                         by = .(vaccine, activity_type)]
    
    return(summary)
}

