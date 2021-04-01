calc_impact_factors <- function(dt) {
    dt <- merge(dt, d_v_at_table, by = "d_v_at_id")
    
    # Routine deaths averted
    routine_averted <- dt[
        (year - age) %in% 2000:2030 & activity_type == "routine",
        .(total_averted = sum(averted, na.rm = T)), by = .(location_id, d_v_at_id)
    ]

    # Campaign deaths averted
    campaign_averted <- dt[
        year %in% 2000:2030 & activity_type == "campaign",
        .(total_averted = sum(averted, na.rm = T)), by = .(location_id, d_v_at_id)
    ]
    
    total_averted <- rbind(routine_averted, campaign_averted)
    total_averted <- merge(total_averted, d_v_at_table, by = "d_v_at_id")

    # Totals FVPs
    total_fvps <- coverage[year %in% 2000:2030, .(total_fvps = sum(fvps,  na.rm = T)),
        by = .(location_id, v_at_id)]
    total_fvps <- merge(total_fvps, v_at_table)
    
    # Combine
    total_dt <- merge(
        total_averted[total_averted > 0], total_fvps,
        by = c("vaccine", "activity_type", "location_id"),
        all.x = T
    )

    # Collapse Rubella to combined
    rub_dt <- total_dt[vaccine == "Rubella", .(total_fvps = sum(total_fvps, na.rm = T), total_averted = sum(total_averted, na.rm = T)), by = .(vaccine, location_id, disease)]
    rub_dt[, activity_type := "combined"]
    rub_dt <- merge(rub_dt, v_at_table, by = c("vaccine", "activity_type"))
    rub_dt <- merge(rub_dt, d_v_at_table, by = c("disease", "vaccine", "activity_type"))
    total_dt <- rbind(total_dt[vaccine != "Rubella"], rub_dt, fill = T)

    total_dt[, pred_deaths_averted_rate := total_averted / total_fvps]

    return(total_dt[])
}

rake_impact <- function(impact_factors) {
    load_tables("vimc_yov_impact")
    yov_dt <- unique(vimc_yov_impact[, .(location_id, vaccine, activity_type, deaths_averted_rate)])
    # Take the mean here for the Rubella mishap
    yov_dt <- yov_dt[, lapply(.SD, mean), by = .(location_id, vaccine, activity_type)]
    impact_dt <- merge(impact_factors, yov_dt,
        by = c("location_id", "vaccine", "activity_type"), all.x = T)
    impact_dt[, raking_factor := deaths_averted_rate / pred_deaths_averted_rate]
    raking_summary <- summarize_raking(impact_dt)
    impact_dt <- merge(impact_dt, raking_summary[, .(vaccine, activity_type, median)],
        by = c("vaccine", "activity_type"), all.x = T)
    overall_routine_median <- median(impact_dt[activity_type == "routine"]$raking_factor, na.rm = T)
    impact_dt[is.na(median), median := overall_routine_median]
    impact_dt[is.na(deaths_averted_rate), raked_deaths_averted_rate := pred_deaths_averted_rate * median]
    impact_dt[, impact_factor := ifelse(is.na(deaths_averted_rate), raked_deaths_averted_rate, deaths_averted_rate)]

}