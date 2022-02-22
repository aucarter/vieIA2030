    # Load 2019 coverage
    load_tables(c("coverage_inputs", "wpp_input"))
    cov_dt <- coverage_inputs[year == 2019]
    baby_dt <- wpp_input[year == 2019 & age == 0, .(nx = sum(nx)), by = .(location_id)]
    dt <- merge(cov_dt, baby_dt, by = "location_id")
    dt <- dt[, .(coverage = max(value)), by = .(location_id, vaccine_id, nx)]
    dt <- merge(dt, vaccine_table[, .(vaccine_id, vaccine_short)])
    dt <- merge(dt, loc_table[, .(location_id, location_name)], by = "location_id")
    cast_dt <- dcast(dt, location_name + nx ~ vaccine_short, value.var = "coverage")
    cast_dt[is.na(cast_dt)] <- 0
    out_dt <- cast_dt[rev(order(nx))]
    write.csv(out_dt, "vacc_intro.csv", row.names = F)

    dt <- merge(out_dt, loc_table[, .(location_name, yf, je)], by = "location_name")
    yf_dt <- dt[yf == 1, .(location_name, nx, YF)]
    write.csv(yf_dt, "yf.csv", row.names = F)
    je_dt <- dt[je == 1, .(location_name, nx, JE)]
    write.csv(je_dt, "je.csv", row.names = F)
