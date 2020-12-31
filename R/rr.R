check_rr <- function(dt) {
    # Look for missing coverage where deaths averted are non-zero
    if (nrow(dt[coverage == 0 & vaccine_deaths_averted > 0]) > 0) {
        prop <- round(nrow(dt[coverage == 0 & vaccine_deaths_averted > 0]) /
            nrow(dt[vaccine_deaths_averted > 0]) * 100, 2)
        warning("Missing coverage in ", prop,
                "% of location-age-years with deaths averted")
    }
    # Check for non-sensical numbers
    if (any(range(dt[!is.na(rr)]$rr) < 0 | range(dt[!is.na(rr)]$rr) > 1)) {
        prop <- round(nrow(dt[coverage > 0 & (rr < 0 | rr > 1)]) /
                      nrow(dt) * 100, 2)
        warning(paste0(
            "Over 1 or less than 0 mortality reduction in ", prop,
            "% of location-age-years"))
    }
}

vimc_rr <- function(dt, alpha) {
    out_dt <- copy(dt)
    out_dt[coverage > 0, rr := (deaths_obs - (vaccine_deaths_averted *
            (1 - coverage ^ alpha) / coverage ^ alpha)) /
            (deaths_obs + vaccine_deaths_averted)]
    return(out_dt[])
}

gbd_rr <- function(dt, alpha, beta) {
    out_dt <- copy(dt)
    out_dt[coverage > 0, deaths_no := vaccine_deaths /
            (1 - beta * efficacy * coverage ^ alpha)]
    out_dt[coverage > 0, rr :=
            (deaths_obs - vaccine_deaths + (1 - beta * efficacy) * deaths_no) /
            (deaths_obs - vaccine_deaths + deaths_no)]
    out_dt[, deaths_no := NULL]

    return(out_dt[])
}

prep_rr <- function(vacc, vacc_params) {
    vimc <- vaccine_table[vaccine_short == vacc]$vimc == 1
    vacc_id <- vaccine_table[vaccine_short == vacc]$vaccine_id
    # Load data
    if (vimc) {
        load_table_list(c("vimc_impact", "all_deaths", "coverage_inputs"))
        dt <- copy(vimc_impact)
        setnames(dt, "value", "vaccine_deaths_averted")
        dt[, c("sex_id", "vaccine_deaths") := .(3, NA)]
        # Calculate both-sexes deaths
        deaths <- copy(all_deaths)[, .(deaths_obs = sum(deaths)),
                         by = .(age, year, location_id)]
        deaths[, sex_id := 3]
    } else {
        load_table_list(c("gbd_vaccine_deaths", "all_deaths", "coverage_inputs"))
        dt <- copy(gbd_vaccine_deaths)
        setnames(dt, "value", "vaccine_deaths")
        dt[, vaccine_deaths_averted := NA]
        deaths <- copy(all_deaths)
        setnames(deaths, "deaths", "deaths_obs")
    }
    # All-cause deaths
    dt <- merge(
        dt[vaccine_id == vacc_id],
        deaths,
        by = c("age", "year", "location_id", "sex_id"),
        all =  T
    )
    dt[, vaccine_id := vacc_id]
    # Coverage
    cov_dt <- coverage_inputs[vaccine_id == vacc_id,
        .(coverage = mean(value)),
        by = .(location_id, vaccine_id, year, age)]
    cov_dt[, sex_id := 3]
    if (!vimc) {
        cov_dt <- rbindlist(lapply(1:2, function(s) {
            copy(cov_dt)[, sex_id := s]
        }))
    }
    dt <- merge(
            dt,
            cov_dt,
            by = c("location_id", "vaccine_id", "year", "age", "sex_id"),
            all = T
        )
    # Efficacy
    dt[, efficacy := ifelse(vimc, NA, efficacy[vaccine_short == vacc]$mean)]
    # Calcualte RR
    if (vimc) {
        dt <- vimc_rr(dt, vacc_params$alpha)
    } else {
        dt <- gbd_rr(dt, vacc_params$alpha, vacc_params$beta)

    }
    out_dt <- dt[, .(location_id, age, year, vaccine_id, deaths_obs,
                 vaccine_deaths_averted, vaccine_deaths, coverage, rr)]
    # Check
    check_rr(out_dt)
    return(out_dt)
}

merge_rr_covariates <- function(dt) {
    # Expand to all locations, years, and ages
    full_dt <- data.table(expand.grid(
        location_id = unique(loc_table$location_id),
        age = 0:95,
        year = 2000:2095
    ))
    dt <- merge(full_dt, dt, by = c("location_id", "age", "year"), all.x = T)
    # Add mortality
    load_table_list("wpp_input")
    mx_dt <- wpp_input[, .(mx = mean(mx)), by = .(location_id, year, age)]
    dt <- merge(dt, mx_dt, by = c("location_id", "age", "year"), all.x = T)
    # Add GBD covariates(SDI and HAQi)
    dt <- merge(dt, gbd_cov, by = c("location_id",  "year"), all.x = T)
    return(dt)
}

get_averted_deaths <- function(deaths_obs, coverage, rr, alpha) {
    averted_deaths <- deaths_obs * (
            coverage ^ alpha * (1 - rr) / (1 - coverage ^ alpha * (1 - rr))
        )

    return(averted_deaths)
}

impute_vacc_rr <- function(vacc, params) {
    print(vacc)
    vacc_params <- params[[vacc]]
    dt <- prep_rr(vacc, vacc_params)
    dt <- merge_rr_covariates(dt)
    fit <- glm(
        rr ~ haqi + sdi + year + mx +
             splines::bs(age, knots = vacc_params$age_knots),
        data = dt[rr < 1 & rr > 0],
        family = "binomial"
    )
    dt[, pred := predict(fit, dt)]
    dt[, pred_rr := ifelse( # for floating point precision
        pred > 0,
        1 / (1 + exp(-pred)),
        exp(pred) / (exp(pred) + 1))]
    dt[,
        c("pred", "haqi", "sdi", "mx") := NULL]
    dt[, vaccine_short := vacc]

    dt[, averted := get_averted_deaths(
        deaths_obs, coverage, pred_rr, vacc_params$alpha)]

    return(dt)
}

impute_all_rr <- function(params) {
    ## Simple model for VIMC imputation
    pred_all <- rbindlist(
        lapply(
            unique(vaccine_table$vaccine_short),
            impute_vacc_rr,
            params
        ),
        fill = T
    )

    return(pred_all)
}
