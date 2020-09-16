loc <- 840


# Set up transition matrix
forward_params <- function(params, year) {
    # Pull in year-specific parameters
    year_dt <- params[["wpp_dt"]][year >= year_start & year < year_end]
    mx <- year_dt[measure %in% c("mxM", "mxF")]$value
    g <- year_dt[measure == "migration"]$value
    tfr <- year_dt[measure == "tfr"]$value
    asfr <- year_dt[measure == "percentASFR"]$value / 100
    srb <- year_dt[measure == "sexRatio"]$value
    age_n <- length(params[["ages"]])
    # Transform parameters
    # TODO: I think we actually need q_x here?
    s <- 1 - mx
    s_m <- s[seq(length(s) / 2)]
    s_f <- s[(length(s) / 2 + 1):length(s)]
    # TODO: What to do about the more granular u5 age groups
    s_m <- c((s_m[1] + 4 * s_m[2]) / 5, s_m[3:length(s_m)])
    s_f <- c((s_f[1] + 4 * s_f[2]) / 5, s_f[3:length(s_f)])
    s_term_m <- tail(s_m, 1)
    s_term_f <- tail(s_f, 1)
    f <- tfr * asfr
    # TODO: Is it right to bind on a zero here?
    # TODO: What is the right way to SRB (M / F) here?
    f_bar_m <- rep(0, age_n)
    f_bar_f <- f_bar_m
    f_bar_m[params[["f_idx"]]] <- s[1] * (1 + srb)^(-1) *
                                  (f + c(tail(f, -1), 0)) * 0.5
    f_bar_f[params[["f_idx"]]] <- s[1] * (1 + (1 - srb))^(-1) *
                                  (f + c(tail(f, -1), 0)) * 0.5
    # Construct blocks of the Leslie matrix
    A <- rbind(
        f_bar_m,
        cbind(
            diag(c(tail(s_m, -1))),
            c(rep(0, age_n - 2), s_term_m)
        )
    )
    B <- diag(rep(0, age_n))
    C <- B
    C[1, ] <- f_bar_f
    D <- rbind(
        rep(0, age_n),
        cbind(
            diag(c(tail(s_f, -1))),
            c(rep(0, age_n - 2), s_term_f)
        )
    )
    L <- rbind(
        cbind(A, B),
        cbind(C, D)
    )
    return(list(L = L, g = g))
}

step_forward <- function(n, params, year) {
    p <- forward_params(params, year)
    n_g <- n * p[["g"]] / 2
    n <- p[["L"]] %*% (n + n_g) + n_g
    return(n)
}

project_pop <- function(params) {
    years <- params[["wpp_dt"]][measure == "tfr"]$year_start
    n <- params[["n0"]]
    n_mat <- matrix(n, nrow = length(n))
    for (y in years) {
        n <- step_forward(n, params, y)
        n_mat <- cbind(n_mat, n)
    }
    return(n_mat)
}

wpp_dt <- data.table(prep_wpp_data())[country_code == loc]
params <- list(
    wpp_dt = wpp_dt,
    n0 = wpp_dt[measure %in% c("popM", "popF") &
            year_start == min(year_start)]$value,
    ages = unique(wpp_dt[measure %in% c("popM", "popF") &
            year_start == min(year_start)]$age_start),
    f_idx = which(ages %in% unique(wpp_dt[measure == "percentASFR"]$age_start))
)

pop_proj <- project_pop(params)