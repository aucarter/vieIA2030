#' Generate population projection parameters from WPP data
#' @param wpp_dt A data.table with WPP data
#' @return A list of parameters
#' @import data.table
#' @export
make_params <- function(wpp_dt) {
    n0 <- wpp_dt[measure %in% c("popM", "popF") &
            year_start == min(year_start)]$value
    ages <- unique(wpp_dt[measure %in% c("popM", "popF") &
            year_start == min(year_start)]$age_start)
    f_idx <- which(ages %in% unique(wpp_dt[measure == "percentASFR"]$age_start))
    return(
        list(wpp_dt = wpp_dt, n0 = n0, ages = ages, f_idx = f_idx)
    )
}


#' Generate the parameters for a forward step
#' @param params A list holding all population projection parameters
#' @param year Integer time period
#' @return A list with two elements:
#'      (1) L - A matrix for survival and progression
#'      (2) g - A vector with migration values
forward_params <- function(params, year) {
    # Pull in year-specific parameters
    year_dt <- params[["wpp_dt"]][year >= year_start & year < year_end]
    mx <- year_dt[measure %in% c("mxM", "mxF")]$value
    tfr <- year_dt[measure == "tfr"]$value
    asfr <- year_dt[measure == "percentASFR"]$value / 100
    srb <- year_dt[measure == "sexRatio"]$value
    L <- make_leslie(mx, tfr, asfr, srb)

    # TODO: This is one value! How to split by age and sex?
    g <- year_dt[measure == "migration"]$value
    return(list(L = L, g = g))
}

make_leslie <- function(mx, tfr, asfr, srb) {
    age_n <- length(asfr)
    
    # Survival
    # TODO: I think we actually need q_x here?
    s <- 1 - mx
    s_m <- s[seq(length(s) / 2)]
    s_f <- s[(length(s) / 2 + 1):length(s)]
    # TODO: What to do about the more granular u5 age groups
    s_m <- c((s_m[1] + 4 * s_m[2]) / 5, s_m[3:length(s_m)])
    s_f <- c((s_f[1] + 4 * s_f[2]) / 5, s_f[3:length(s_f)])
    s_term_m <- tail(s_m, 1)
    s_term_f <- tail(s_f, 1)
    
    # Fertility
    f <- tfr * asfr
    f_bar_m <- rep(0, age_n)
    f_bar_f <- f_bar_m
    f_bar_m[params[["f_idx"]]] <- s[1] * (1 - (1 + srb)^(-1)) *
                                  (f + c(tail(f, -1), 0)) * 0.5
    f_bar_f[params[["f_idx"]]] <- s[1] * (1 + srb)^(-1) *
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

    return(L)
}

#' Step the population forward one time period
#' @param n A vector with the current population
#' @param params A list holding all population projection parameters
#' @param year Integer time period
#' @return A vector with the population one time period forward
step_forward <- function(n, params, year) {
    p <- forward_params(params, year)
    n_g <- n * p[["g"]] / 2
    n <- p[["L"]] %*% (n + n_g) + n_g
    return(n)
}

#' Project the population
#' @param params A list holding all population projection parameters
#' @return A matrix with the population in all time periods
#' @export
# TODO: Output mortality too!
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
