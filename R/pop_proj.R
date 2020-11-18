#' Execute cohort-component projection model
#' @param nx Population matrix
#' @param sx Survival matrix
#' @param fx Fertility matrix
#' @param mig Migration matrix
#' @return A list with population and births matrices
#' @export
get_ccpm <- function(nx, sx, fx, mig, n) {
  z <- dim(sx)[1] / 2
  n <- dim(sx)[2]
  srb <- 1.05
  bx <- array(dim = n)
  p <- array(dim = c(2 * z, n + 1))
  p[, 1] <- nx[, 1]
  for (i in 1:n) {
    next_pop <- update_pop(p[, i], sx[, i], fx[, i], mig[, i], z, srb)
    p[, i + 1] <- next_pop[[1]]
    bx[i] <- next_pop[[2]]
  }

  ccpm <- list(population = p[, 2:(n + 1)], births = bx)

  return(ccpm)
}

update_pop <- function(p_in, sx, fx, mig, z, srb) {
  p_out <- vector()
  bx_out <- 0
  for (sex in c("female", "male")) {
    if (sex == "female") {
      first_idx <- 1
      mid_idx <- 2:(z - 1)
      final_idx <- z
    } else {
      first_idx <- z + 1
      mid_idx <- z + 2:(z - 1)
      final_idx <- z + z
    }

    # Middle age groups
    p_out[mid_idx] <- p_in[mid_idx - 1] * (
        sx[mid_idx - 1] * (1 + .5 * mig[mid_idx]) +
        .5 * mig[mid_idx]
      )

    # Final age group
    p_out[final_idx] <- p_in[final_idx - 1] * (
        sx[final_idx - 1] * (1 + .5 * mig[final_idx]) +
        .5 * mig[final_idx]
      ) +
      p_in[final_idx] * (
        sx[final_idx] * (1 + .5 * mig[final_idx]) +
        .5 * mig[final_idx]
      )
    
    # First age group
    bx <- calc_births(srb, fx, sx, p_in, mig, sex)
    bx_out <- bx_out + bx
    p_out[first_idx] <- bx * (sx[first_idx] * (1 + .5 * mig[first_idx]) +
      .5 * mig[first_idx])
  }


  return(list(p_out, bx_out))
}

calc_births <- function(srb, fx, sx, p_in, mig, sex = "female") {
  # Calculate births
  fxb <- ((1 + srb) ^ (-1) * (
      fx[10:54] +
      fx[11:55] * sx[11:55]
    ) * 0.5)

  if (sex == "male") {
    fxb <- fxb * srb
  }

  bx <- sum(
      fxb * p_in[10:54] * (1 + .5 * mig[10:54])
    )

  return(bx)
}


# TODO: This could be more clear
#' Not sure why this is needed
get_person <- function(x) {
  x[1:96] + x[-c(1:96)]
}

#' Calculate life-table values from mortality rates
#' @param y Vector of mortality rates
#' @return  A vector of life-expectancy, 1q0, and 5q0
lt_est <- function(y) {
  px <- exp(-y)
  lx <- c(1, cumprod(px))
  ex <- round(sum(head(lx, -1) + tail(lx, -1)) / 2, 3)
  q5 <- 1000 * (1 - lx[6])
  q1 <- 1000 * (1 - lx[2])
  lt <- c(ex, q1, q5)

  return(lt)
}

#' Run the population projection for a particular country and year range
#' @param is Location name
#' @param y0 Start year of projection
#' @param y1 End year of projection
#' @param wpp_input Input WPP data
#' @param scenario Name of the scenario being run
#' @return A list of tables with population projection results
project_pop <- function(is, y0, y1, wpp_input, scenario = "default") {
  n <- y1 - y0 + 1
  wpp_ina <- wpp_input  %>%
    filter(location_name == is & year_id %in% (y0 - 1):y1) %>%
    select(-c(location_name)) %>%
    arrange(sex_name, age, year_id)
  nx <- wpp_ina %>%
    select(sex_name, age, year_id, nx) %>%
    tidyr::spread(year_id, nx) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
  nx[nx == 0]   <- 1e-09

  wpp_ina <- wpp_ina %>%
    filter(year_id %in% y0:y1)
  fx <- wpp_ina %>%
    select(sex_name, age, year_id, fx) %>%
    tidyr::spread(year_id, fx) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
  mig <- wpp_ina %>%
    select(sex_name, age, year_id, mig) %>%
    tidyr::spread(year_id, mig) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
  if (scenario == "default") {
    mx <- wpp_ina %>%
    select(sex_name, age, year_id, mx) %>%
    tidyr::spread(year_id, mx) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
    mx[mx == 0] <- 1e-09
  } else {
    mx <- get_mx_scenario(is, y0, y1, scenario)
  }

  sx <- exp(-mx)
  
  ccpm_res <- get_ccpm(nx, sx, fx, mig)
  population <- ccpm_res[["population"]]
  births <- ccpm_res[["births"]]
  deaths <- -1 * (log(sx)) * population[, 1:n]

  list(population = population, deaths = deaths, births = births, mx = mx)
}

add_lt <- function(projected_pop, is, y0, y1) {
  population <- projected_pop$population
  deaths <- projected_pop$deaths
  births <- projected_pop$births
  mx <- projected_pop$mx

  pop_tot <- apply(population, 2, get_person)
  deaths_tot <- apply(deaths, 2, get_person)
  mx_both  <- deaths_tot / pop_tot

  lt_out_both <- apply(mx_both, 2, lt_est)
  lt_out_fmle <- apply(mx[1:96, ], 2, lt_est)
  lt_out_mle <- apply(mx[97:192, ], 2, lt_est)

  deaths_both <- apply(deaths, 2, sum)
  deaths_male <- apply(deaths[97:192, ], 2, sum)
  deaths_female <- apply(deaths[1:96, ], 2, sum)

  e0 <- lt_out_both[1, ]
  e0_male <- lt_out_mle[1, ]
  e0_female <- lt_out_fmle[1, ]

  imr <- lt_out_both[2, ]
  u5mr <- lt_out_both[3, ]

  locs <- loc_table %>%
    filter(location_name == is) %>%
    select(location_iso3, location_name)
  out_df <- data.table(
    year = y0:y1,
    deaths_both = deaths_both,
    deaths_male = deaths_male,
    deaths_female = deaths_female,
    e0 = e0,
    e0_male = e0_male,
    e0_female = e0_female,
    imr = imr, u5mr = u5mr, births = births, locs
  )

  return(out_df)
}

#' @param obs_wpp Observed WPP data
add_obs <- function(df, obs_wpp, is, y0, y1) {
  out_df <- df %>%
  mutate(group = "CCPM") %>%
  rbind(
    obs_wpp %>%
    filter(location_name == is & year %in% y0:y1) %>%
    mutate(group = "WPP2019") %>%
    select(-c("location_id"))
  )

  return(out_df)
}

#' Calculate all single-year deaths
#' @param y0 Start year of projection
#' @param y1 End year of projection
#' @param wpp_input Input WPP data
#' @return A data.table with single-year deaths
get_all_deaths <- function(y0, y1, wpp_input) {
  locsall <- loc_table %>%
    filter(location_name %in% unique(wpp_input$location_name)) %>%
    select(location_iso3, location_name) %>%
    arrange(location_iso3)

  isc <- locsall$location_name
  isco <- locsall$location_iso3
  isn <- length(isc)

  all_deaths <- rbindlist(
    parallel::mclapply(1:isn, function(c) {
      is   <- isc[c]
      iso  <- isco[c]

      out  <- project_pop(is, y0, y1, wpp_input)$deaths %>%
        as.data.table()
      n    <- y1 - y0 + 1

      yrv  <- paste0(y0:y1)
      sxv  <- rep(c("Female", "Male"), each = 96)
      agv  <- c(0:95, 0:95)

      colnames(out) <- yrv

      out <- out %>%
        mutate(age = agv, sex_name = sxv) %>%
        tidyr::gather(year_id, deaths, -age, -sex_name) %>%
        mutate(
          year_id = as.numeric(year_id),
          location_name = is,
          location_iso3 = iso
        ) %>%
        arrange(sex_name, age, year_id)

      return(out)
    }, mc.cores = parallel::detectCores())
  )

  return(all_deaths)
}