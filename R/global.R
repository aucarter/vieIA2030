get_ccpm <- function(nx, sx, fx, mig, z, n) {
  nxf <- nx[1:z, ]
  nxm <- nx[(z + 1):(2 * z), ]
  sxf <- sx[1:z, ]
  sxm <- sx[(z + 1):(2 * z), ]
  migf <- mig[1:z, ]
  migm <- mig[(z + 1):(2 * z), ]
  srb <- 1.05

  bxm  <- bxf <- array(dim = n)
  pm  <- pf <- array(dim = c(z, n + 1))
  pf[, 1] <- nxf[, 1]
  pm[, 1] <- nxm[, 1]

  for (i in 1:n) {
    pf[2:(z - 1), i + 1] <- pf[1:(z - 2), i] * (
        sxf[1:(z - 2), i] * (1 + .5 * migf[2:(z - 1), i]) +
        .5 * migf[2:(z - 1), i]
      )

    pf[z, i + 1] <- pf[z - 1, i] * (
        sxf[z - 1, i] * (1 + .5 * migf[z, i]) +
        .5 * migf[z, i]
      ) +
      pf[z, i] * (
        sxf[z, i] * (1 + .5 * migf[z, i]) +
        .5 * migf[z, i]
      )

    fxbf <- ((1 + srb) ^ (-1) * (
        fx[10:54, i] +
        fx[11:55, i] * sxf[11:55, i]
      ) * 0.5)

    bxf[i] <- sum(
        fxbf * pf[10:54, i] * ((1 + .5 * migf[10:54, i]) + .5 * migf[10:54, i])
      )

    pf[1, i + 1] <- bxf[i] * (sxf[1, i] * (1 + .5 * migf[1, i]) +
      .5 * migf[1, i])
  }

  for (i in 1:n) {
    pm[2:(z - 1), i + 1] <- pm[1:(z - 2), i] * (
        sxm[1:(z - 2), i] * (1 + .5 * migm[2:(z - 1), i]) +
        .5 * migm[2:(z - 1), i]
      )

    pm[z, i + 1] <- pm[z - 1, i] * (
        sxm[z - 1, i] * (1 + .5 * migm[z, i]) +
        .5 * migm[z, i]
      ) +
      pm[z, i] * (
        sxm[z, i] * (1 + .5 * migm[z, i]) +
        .5 * migm[z, i]
      )

    fxbm <- (srb * (1 + srb) ^ (-1) * (
        fx[10:54, i] +
        fx[11:55, i] * sxf[11:55, i]
      ) * 0.5)
    bxm[i] <- sum(
        fxbm * pf[10:54, i] * ((1 + .5 * migf[10:54, i]) + .5 * migf[10:54, i])
      )
    pm[1, i + 1] <- bxm[i] * (sxm[1, i] * (1 + .5 * migm[1, i]) +
      .5 * migm[1, i])
  }

  ccpm <- list(population = rbind(pf, pm)[, 1:n], births = bxf + bxm)

  return(ccpm)
}

get_person <- function(x) {
  x[1:96] + x[-c(1:96)]
}

lt_est <- function(y) {
  px <- exp(-y)
  lx <- c(1, cumprod(px))
  ex <- round(sum(head(lx, -1) + tail(lx, -1)) / 2, 3)
  q5 <- 1000 * (1 - lx[6])
  q1 <- 1000 * (1 - lx[2])
  c(ex, q1, q5)
}

project_pop <- function(is, y0, y1) {
  data(wpp_input)
  data(obs_wpp)

  wpp_ina <- wpp_input  %>%
    filter(location_name == is & year_id %in% y0:(y1 + 1)) %>%
    select(-c(location_name)) %>%
    arrange(sex_name, age, year_id)
  fx <- wpp_ina %>%
    select(sex_name, age, year_id, fx) %>%
    tidyr::spread(year_id, fx) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
  nx <- wpp_ina %>%
    select(sex_name, age, year_id, nx) %>%
    tidyr::spread(year_id, nx) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
  nx[nx == 0]   <- 1e-09
  mig <- wpp_ina %>%
    select(sex_name, age, year_id, mig) %>%
    tidyr::spread(year_id, mig) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
  mx <- wpp_ina %>%
    select(sex_name, age, year_id, mx) %>%
    tidyr::spread(year_id, mx) %>%
    select(-c(sex_name, age)) %>%
    as.matrix()
  mx[mx == 0] <- 1e-09
  sx <- exp(-mx)
  n <- y1 - y0 + 1
  z <- length(0:95)

  ccpm_res <- get_ccpm(nx, sx, fx, mig, z, n)
  population <- ccpm_res[["population"]]
  births <- ccpm_res[["births"]]
  deaths <- -1 * (log(sx)[, 1:n]) * population

  pop_tot <- apply(population, 2, get_person)
  deaths_tot <- apply(deaths, 2, get_person)
  mx_both  <- deaths_tot / pop_tot

  lt_out_both <- apply(mx_both, 2, lt_est)
  lt_out_fmle <- apply(mx[1:96, 1:n], 2, lt_est)
  lt_out_mle <- apply(mx[97:192, 1:n], 2, lt_est)

  deaths_both <- apply(deaths, 2, sum)
  deaths_male <- apply(deaths[97:192, ], 2, sum)
  deaths_female <- apply(deaths[1:96, ], 2, sum)

  e0 <- lt_out_both[1, ]
  e0_male <- lt_out_mle[1, ]
  e0_female <- lt_out_fmle[1, ]

  imr <- lt_out_both[2, ]
  u5mr <- lt_out_both[3, ]

  locs <- loc %>%
    filter(location_name == is) %>%
    select(iso3, location_name)

  out_df <- data.table::data.table(
    group = "CCPM",
    year = y0:y1,
    deaths_both = deaths_both,
    deaths_male = deaths_male,
    deaths_female = deaths_female,
    e0 = e0,
    e0_male = e0_male,
    e0_female = e0_female,
    imr = imr, u5mr = u5mr, births = births, locs
  ) %>%
  rbind(
    obs_wpp %>%
    filter(location_name == is & year %in% y0:y1) %>%
    mutate(group = "WPP2019")
  )

  list(population = population, deaths = deaths, out_df = out_df)
}

get_all_deaths <- function(y0, y1) {
  data(loc)
  data(wpp_input)

  locsall <- loc %>%
    filter(location_name %in% unique(wpp_input$location_name)) %>%
    select(iso3, location_name) %>%
    arrange(iso3)

  isc <- locsall$location_name
  isco <- locsall$iso3
  isn <- length(isc)
  death_list <- list(isn)

  for (c in 1:isn) {
    is   <- isc[c]
    iso  <- isco[c]

    print(paste(c, "of", isn, iso))

    out  <- project_pop(is, y0, y1)$deaths %>%
      data.table::as.data.table()
    n    <- y1 - y0 + 1

    yrv  <- paste0(y0 + 1:n)
    sxv  <- rep(c("Female", "Male"), each = 96)
    agv  <- c(0:95, 0:95)

    colnames(out) <- yrv

    out <- out %>%
      mutate(age = agv, sex_name = sxv) %>%
      tidyr::gather(year_id, deaths, -age, -sex_name) %>%
      mutate(
        year_id = as.numeric(year_id),
        location_name = is,
        iso3 = iso
      ) %>%
      arrange(sex_name, age, year_id)

    death_list[[c]] <- out
  }

  all_deaths <- data.table::rbindlist(death_list)

  return(all_deaths)
}