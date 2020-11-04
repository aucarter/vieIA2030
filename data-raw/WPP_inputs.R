f1 <- system.file(
  "extdata",
  "WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx",
  package = "vieIA2030"
)
f2 <- system.file(
  "extdata",
  "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx",
  package = "vieIA2030"
)
f3 <- system.file(
  "extdata",
  "loc.csv",
  package = "vieIA2030"
)
f4 <- system.file(
  "extdata",
  "obs_wpp_demo.csv",
  package = "vieIA2030"
)

split_rate <- function(mx) {
  pop <- log(mx)
  pop[pop < -13] <- -13
  pop[pop > -0.0001] <- -0.0001
  m1 <- predict(
    pspline::smooth.Pspline(
      c(0, 2, seq(7, 97, 5), 100),
      pop[1:22],
      spar = 0.1
    ),
    0:100
  )
  m2 <- predict(
    pspline::smooth.Pspline(
      c(0, 2, seq(7, 97, 5), 100),
      pop[23:44], spar = 0.1
    ),
    0:100
  )
  pop2 <- c(m1, m2)
  pop2[pop2 > -0.0001] <- -0.0001
  exp(pop2)
}

get_x <- function(fl, rn, sx, my) {
  if (rn == 1) {
    sh <- "ESTIMATES"
    rh <- "A17:DE18122"
  } else {
    sh <- "MEDIUM VARIANT"
    rh <- "A17:DE20672"
  }

  x <- readxl::read_excel(fl, sheet = sh, range = rh) %>%
    dplyr::mutate(sex_name = sx) %>%
    dplyr::select(
      -c("Index", "Variant", "Region, subregion, country or area *", "Notes",
          "Type", "Parent code")
    ) %>%
    dplyr::rename(
      year_id = `Reference date (as of 1 July)`,
      country_code = `Country code`
    ) %>%
    tidyr::gather(age, val, -country_code, -sex_name, -year_id) %>%
    dplyr::filter(year_id > 1979 & year_id < my) %>%
    dplyr::mutate(age = as.numeric(age), val = val * 1000) %>%
    dplyr::group_by(country_code, year_id, sex_name, age) %>%
    dplyr::summarise(nx = sum(val)) %>%
    dplyr::ungroup()

  return(x)
}

get_pop <- function(f1, f2) {
  pm1 <- get_x(f1, 1, "Male", 2020)
  pm2 <- get_x(f1, 2, "Male", 2097)
  pf1 <- get_x(f2, 1, "Female", 2020)
  pf2 <- get_x(f2, 2, "Female", 2097)
  wpppop <- rbind(pm1, pm2, pf1, pf2)

  return(wpppop)
}

get_mx <- function() {
  data(mxF, package = "wpp2019")
  data(mxM, package = "wpp2019")

  mx_f <- mxF %>%
    tidyr::gather(year, mx, -country_code, -name, -age) %>%
    dplyr::mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    dplyr::filter(year > 1977) %>%
    tidyr::spread(year, mx) %>%
    dplyr::select(-c(name)) %>%
    dplyr::mutate(sex_name = "Female")
  mx_m <- mxM %>%
    dplyr::distinct() %>%
    tidyr::gather(year, mx, -country_code, -name, -age) %>%
    dplyr::mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    dplyr::filter(year > 1977) %>%
    tidyr::spread(year, mx) %>%
    dplyr::select(-c(name)) %>%
    dplyr::mutate(sex_name = "Male")
  wppmx42   <- rbind(mx_f, mx_m)
  for (j in seq(1977.5, 2092.5, 5)) {
    for (i in (j + 0.5):(j + 4.5)) {
      k <- j + 5
      eval(
        parse(
          text = paste(
            paste(
              "wppmx42 <- wppmx42 %>% dplyr::mutate(`",
              i,
              "` = `",
              j,
              "`*exp((i - j)*1/5*log(`",
              k,
              "`/`",
              j,
              "`)))",
              sep = ""
            ),
            collapse = ";"
          )
        )
      )
    }
  }
  wppmx42 <- wppmx42 %>%
    dplyr::select(country_code, sex_name, age, paste0(1980:2096))

  wppmx <- data.table(
    sex_name = character(),
    country_code = integer(),
    age = integer(),
    year_id = integer(),
    mx = integer()
  )
  for (code in unique(wppmx42$country_code)) {
    print(paste0(code))
    dpred <- wppmx42 %>%
      dplyr::filter(country_code == code) %>%
      dplyr::arrange(sex_name, age) %>%
      dplyr::select(-c(country_code, sex_name, age)) %>%
      as.matrix()
    dpred[is.nan(dpred) | is.na(dpred)] <- 0.5
    dpred_mat <- apply(dpred, 2, split_rate)
    dpred_matb <- dpred_mat %>%
      data.table() %>%
      dplyr::mutate(
        sex_name = rep(c("Female", "Male"), each = 101),
        country_code = code,
        age = rep(0:100, 2)
      ) %>%
      tidyr::gather(year, mx, -country_code, -sex_name, -age) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::rename(year_id = year)
    wppmx <- rbind(wppmx, dpred_matb)
  }

  return(wppmx)
}

get_fx <- function() {
  data("tfr", package = "wpp2019")
  data("tfrprojMed", package = "wpp2019")

  tfra <- tfr %>%
    dplyr::select(-c(last.observed)) %>%
    tidyr::gather(year, tfr, -country_code, -name) %>%
    dplyr::mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    dplyr::filter(year > 1977) %>%
    tidyr::spread(year, tfr) %>%
    dplyr::select(-c(name))
  tfrb <- tfrprojMed %>%
    tidyr::gather(year, tfr, -country_code, -name) %>%
    dplyr::mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    dplyr::filter(year > 1977) %>%
    tidyr::spread(year, tfr) %>%
    dplyr::select(-c(name))
  tfrall   <- dplyr::left_join(tfra, tfrb, by = "country_code")
  for (j in seq(1977.5, 2092.5, 5)) {
    for (i in (j + 0.5):(j + 4.5)) {
      k <- j + 5
      eval(
        parse(
          text = paste(
            paste(
              "tfrall <- tfrall %>% dplyr::mutate(`",
              i,
              "` = `",
              j,
              "`*exp((i - j)*1/5*log(`",
              k,
              "`/`",
              j,
              "`)))",
              sep = ""
            ),
            collapse = ";"
          )
        )
      )
    }
  }
  tfrall <- tfrall %>%
    dplyr::select(country_code, paste0(1980:2096)) %>%
    tidyr::gather(year, tfr, -country_code) %>%
    dplyr::mutate(year = as.numeric(year))

  data("percentASFR", package = "wpp2019")

  agef <- dplyr::tibble(
    agec = paste0(seq(15, 45, 5), "-", seq(15, 45, 5) + 4),
    age = seq(15, 45, 5)
  )
  asfr <- percentASFR %>%
    tidyr::gather(year, afr, -country_code, -name, -age) %>%
    dplyr::mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    dplyr::filter(year > 1977) %>%
    tidyr::spread(year, afr)  %>%
    dplyr::rename(agec = age) %>%
    dplyr::mutate(agec = paste0(agec)) %>%
    dplyr::left_join(agef, by = "agec") %>%
    dplyr::select(-c(name, agec)) %>%
    dplyr::arrange(country_code, age)

  age_asfr <- dplyr::tibble(agen = rep(seq(15, 45, 5), each = 5), age = 15:49)

  for (j in seq(1977.5, 2092.5, 5)) {
    for (i in (j + 0.5):(j + 4.5)) {
      k <- j + 5
      eval(
        parse(
          text = paste(
            paste(
              "asfr <- asfr %>% dplyr::mutate(`",
              i,
              "` = `",
              j,
              "`*exp((i - j)*1/5*log(`",
              k,
              "`/`",
              j,
              "`)))",
              sep = ""
            ),
            collapse = ";"
          )
        )
      )
    }
  }
  asfrall <- asfr %>%
    dplyr::select(country_code, age, paste0(1980:2096)) %>%
    dplyr::rename(agen = age) %>%
    dplyr::left_join(age_asfr, by = "agen") %>%
    dplyr::select(-c(agen)) %>%
    tidyr::gather(year, fx, -country_code, -age) %>%
    dplyr::group_by(country_code, year) %>%
    dplyr::mutate(sfr = sum(fx, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(asfr = fx / sfr)  %>%
    dplyr::select(country_code, age, year, asfr) %>%
    dplyr::mutate(year = as.numeric(year))

  wppfx <- dplyr::left_join(tfrall, asfrall, by = c("country_code", "year")) %>%
    dplyr::mutate(fx = asfr * tfr) %>%
    dplyr::select(country_code, age, year, fx) %>%
    tidyr::spread(year, fx) %>%
    dplyr::arrange(country_code, age) %>%
    tidyr::gather(year_id, fx, -age, -country_code) %>%
    dplyr::mutate(sex_name = "Female", year_id = as.numeric(year_id))

  return(wppfx)
}

get_mig <- function(is, nx, sx, fx, z) {
  nxf <- nx[1:z, ]
  nxm <- nx[(z + 1):(2 * z), ]
  sxf <- sx[1:z, ]
  sxm <- sx[(z + 1):(2 * z), ]

  n <- ncol(nx) - 1
  migm <- migf <- array(dim = c(z, n))
  srb <- 1.05
  # Females
  for (i in 1:n) {
    num <- 2 * (nxf[2:(z - 1), i + 1] - nxf[1:(z - 2), i] * sxf[1:(z - 2), i])
    denom <- (nxf[1:(z - 2), i] * (1 + sxf[1:(z - 2), i]))
    migf[2:(z - 1), i] <- num / denom
    migf[z, i] <- migf[z - 1, i]
    fxbf <- (
      (1 + srb) ^ (-1) *
      (fx[10:54, i] + fx[11:55, i] * sxf[11:55, i]) *
      0.5
    )
    bxfs <- sum(
      fxbf *
      nxf[10:54, i] *
      ((1 + .5 * migf[10:54, i]) + .5 * migf[10:54, i])
    )
    migf[1, i] <- 2 * (nxf[1, i + 1] / bxfs - sxf[1, i]) / (1 + sxf[1, i])
  }

  # Males
  for (i in 1:n) {
    num <- 2 * (nxm[2:(z - 1), i + 1] - nxm[1:(z - 2), i] * sxm[1:(z - 2), i])
    denom <- (nxm[1:(z - 2), i] * (1 + sxm[1:(z - 2), i]))
    migm[2:(z - 1), i] <- num / denom
    migm[z, i] <- migm[z - 1, i]
    fxbm <- (
      srb * (1 + srb) ^ (-1) *
      (fx[10:54, i] + fx[11:55, i] * sxf[11:55, i]) *
      0.5
    )
    bxms <- sum(
      fxbm *
      nxf[10:54, i] *
      ((1 + .5 * migf[10:54, i]) + .5 * migf[10:54, i])
    )
    migm[1, i] <- 2 * (nxm[1, i + 1] / bxms - sxm[1, i]) / (1 + sxm[1, i])
  }

  mig <- rbind(migf, migm)

  return(mig)
}

loc <- data.table::fread(f3)
agem <- dplyr::tibble(
  agen = c(seq(0, 100, 5)),
  agec = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+")
)

wpppop <- get_pop(f1, f2)
wppmx <- get_mx()
wpp_in <- dplyr::left_join(wpppop, loc, by = "country_code") %>%
  dplyr::left_join(
    wppmx,
    by = c("country_code", "sex_name", "year_id", "age")
  ) %>%
  dplyr::mutate(dx = mx * nx, age = ifelse(age > 95, 95, age)) %>%
  dplyr::group_by(sex_name, year_id, age, country_code, location_name) %>%
  dplyr::summarise(nx = sum(nx, na.rm = T), dx = sum(dx, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(mx = ifelse(nx == 0, 0, dx / nx))

wppfx <- get_fx()
wpp_in   <- wpp_in %>%
  dplyr::left_join(
    wppfx,
    by = c("country_code", "sex_name", "year_id", "age")
  ) %>%
  dplyr::mutate(fx = ifelse(is.na(fx), 0, fx)) %>%
  dplyr::select(location_name, sex_name, age, year_id, nx, mx, fx) %>%
  dplyr::filter(!is.na(sex_name) & !is.na(location_name)) %>%
  dplyr::arrange(location_name, sex_name, age) %>%
  dplyr::left_join(
    loc %>% dplyr::select(location_name, iso3),
    by = "location_name"
  )

# loop through pulling migration
isc <- sort(unique(wpp_in$location_name))
isn <- length(isc)
wpp_in_list <- list(isn)

for (c in 1:isn) {
  is <- isc[c]
  wpp_ina <- wpp_in  %>%
    dplyr::filter(location_name == is) %>%
    dplyr::arrange(sex_name, age, year_id)
  fx <- wpp_ina %>%
    dplyr::select(sex_name, age, year_id, fx) %>%
    tidyr::spread(year_id, fx) %>%
    dplyr::select(-c(sex_name, age)) %>%
    as.matrix()
  nx <- wpp_ina %>%
    dplyr::select(sex_name, age, year_id, nx) %>%
    tidyr::spread(year_id, nx) %>%
    dplyr::select(-c(sex_name, age)) %>%
    as.matrix()
  nx[nx == 0] <- 1e-09
  mx <- wpp_ina %>%
    dplyr::select(sex_name, age, year_id, mx) %>%
    tidyr::spread(year_id, mx) %>%
    dplyr::select(-c(sex_name, age)) %>%
    as.matrix()
  mx[mx == 0] <- 1e-09
  sx <- exp(-mx)
  z <- length(0:95)
  n <- ncol(nx) - 1

  migs <- get_mig(is, nx, sx, fx, z) %>% as.data.table()

  yrv  <- paste0(1979 + 1:n)
  sxv  <- rep(c("Female", "Male"), each = 96)
  agv  <- c(0:95, 0:95)

  colnames(migs) <- yrv

  migs <- migs %>%
    dplyr::mutate(age = agv, sex_name = sxv) %>%
    tidyr::gather(year_id, mig, -age, -sex_name) %>%
    dplyr::mutate(year_id = as.numeric(year_id)) %>%
    dplyr::arrange(sex_name, age, year_id)

  wpp_in_list[[c]] <- dplyr::right_join(
      wpp_ina,
      migs,
      by = c("year_id", "sex_name", "age")
    ) %>%
    dplyr::select(location_name, iso3, sex_name, age, year_id, nx, mx, fx, mig)
}

wpp_input <- rbindlist(wpp_in_list)

obs_wpp  <- fread(f4) %>%
  dplyr::filter(!(variant == "Medium variant" & year == 2020)) %>%
  dplyr::select(-c(variant)) %>%
  dplyr::filter(year %in% 1980:2095) %>%
  dplyr::right_join(loc, by = "country_code") %>%
  dplyr::select(-c(country_code)) %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::mutate(
    deaths_both = deaths_both * 1e03,
    deaths_male = deaths_male * 1e03,
    deaths_female = deaths_female * 1e03,
    births = births * 1e03
  )

usethis::use_data(wpp_input, overwrite = TRUE)
usethis::use_data(obs_wpp, overwrite = TRUE)
usethis::use_data(loc, overwrite = TRUE)
