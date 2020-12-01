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

get_mx <- function() {
  data(mxF, package = "wpp2019")
  data(mxM, package = "wpp2019")

  mx_f <- mxF %>%
    tidyr::gather(year, mx, -country_code, -name, -age) %>%
    mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    filter(year > 1977) %>%
    tidyr::spread(year, mx) %>%
    select(-c(name)) %>%
    mutate(sex_name = "Female")
  mx_m <- mxM %>%
    distinct() %>%
    tidyr::gather(year, mx, -country_code, -name, -age) %>%
    mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    filter(year > 1977) %>%
    tidyr::spread(year, mx) %>%
    select(-c(name)) %>%
    mutate(sex_name = "Male")
  wppmx42   <- rbind(mx_f, mx_m)
  for (j in seq(1977.5, 2092.5, 5)) {
    for (i in (j + 0.5):(j + 4.5)) {
      k <- j + 5
      eval(
        parse(
          text = paste(
            paste(
              "wppmx42 <- wppmx42 %>% mutate(`",
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
    select(country_code, sex_name, age, paste0(1980:2096))

  wppmx <- data.table(
    sex_name = character(),
    country_code = integer(),
    age = integer(),
    year_id = integer(),
    mx = integer()
  )
  for (code in unique(wppmx42$country_code)) {
    dpred <- wppmx42 %>%
      filter(country_code == code) %>%
      arrange(sex_name, age) %>%
      select(-c(country_code, sex_name, age)) %>%
      as.matrix()
    dpred[is.nan(dpred) | is.na(dpred)] <- 0.5
    dpred_mat <- apply(dpred, 2, split_rate)
    dpred_matb <- dpred_mat %>%
      data.table() %>%
      mutate(
        sex_name = rep(c("Female", "Male"), each = 101),
        country_code = code,
        age = rep(0:100, 2)
      ) %>%
      tidyr::gather(year, mx, -country_code, -sex_name, -age) %>%
      mutate(year = as.numeric(year)) %>%
      rename(year_id = year)
    wppmx <- rbind(wppmx, dpred_matb)
  }
  wppmx <- wppmx %>% rename(wpp_location_code = country_code)

  return(wppmx)
}

get_fx <- function() {
  data("tfr", package = "wpp2019")
  data("tfrprojMed", package = "wpp2019")

  tfra <- tfr %>%
    select(-c(last.observed)) %>%
    tidyr::gather(year, tfr, -country_code, -name) %>%
    mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    filter(year > 1977) %>%
    tidyr::spread(year, tfr) %>%
    select(-c(name))
  tfrb <- tfrprojMed %>%
    tidyr::gather(year, tfr, -country_code, -name) %>%
    mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    filter(year > 1977) %>%
    tidyr::spread(year, tfr) %>%
    select(-c(name))
  tfrall   <- left_join(tfra, tfrb, by = "country_code")
  for (j in seq(1977.5, 2092.5, 5)) {
    for (i in (j + 0.5):(j + 4.5)) {
      k <- j + 5
      eval(
        parse(
          text = paste(
            paste(
              "tfrall <- tfrall %>% mutate(`",
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
    select(country_code, paste0(1980:2096)) %>%
    tidyr::gather(year, tfr, -country_code) %>%
    mutate(year = as.numeric(year))

  data("percentASFR", package = "wpp2019")

  agef <- tibble(
    agec = paste0(seq(15, 45, 5), "-", seq(15, 45, 5) + 4),
    age = seq(15, 45, 5)
  )
  asfr <- percentASFR %>%
    tidyr::gather(year, afr, -country_code, -name, -age) %>%
    mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>%
    filter(year > 1977) %>%
    tidyr::spread(year, afr)  %>%
    rename(agec = age) %>%
    mutate(agec = paste0(agec)) %>%
    left_join(agef, by = "agec") %>%
    select(-c(name, agec)) %>%
    arrange(country_code, age)

  age_asfr <- tibble(agen = rep(seq(15, 45, 5), each = 5), age = 15:49)

  for (j in seq(1977.5, 2092.5, 5)) {
    for (i in (j + 0.5):(j + 4.5)) {
      k <- j + 5
      eval(
        parse(
          text = paste(
            paste(
              "asfr <- asfr %>% mutate(`",
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
    select(country_code, age, paste0(1980:2096)) %>%
    rename(agen = age) %>%
    left_join(age_asfr, by = "agen") %>%
    select(-c(agen)) %>%
    tidyr::gather(year, fx, -country_code, -age) %>%
    group_by(country_code, year) %>%
    mutate(sfr = sum(fx, na.rm = T)) %>%
    ungroup() %>%
    mutate(asfr = fx / sfr)  %>%
    select(country_code, age, year, asfr) %>%
    mutate(year = as.numeric(year))

  wppfx <- left_join(tfrall, asfrall, by = c("country_code", "year")) %>%
    mutate(fx = asfr * tfr) %>%
    select(country_code, age, year, fx) %>%
    tidyr::spread(year, fx) %>%
    arrange(country_code, age) %>%
    tidyr::gather(year_id, fx, -age, -country_code) %>%
    mutate(sex_name = "Female", year_id = as.numeric(year_id))

  wppfx <- wppfx %>% rename(wpp_location_code = country_code)

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
      (1 + .5 * migf[10:54, i])
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
      (1 + .5 * migf[10:54, i])
    )
    migm[1, i] <- 2 * (nxm[1, i + 1] / bxms - sxm[1, i]) / (1 + sxm[1, i])
  }

  mig <- rbind(migf, migm)

  return(mig)
}

agem <- tibble(
  agen = c(seq(0, 100, 5)),
  agec = c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+")
)

load(
  system.file("extdata", "wpp_input_pop.RData", package = "vieIA2030")
)
wpp_input_pop <- wpp_input_pop %>%
    left_join(
        data.table(sex_id = 1:2, sex_name = c("Male", "Female")),
        by = "sex_id"
    ) %>%
    select(-c("sex_id"))
wppmx <- get_mx()
wpp_in <- left_join(wpp_input_pop, loc_table, by = "wpp_location_code") %>%
  left_join(
    wppmx,
    by = c("wpp_location_code", "sex_name", "year_id", "age")
  ) %>%
  mutate(dx = mx * nx, age = ifelse(age > 95, 95, age)) %>%
  group_by(sex_name, year_id, age, wpp_location_code, location_name) %>%
  summarise(nx = sum(nx, na.rm = T), dx = sum(dx, na.rm = T)) %>%
  ungroup() %>%
  mutate(mx = ifelse(nx == 0, 0, dx / nx))

wppfx <- get_fx()
wpp_in   <- wpp_in %>%
  left_join(
    wppfx,
    by = c("wpp_location_code", "sex_name", "year_id", "age")
  ) %>%
  mutate(fx = ifelse(is.na(fx), 0, fx)) %>%
  select(location_name, sex_name, age, year_id, nx, mx, fx) %>%
  filter(!is.na(sex_name) & !is.na(location_name)) %>%
  arrange(location_name, sex_name, age) %>%
  left_join(
    loc_table %>% select(location_name, location_iso3),
    by = "location_name"
  )

# loop through pulling migration
isc <- sort(unique(wpp_in$location_name))
isn <- length(isc)
wpp_in_list <- list(isn)

for (c in 1:isn) {
  is <- isc[c]
  wpp_ina <- wpp_in  %>%
    filter(location_name == is) %>%
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
  nx[nx == 0] <- 1e-09
  mx <- wpp_ina %>%
    select(sex_name, age, year_id, mx) %>%
    tidyr::spread(year_id, mx) %>%
    select(-c(sex_name, age)) %>%
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
    mutate(age = agv, sex_name = sxv) %>%
    tidyr::gather(year_id, mig, -age, -sex_name) %>%
    mutate(year_id = as.numeric(year_id)) %>%
    arrange(sex_name, age, year_id)

  wpp_in_list[[c]] <- right_join(
      wpp_ina,
      migs,
      by = c("year_id", "sex_name", "age")
    ) %>%
    select(location_name, location_iso3, sex_name, age, year_id, nx, mx, fx, mig)
}

wpp_input <- rbindlist(wpp_in_list)
wpp_input <- wpp_input %>%
  right_join(
    loc_table[, .(location_id, location_iso3)],
    by = "location_iso3"
  ) %>%
  select(-c("location_name", "location_iso3")) %>%
  filter(!is.na(nx))

# Calculate both-sexes deaths
temp_wpp_input <- merge(wpp_input, loc_table)
deaths <- get_all_deaths(2000, 2030, temp_wpp_input)
deaths <- merge(deaths, loc_table[, .(location_iso3, location_id)])
deaths[, c("location_name", "location_iso3") := NULL]

mydb <- open_connection()
DBI::dbWriteTable(mydb, "wpp_input", wpp_input, overwrite = TRUE)
DBI::dbWriteTable(mydb, "all_deaths", deaths, overwrite = TRUE)
DBI::dbDisconnect(mydb)
