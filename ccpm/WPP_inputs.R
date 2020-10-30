############################################################################################################
# House keeping
rm(list = ls(all = TRUE)) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(wpp2019)
library(dplyr)
library(tidyr)
library(data.table)
library(demography)
library(readxl)
library(pspline)

##############################################################################################################


'%!in%' <- function(x,y)!('%in%'(x,y))

split.rate <- function(mx){
  pop <- log(mx)
  pop[pop < -13] <- -13
  pop[pop > -0.0001] <- -0.0001
  m1 <- predict(smooth.Pspline(c(0, 2, seq(7,97,5), 100), pop[1:22],  spar = 0.1), 0:100)
  m2 <- predict(smooth.Pspline(c(0, 2, seq(7,97,5), 100), pop[23:44], spar = 0.1), 0:100)
  pop2 <- c(m1,m2)
  pop2[pop2 > -0.0001] <- -0.0001
  exp(pop2)
}

###############################################################################################################

loc             <- fread("Data/loc.csv") 
agem            <- tibble(agen=c(seq(0,100,5)), agec = c(paste0(seq(0,95,5),"-",seq(4,99,5)),"100+"))

###############################################################################################################


###############################################################################################################
# Population numbers 
###############################################################################################################

f1 <- "Data/WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"
f2 <- "Data/WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"

get.x <- function(fl, rn, sx, my){
  if (rn==1){
    sh = "ESTIMATES"; rh="A17:DE18122"
  } else {
    sh = "MEDIUM VARIANT"; rh="A17:DE20672"
  }

read_excel(fl, sheet = sh, range = rh) %>% mutate(sex_name = sx) %>% 
  select(-c("Index","Variant", "Region, subregion, country or area *", "Notes", "Type", "Parent code")) %>%
  rename(year_id = `Reference date (as of 1 July)`, country_code = `Country code`) %>%  
  gather(age, val, -country_code, -sex_name, -year_id) %>% filter(year_id > 1979 & year_id < my) %>%
  mutate(age = as.numeric(age), val = val*1000) %>% 
  group_by(country_code, year_id, sex_name, age) %>% summarise(Nx = sum(val)) %>% ungroup()
}

pm1 <- get.x(f1, 1, "Male", 2020) 
pm2 <- get.x(f1, 2, "Male", 2097)
pf1 <- get.x(f2, 1, "Female", 2020)
pf2 <- get.x(f2, 2, "Female", 2097)
wpppop <- rbind(pm1, pm2, pf1, pf2)

###############################################################################################################
# mx rates 
###############################################################################################################

data(mxF)
data(mxM)

mxF     <- mxF %>% gather(year, mx, -country_code, -name, -age) %>%
  mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>% filter(year > 1977) %>% 
  spread(year, mx) %>% select(-c(name)) %>% mutate(sex_name = "Female")
mxM     <- mxM %>% distinct() %>% gather(year, mx, -country_code, -name, -age) %>%
  mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>% filter(year > 1977) %>% 
  spread(year, mx) %>% select(-c(name)) %>% mutate(sex_name = "Male")
wppmx42   <- rbind(mxF, mxM)
for (j in seq(1977.5, 2092.5, 5)){
for (i in (j+0.5):(j + 4.5)){
  k = j+5
eval(parse(text=paste(paste("wppmx42 <- wppmx42 %>% mutate(`",i,"` = `",j,
                            "`*exp((i - j)*1/5*log(`",k,"`/`",j,"`)))", 
                            sep=""), collapse=";")))
}
}
wppmx42 <- wppmx42 %>% select(country_code, sex_name, age, paste0(1980:2096)) 

wppmx <- data.table(sex_name = character(), country_code = integer(), age = integer(), year_id = integer(), mx = integer())
for (code in unique(wppmx42$country_code)){
  print(paste0(code))
    dpred <- wppmx42 %>% filter(country_code==code) %>% arrange(sex_name, age) %>%
      select(-c(country_code, sex_name, age)) %>% as.matrix()
    dpred[is.nan(dpred)|is.na(dpred)] <- 0.5
    dpred_mat <- apply(dpred, 2, split.rate)
    dpred_matb <- dpred_mat %>% data.table() %>% 
      mutate(sex_name = rep(c("Female","Male"), each = 101), country_code = code, age = rep(0:100, 2)) %>% 
      gather(year, mx, -country_code, -sex_name, -age) %>%
      mutate(year = as.numeric(year)) %>% rename(year_id = year)
    wppmx = rbind(wppmx, dpred_matb)
}

wpp.in <- left_join(wpppop, loc, by = "country_code") %>% 
  left_join(wppmx, by = c("country_code", "sex_name", "year_id", "age")) %>%
  mutate(dx = mx*Nx, age = ifelse(age>95, 95, age)) %>% 
  group_by(sex_name, year_id, age, country_code, location_name) %>%
  summarise(Nx = sum(Nx, na.rm=T), dx = sum(dx, na.rm=T)) %>% ungroup() %>% 
  mutate(mx = ifelse(Nx==0, 0, dx/Nx))

###############################################################################################################

###############################################################################################################
# tfrs
###############################################################################################################

data("tfr")
data("tfrprojMed")

tfra     <- tfr %>% select(-c(last.observed)) %>% gather(year, tfr, -country_code, -name) %>%
  mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>% filter(year > 1977) %>% 
  spread(year, tfr) %>% select(-c(name))
tfrb     <-tfrprojMed %>% gather(year, tfr, -country_code, -name) %>%
  mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>% filter(year > 1977) %>% 
  spread(year, tfr) %>% select(-c(name))
tfrall   <- left_join(tfra, tfrb, by = "country_code")
for (j in seq(1977.5, 2092.5, 5)){
  for (i in (j+0.5):(j + 4.5)){
    k = j+5
    eval(parse(text=paste(paste("tfrall <- tfrall %>% mutate(`",i,"` = `",j,
                                "`*exp((i - j)*1/5*log(`",k,"`/`",j,"`)))", 
                                sep=""), collapse=";")))
  }
}
tfrall <- tfrall %>% select(country_code, paste0(1980:2096)) %>%
  gather(year, tfr, -country_code) %>% mutate(year = as.numeric(year))

###############################################################################################################
# asfrs
###############################################################################################################

data("percentASFR")

agef     <- tibble(agec = paste0(seq(15,45,5),"-",seq(15,45,5)+4), 
                   age = seq(15,45,5))
asfr     <- percentASFR %>% gather(year, afr, -country_code, -name, -age) %>%
  mutate(year = as.numeric(substr(year, 1, 4)) + 2.5) %>% filter(year > 1977) %>% 
  spread(year, afr)  %>% rename(agec = age) %>% mutate(agec = paste0(agec)) %>%
  left_join(agef, by = "agec") %>% select(-c(name, agec)) %>%
  arrange(country_code, age)

age.asfr <- tibble(agen = rep(seq(15,45,5), each = 5), age = 15:49)

for (j in seq(1977.5, 2092.5, 5)){
  for (i in (j+0.5):(j + 4.5)){
    k = j+5
    eval(parse(text=paste(paste("asfr <- asfr %>% mutate(`",i,"` = `",j,
                                "`*exp((i - j)*1/5*log(`",k,"`/`",j,"`)))", 
                                sep=""), collapse=";")))
  }
}
asfrall <- asfr %>% select(country_code, age, paste0(1980:2096)) %>% rename(agen = age) %>%
  left_join(age.asfr, by = "agen") %>% select(-c(agen)) %>%
  gather(year, fx, -country_code, -age) %>% group_by(country_code, year) %>% mutate(sfr = sum(fx, na.rm=T)) %>% ungroup() %>%
  mutate(asfr = fx/sfr)  %>% select(country_code, age, year, asfr) %>% mutate(year = as.numeric(year))

wppfx <- left_join(tfrall, asfrall, by = c("country_code","year")) %>% 
  mutate(fx = asfr*tfr) %>% select(country_code, age, year, fx) %>%
  spread(year, fx) %>% arrange(country_code, age) %>% gather(year_id, fx, -age, -country_code) %>%
  mutate(sex_name = "Female", year_id = as.numeric(year_id))

#############################################################################################################################
#############################################################################################################################

wpp.in   <- wpp.in %>%
  left_join(wppfx, by = c("country_code", "sex_name", "year_id", "age")) %>% 
  mutate(fx = ifelse(is.na(fx), 0, fx)) %>% 
  select(location_name, sex_name, age, year_id, Nx, mx, fx) %>%
  filter(!is.na(sex_name) & !is.na(location_name)) %>% arrange(location_name, sex_name, age) %>%
  left_join(loc %>% select(location_name, iso3), by = "location_name")

#############################################################################################################################
# Get migrants 

get_mig <- function(is, Nx, sx, fx, z){
Nxf   <- Nx[1:z,];  Nxm   <- Nx[(z+1):(2*z),]
sxf   <- sx[1:z,];  sxm   <- sx[(z+1):(2*z),]

n      <- ncol(Nx) - 1
migm   <- migf<- array(dim = c(z, n))
srb    <- 1.05

# Get migration

# Females
for (i in 1:n){
  migf[2:(z-1), i]      <- 2*(Nxf[2:(z-1), i+1] - Nxf[1:(z-2), i]* sxf[1:(z-2), i])/(Nxf[1:(z-2), i]*(1 + sxf[1:(z-2), i]))
  migf[z, i]            <- migf[z-1, i]
  fxbf                  <- ((1 + srb)^(-1)*(fx[10:54, i] + fx[11:55, i]*sxf[11:55, i])*0.5)
  Bxfs                  <- sum(fxbf*Nxf[10:54, i] * ((1 + .5*migf[10:54, i]) + .5*migf[10:54, i]))
  migf[1, i]            <- 2*(Nxf[1, i+1]/Bxfs - sxf[1, i])/(1 + sxf[1, i])
}

# Males
for (i in 1:n){
  migm[2:(z-1), i]      <- 2*(Nxm[2:(z-1), i+1] - Nxm[1:(z-2), i]* sxm[1:(z-2), i])/(Nxm[1:(z-2), i]*(1 + sxm[1:(z-2), i]))
  migm[z, i]            <- migm[z-1, i]
  fxbm                  <- (srb*(1 + srb)^(-1)*(fx[10:54, i] + fx[11:55, i]*sxf[11:55, i])*0.5)
  Bxms                  <- sum(fxbm*Nxf[10:54, i] * ((1 + .5*migf[10:54, i]) + .5*migf[10:54, i]))
  migm[1, i]            <- 2*(Nxm[1, i+1]/Bxms - sxm[1, i])/(1 + sxm[1, i])
}

rbind(migf, migm)
}

# loop through pulling migration
isc <- sort(unique(wpp.in$location_name))
isn <- length(isc)
wpp.in.list <- list(isn)

for (c in 1:isn){
  is        <- isc[c]
wpp.ina     <- wpp.in  %>% filter(location_name == is) %>% arrange(sex_name, age, year_id)
fx          <- wpp.ina %>% select(sex_name, age, year_id, fx) %>% spread(year_id, fx) %>% 
  select(-c(sex_name, age)) %>% as.matrix()
Nx          <- wpp.ina %>% select(sex_name, age, year_id, Nx) %>% spread(year_id, Nx) %>% 
  select(-c(sex_name, age)) %>% as.matrix()
Nx[Nx==0]   <- 1e-09
mx          <- wpp.ina %>% select(sex_name, age, year_id, mx) %>% spread(year_id, mx) %>% 
  select(-c( sex_name, age)) %>% as.matrix()
mx[mx==0]   <- 1e-09
sx          <- exp(-mx)
z           <- length(0:95)

migs <- get_mig(is, Nx, sx, fx, z) %>% as.data.table()

yrv  <- paste0(1979 + 1:n)
sxv  <- rep(c("Female","Male"), each = 96)
agv  <- c(0:95, 0:95)

colnames(migs) <- yrv

migs <- migs %>%
  mutate(age = agv, sex_name = sxv) %>%
  gather(year_id, mig, -age, -sex_name) %>% mutate(year_id = as.numeric(year_id)) %>%
  arrange(sex_name, age, year_id)

wpp.in.list[[c]] <- right_join(wpp.ina, migs, by = c("year_id", "sex_name", "age")) %>%
  select(location_name, iso3, sex_name, age, year_id, Nx, mx, fx, mig)
}

wpp.input <- rbindlist(wpp.in.list)

####################################################################################################################################

obs_wpp  <- fread("Data/obs_wpp_demo.csv") %>%
  filter(!(variant == "Medium variant" & year == 2020)) %>%
  select(-c(variant)) %>% filter(year %in% 1980:2095) %>%
  right_join(loc, by = "country_code") %>% select(-c(country_code)) %>%
  filter(!is.na(year)) %>%
  mutate(deaths_both   = deaths_both*1e03,
         deaths_male   = deaths_male*1e03,
         deaths_female = deaths_female*1e03,
         births        = births*1e03)
                        

save(wpp.input, obs_wpp, loc, file = "wppinput.Rda")
