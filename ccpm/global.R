##########################
## load packages
##########################

library(shiny) 
library(shinythemes) 
library(shinydashboard) 
library(DT) 
library(data.table) 
library(dplyr) 
library(tidyr) 
library(highcharter) 
library(progress) 
library(pspline) 
library(MortalityLaws) 

#######################################################################################################################
# Load all data
#######################################################################################################################

load("wppinput.Rda")
locsall   <- loc %>% filter(location_name %in% unique(wpp.input$location_name)) %>% select(iso3, location_name) %>% arrange(iso3)
countries <- locsall$location_name

cols      <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666",
               "#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3")

##########################
## get required functions
##########################

####################################################################################################  
# Run CCPM using default estimates of fertility and survival, and calibrated estimates of migration
# This is the section in which the sx will be changed based on reduction in mx
#################################################################################################### 

get_ccpm <- function(Nx, sx, fx, mig, z, n){
  
  # Sex specific data
  Nxf   <- Nx[1:z,];  Nxm   <- Nx[(z+1):(2*z),]
  sxf   <- sx[1:z,];  sxm   <- sx[(z+1):(2*z),]
  migf  <-mig[1:z,];  migm  <-mig[(z+1):(2*z),]
  srb   <- 1.05
  
  # Empty marrays to place births and populations in
  Bxm  <- Bxf<- array(dim = n)
  Pm   <- Pf <- array(dim = c(z, n+1))
  Pf[,1] = Nxf[,1]
  Pm[,1] = Nxm[,1]
  
  # Females
  for(i in 1:n){
    Pf[2:(z-1),i+1]       <- Pf[1:(z-2),i] * (sxf[1:(z-2), i] * (1 + .5*migf[2:(z-1), i]) + .5*migf[2:(z-1), i])
    Pf[z,i+1]             <- Pf[z-1,i] * (sxf[z-1, i] * (1 + .5*migf[z, i]) + .5*migf[z, i]) +
      Pf[z,i] * (sxf[z, i] * (1 + .5*migf[z, i]) + .5*migf[z, i])
    fxbf                  <- ((1 + srb)^(-1)*(fx[10:54, i] + fx[11:55, i]*sxf[11:55, i])*0.5)
    Bxf[i]                <- sum(fxbf*Pf[10:54, i] * ((1 + .5*migf[10:54, i]) + .5*migf[10:54, i]))
    Pf[1,i+1]             <- Bxf[i] * (sxf[1, i]*(1 + .5*migf[1, i]) + .5*migf[1, i])
  }
  
  # Males
  for(i in 1:n){
    Pm[2:(z-1),i+1]       <- Pm[1:(z-2),i] * (sxm[1:(z-2), i] * (1 + .5*migm[2:(z-1), i]) + .5*migm[2:(z-1), i])
    Pm[z,i+1]             <- Pm[z-1,i] * (sxm[z-1, i] * (1 + .5*migm[z, i]) + .5*migm[z, i]) +
      Pm[z,i] * (sxm[z, i] * (1 + .5*migm[z, i]) + .5*migm[z, i])
    fxbm                  <- (srb*(1 + srb)^(-1)*(fx[10:54, i] + fx[11:55, i]*sxf[11:55, i])*0.5)
    Bxm[i]                <- sum(fxbm*Pf[10:54, i] * ((1 + .5*migf[10:54, i]) + .5*migf[10:54, i]))
    Pm[1,i+1]             <- Bxm[i] * (sxm[1, i]*(1 + .5*migm[1, i]) + .5*migm[1, i])
  }
  
  list(population = rbind(Pf, Pm)[,1:n], births = Bxf + Bxm)
}

####################################################################################################  
# Other functions in CCPM app for aggregating by sex and generating life table estimates
####################################################################################################  

get.person <- function(x){
  x[1:96] + x[-c(1:96)]
}

lt.est <- function(y){
  px  = exp(-y)
  lx  = c(1,cumprod(px))
  ex  = round(sum(head(lx,-1) + tail(lx,-1)) / 2, 3)
  q5  = 1000*(1 - lx[6])
  q1  = 1000*(1 - lx[2])
  c(ex, q1, q5)
}

######################################################################################################
# Apply the CCPM to data to project a population 
######################################################################################################

project_pop <- function(is, y0, y1){
  
  ####################################################################################################  
  # All WPP input parameters 
  wpp.ina     <- wpp.input  %>% filter(location_name == is & year_id %in% y0:(y1+1)) %>% select(-c(location_name)) %>% 
    arrange(sex_name, age, year_id)
  fx          <- wpp.ina %>% select(sex_name, age, year_id, fx) %>% spread(year_id, fx) %>% 
    select(-c(sex_name, age)) %>% as.matrix()
  Nx          <- wpp.ina %>% select(sex_name, age, year_id, Nx) %>% spread(year_id, Nx) %>% 
    select(-c(sex_name, age)) %>% as.matrix()
  Nx[Nx==0]   <- 1e-09
  mig          <- wpp.ina %>% select(sex_name, age, year_id, mig) %>% spread(year_id, mig) %>% 
    select(-c(sex_name, age)) %>% as.matrix()
  mx          <- wpp.ina %>% select(sex_name, age, year_id, mx) %>% spread(year_id, mx) %>% 
    select(-c(sex_name, age)) %>% as.matrix()
  mx[mx==0]   <- 1e-09
  sx          <- exp(-mx)
  
  n           <- y1- y0+ 1
  z           <- length(0:95)
  #################################################################################################### 
  
  ####################################################################################################  
  # Run model
  
  ccpm_res        <- get_ccpm(Nx, sx, fx, mig, z, n)
  population      <- ccpm_res[["population"]]; births   <- ccpm_res[["births"]]
  deaths          <- -(log(sx)[,1:n]) * population 
  
  pop_tot         <- apply(population, 2, get.person)
  deaths_tot      <- apply(deaths, 2, get.person)
  mx_both         <- deaths_tot/pop_tot
  
  lt.out_both     <- apply(mx_both, 2, lt.est)
  lt.out_fmle     <- apply(mx[1:96,1:n], 2, lt.est)
  lt.out_mle      <- apply(mx[97:192,1:n], 2, lt.est)
  
  deaths_both     <- apply(deaths, 2, sum)
  deaths_male     <- apply(deaths[97:192,], 2, sum)
  deaths_female   <- apply(deaths[1:96,], 2, sum)
  
  e0              <- lt.out_both[1,]
  e0_male         <- lt.out_mle[1,]
  e0_female       <- lt.out_fmle[1,]
  
  imr             <- lt.out_both[2,]
  u5mr            <- lt.out_both[3,]
  
  locs            <- loc %>% filter(location_name == is) %>% select(iso3, location_name)
  
  out.df          <- data.table(group = "CCPM", year = y0:y1,
                                deaths_both = deaths_both, deaths_male = deaths_male, deaths_female = deaths_female,
                                e0 = e0, e0_male = e0_male, e0_female = e0_female, 
                                imr = imr, u5mr = u5mr, births = births, locs) %>%
    rbind(obs_wpp %>% filter(location_name == is & year %in% y0:y1) %>% mutate(group = "WPP2019"))
  
  list(population = population, deaths = deaths, out.df = out.df)
}

#####################################################################################################
# Function to loop through all countries to get estimated deaths
#####################################################################################################

get_all_deaths <- function(y0, y1){
  # loop through pulling migration
  
  isc <- locsall$location_name
  isco<- locsall$iso3
  isn <- length(isc)
  death.list <- list(isn)
  
  for (c in 1:isn){
    is   <- isc[c]
    iso  <- isco[c]
    
    print(paste(c, "of",isn,iso))
    
    out  <- project_pop(is, y0, y1)$deaths %>% as.data.table()
    n    <- y1- y0+ 1
    
    yrv  <- paste0(y0 + 1:n)
    sxv  <- rep(c("Female","Male"), each = 96)
    agv  <- c(0:95, 0:95)
    
    colnames(out) <- yrv
    
    out <- out %>%
      mutate(age = agv, sex_name = sxv) %>%
      gather(year_id, deaths, -age, -sex_name) %>% mutate(year_id = as.numeric(year_id), location_name = is, iso3 = iso) %>%
      arrange(sex_name, age, year_id)
    
    death.list[[c]] <- out
  }
  
  rbindlist(death.list)
}


################################################################################################################################
## Get all raw deaths
## raw.deaths <- get_all_deaths(1990, 2020)
################################################################################################################################