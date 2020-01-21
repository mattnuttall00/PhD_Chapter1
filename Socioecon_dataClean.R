#### This is the data aggregating and data cleaning code for the socioeconomic analysis of chapter 2 (data chapter 1) in my PhD.  The data are from the Commune Database of Cambodia for the years 2007-2012.

#### Load libraries ####

library('tidyverse')
library('cowplot')
library('raster')
library('rgdal')
library('rgeos')
library('RStoolbox')
library('rasterVis')
library('sf')
library('ggmap')
library('mapview')
library('tmap')

#### Load socioeconomic variable and commune data ####

socioecon.dat <- read.csv("Data/commune/socioecon_vars_07-12.csv", header=T) 

str(socioecon.dat)

socioecon.dat$Year <- as.factor(socioecon.dat$Year)
socioecon.dat$VillGis <- as.factor(socioecon.dat$VillGis)

commDat <- read.csv("Data/commune/commGIS.csv")
commDat$province <- as.character(commDat$province)
commDat$commune <- as.character(commDat$commune)
str(commDat)

#### Assign commune code ####

# what is the difference in number of communes between years?
length(unique(socioecon.dat[socioecon.dat$Year==2007, ]$Commune))
length(unique(socioecon.dat[socioecon.dat$Year==2008, ]$Commune))
length(unique(socioecon.dat[socioecon.dat$Year==2009, ]$Commune))
length(unique(socioecon.dat[socioecon.dat$Year==2010, ]$Commune))
length(unique(socioecon.dat[socioecon.dat$Year==2011, ]$Commune))
length(unique(socioecon.dat[socioecon.dat$Year==2012, ]$Commune))

# merge data with commDat
socioecon.dat <- merge(socioecon.dat, commDat, by = c("Province", "Commune"))
str(socioecon.dat)
socioecon.dat <- socioecon.dat[ ,-39]


#### Aggregate to commune level ####

socioecon.dat <- as_tibble(socioecon.dat)

# NEED TO ACCOUNT FOR YEARS

# variables that need to be summed
dat2 <- socioecon.dat %>% 
   
  dplyr::select(commGIS,tot_pop,family,male_18_60,fem_18_60,pop_over61,tot_ind,numPrimLivFarm,land_confl,Pax_migt_in,Pax_migt_out) %>%  
  group_by(commGIS) %>%
  summarise_all(funs(sum)) 

# variables that need to be meaned
dat3 <- socioecon.dat %>% 
  dplyr::select(commGIS,prop_ind,F6_24_sch,M6_24_sch,F15_45_ill,M15_45_ill,propPrimLivFarm,propPrimSec,propSecSec,propTerSec,propQuatSec,
         Les1_R_Land,Les1_F_Land,buff_fam,pig_fam,garbage,crim_case,inf_mort,U5_mort) %>% 
  group_by(commGIS) %>%
  summarise_all(funs(mean)) 

# variables where median needed
dat4 <- socioecon.dat %>% 
  dplyr::select(commGIS, dist_sch,KM_Comm, KM_Heal_cent) %>% 
  group_by(commGIS) %>% 
  summarise_all(funs(median))

# Join all of the above                
dat5 <- left_join(dat2,dat3,by = "commGIS")
dat6 <- left_join(dat5, dat4, by = "commGIS")

# Aggregate the admin variables up to the Commune level
admindat <- socioecon.dat %>% 
  dplyr::select(commGIS,Province, Commune) %>% 
  group_by(commGIS) %>% 
  distinct(commGIS, .keep_all=TRUE)

# Join tables
dat_master <- left_join(admindat, dat7, by = "CommCode")
str(dat_master)