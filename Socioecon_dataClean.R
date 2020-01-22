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
# before aggregating - split up by year.  Run aggregation code for each year df, then rbind together. Then have another look at the admin aggregation as that is not correct now 

# split by year
dat.07 <- socioecon.dat[socioecon.dat$Year=="2007", ]
dat.08 <- socioecon.dat[socioecon.dat$Year=="2008", ]
dat.09 <- socioecon.dat[socioecon.dat$Year=="2009", ]
dat.10 <- socioecon.dat[socioecon.dat$Year=="2010", ]
dat.11 <- socioecon.dat[socioecon.dat$Year=="2011", ]
dat.12 <- socioecon.dat[socioecon.dat$Year=="2012", ]

# function to aggregate
aggFun <- function(dat){
  
  # sum
  datSumF <- dat %>% 
    dplyr::select(commGIS,tot_pop,family,male_18_60,fem_18_60,pop_over61,tot_ind,numPrimLivFarm,land_confl,
                  Pax_migt_in,Pax_migt_out) %>%  
  group_by(commGIS) %>%
  summarise_all(funs(sum))
  
  # mean
  datMeanF <- dat %>% 
    dplyr::select(commGIS,prop_ind,F6_24_sch,M6_24_sch,F15_45_ill,M15_45_ill,propPrimLivFarm,propPrimSec,
                  propSecSec,propTerSec,propQuatSec,Les1_R_Land,Les1_F_Land,buff_fam,pig_fam,garbage,
                  crim_case,inf_mort,U5_mort) %>% 
  group_by(commGIS) %>%
  summarise_all(funs(mean))
    
  # Median
  datMedF <- dat %>% 
    dplyr::select(commGIS, dist_sch,KM_Comm, KM_Heal_cent) %>% 
  group_by(commGIS) %>% 
  summarise_all(funs(median))
  
  left_join(datSumF,datMeanF,datMedF, by="commGIS")
  
}

dat.07.agg <- aggFun(dat.07)

# Check the number of resulting rows in dat.07.agg is correct (i.e. it has the correct number of unique combinations of Province and Commune). This is necessary because when you check the number of unique commune names in dat.07 you are not accounting for communes in different provinces that have the same commune name. 
dat.07$ComProv <- paste(dat.07$Province, dat.07$Commune, sep="_") 
head(dat.07$ComProv)
length(unique(dat.07$ComProv))
# Function appears to be working correctly

# run function for the other years
dat.08.agg <- aggFun(dat.08)
dat.09.agg <- aggFun(dat.09)
dat.10.agg <- aggFun(dat.10)
dat.11.agg <- aggFun(dat.11)
dat.12.agg <- aggFun(dat.12)


# Aggregate the admin variables up to the Commune level
admindat <- socioecon.dat %>% 
  dplyr::select(commGIS,Province, Commune) %>% 
  group_by(commGIS) %>% 
  distinct(commGIS, .keep_all=TRUE)

# Join tables
dat_master <- left_join(admindat, dat7, by = "CommCode")
str(dat_master)