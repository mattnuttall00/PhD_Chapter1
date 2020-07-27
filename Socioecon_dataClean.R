#### This is the data aggregating and data cleaning code for the socioeconomic analysis of chapter 2 (data chapter 1) in my PhD.  The data are from the Commune Database of Cambodia for the years 2007-2012. The land cover data are from the European Space Agency Climate Change Initiative satellite.

##  To skip data aggregation & cleaning process load 'dat_merge' file from "LOAD LATEST VERSION" line 1610

## If errors are detected in raw data, they must be fixed in socioecon_vars_07-12.csv and then all data aggregation and cleaning must be run again and the corrected version saved as dat_merge. 

#### Load libraries ####

library('tidyverse')
library('plyr')
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
library("FactoMineR")
library("factoextra")
library("corrplot")
library("patchwork")

# make sure select() is dplyr, not MASS otherwise loads of code won't work
select <- dplyr::select

#### Socioeconomic data -----------------------------------------------------------------
### Load socioeconomic variable and commune data ####

socioecon.dat <- read.csv("Data/commune/socioecon_vars_07-12.csv", header=T) 

str(socioecon.dat)

socioecon.dat$Year <- as.factor(socioecon.dat$Year)
socioecon.dat$VillGis <- as.factor(socioecon.dat$VillGis)

commDat <- read.csv("Data/commune/commGIS.csv")
commDat$Province <- as.character(commDat$Province)
commDat$Commune <- as.character(commDat$Commune)
str(commDat)

### Assign commune code ####

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


### Aggregate to commune level ####

socioecon.dat <- as_tibble(socioecon.dat)

# NEED TO ACCOUNT FOR YEARS
# before aggregating - split up by year.  Run aggregation code for each year df, then rbind together.  

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
    dplyr::select(commGIS,tot_pop,family,male_18_60,fem_18_60,pop_over61,tot_ind,numPrimLivFarm,
                  land_confl,Pax_migt_in,Pax_migt_out) %>%  
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
  
 
  
  left_join(datSumF,datMeanF, by="commGIS") %>% left_join(., datMedF, by="commGIS")
  
}

dat.07.agg <- aggFun(dat.07)

# Check the number of resulting rows in dat.07.agg is correct (i.e. it has the correct number of unique combinations of Province and Commune). This is necessary because when you check the number of unique commune names in dat.07 you are not accounting for communes in different provinces that have the same commune name. 
dat.07$ComProv <- paste(dat.07$Province, dat.07$Commune, sep="_") 
head(dat.07$ComProv)
length(unique(dat.07$ComProv))
# Function appears to be working correctly i.e. there are 1590 rows in dat.07$ComProv and 1590 rows in dat.07.agg

# run function for the other years
dat.08.agg <- aggFun(dat.08)
dat.09.agg <- aggFun(dat.09)
dat.10.agg <- aggFun(dat.10)
dat.11.agg <- aggFun(dat.11)
dat.12.agg <- aggFun(dat.12)


 # Aggregate the admin variables up to the Commune level & add year
admindat <- socioecon.dat %>% 
  dplyr::select(commGIS,Province, Commune) %>% 
  group_by(commGIS) %>% 
  distinct(commGIS, .keep_all=TRUE)

dat.07.agg <- left_join(dat.07.agg,admindat, by="commGIS")
dat.07.agg$year <- "2007"
dat.08.agg <- left_join(dat.08.agg,admindat, by="commGIS")
dat.08.agg$year <- "2008"
dat.09.agg <- left_join(dat.09.agg,admindat, by="commGIS")
dat.09.agg$year <- "2009"
dat.10.agg <- left_join(dat.10.agg,admindat, by="commGIS")
dat.10.agg$year <- "2010"
dat.11.agg <- left_join(dat.11.agg,admindat, by="commGIS")
dat.11.agg$year <- "2011"
dat.12.agg <- left_join(dat.12.agg,admindat, by="commGIS")
dat.12.agg$year <- "2012"


#### Land cover data -------------------------------------------------------------------
### Load in data ####
  ## CCI raster ####

# CCI layer (unclipped)
CCI_full <- brick("Spatial_data/CCI_Ind1960.tif")
proj4string(CCI_full) <- CRS("+init=epsg:3148")
plot(CCI_full$CCI_Ind1960.1)

# clip to com.shp using mask
CCI_KH <- mask(CCI_full, com.shp)
plot(CCI_KH$CCI_Ind1960.1)

# CCI raster layer (KH clip) - done in QGIS
CCI <- brick("Spatial_data/ESACCI_clip_KH/CCI_clip.tif")

# set CRS to UTM zone 48, Indian 1960
proj4string(CCI) <- CRS("+init=epsg:3148")

# explore raster
str(CCI)
CCI
hist(CCI)
plot(CCI$CCI_clip.1)
CCI@data@values

  ## Commune shapefile ####

## as SF object

# read in as a sf object
com.shp <- st_read('Spatial_data/boundary_khum.shp')

# explore shapefile
plot(com.shp, bg="transparent", add=T)
length(com.shp$CODEKHUM)
nrow(com.shp)
summary(com.shp)
head(com.shp@data)

# plot using ggplot
ggplot(com.shp)+
  geom_sf()

# set CRS to UTM zone 48, Indian 1960
proj4string(com.shp) <- CRS("+init=epsg:3148")

# are there any polygons with area=0
length(com.shp$AREA[com.shp$AREA==0])
length(com.shp$AREA[com.shp$AREA<5000])

# pull out unique commGIS from data (because they are repeated for each year)
comCode <- unique(dat_merge$commGIS)

# subset
com.shp <- subset(com.shp, CODEKHUM %in% comCode)
str(com.shp)
plot(com.shp)



# test plotting based on data from dat_merge
comInd <- dat_merge$commGIS[dat_merge$prop_ind > 0.8]
comInd <- unique(comInd)

# plots only the selected communes
ggplot(com.shp[com.shp$CODEKHUM %in% comInd,])+
  geom_sf()

# this doesn't work
ggplot(com.shp)+
  geom_sf(aes(fill = CODEKHUM[CODEKHUM %in% comInd]))

# add prop_ind data to the shapefile to facilitate plotting
prop_ind07 <- dat_merge %>% filter(year=="2007") %>% select(commGIS,prop_ind)
prop_ind08 <- dat_merge %>% filter(year=="2008") %>% select(commGIS,prop_ind)
prop_ind09 <- dat_merge %>% filter(year=="2009") %>% select(commGIS,prop_ind)
prop_ind10 <- dat_merge %>% filter(year=="2010") %>% select(commGIS,prop_ind)
prop_ind11 <- dat_merge %>% filter(year=="2011") %>% select(commGIS,prop_ind)
prop_ind12 <- dat_merge %>% filter(year=="2012") %>% select(commGIS,prop_ind)

## subset shapefile for each year (because each year of dat_merge has different number of rows to the shapefile)

# extract annual commGIS values
dat07 <- dat_merge$commGIS[dat_merge$year=="2007"]
dat08 <- dat_merge$commGIS[dat_merge$year=="2008"]
dat09 <- dat_merge$commGIS[dat_merge$year=="2009"]
dat10 <- dat_merge$commGIS[dat_merge$year=="2010"]
dat11 <- dat_merge$commGIS[dat_merge$year=="2011"]
dat12 <- dat_merge$commGIS[dat_merge$year=="2012"]

# subset shapefile
com.shp.07 <- subset(com.shp, CODEKHUM %in% dat07)
com.shp.08 <- subset(com.shp, CODEKHUM %in% dat08)
com.shp.09 <- subset(com.shp, CODEKHUM %in% dat09)
com.shp.10 <- subset(com.shp, CODEKHUM %in% dat10)
com.shp.11 <- subset(com.shp, CODEKHUM %in% dat11)
com.shp.12 <- subset(com.shp, CODEKHUM %in% dat12)


## load using GDAL
com.shp <- readOGR(dsn = 'Spatial_data/boundary_khum.shp')

  ## Calculate forested pixels in each commune (not working) ####

## count the number of forested pixels in each commune polygon. The code below kind of works, but for some reason the number of communes in the shapefile (1687) is not matching the number of unique IDs (which should represent communes) in the resulting ext3 (1681). Therefore I am getting the forest pixel data from QGIS instead.

# extract raster values for each commune polygon for each year. output as data frame
ext2 <- extract(CCI_KH, com.shp, df=T)
str(ext2)
head(ext2)
length(ext2$ID[ext2$ID==1])

# remove unwanted years (check column selection)
ext3 <- ext2[ ,c(1,16:21)]
head(ext3)
length(unique(ext3$ID))
write.csv(ext3, file = "ext3.csv")


  ## Forest cover data ####

forest_dat <- read.csv("Data/commune/forest_cover_07-13.csv", header=TRUE)
forest_dat$khum_name <- as.character(forest_dat$khum_name)
colnames(forest_dat)[2] <- "commGIS"
str(forest_dat)
head(forest_dat)

## Correcting duplicate communes in forest_dat ####

# In the commune shapefile, some communes have multiple ploygons because of things like island groups.  This causes issues down the line as you end up with duplicate rows with the same commune code.  Therefore I need to identify these communes, and aggregate them into one row each

# first split them into years
forest07 <- forest_dat[forest_dat$year=="2007", ]
forest08 <- forest_dat[forest_dat$year=="2008", ]
forest09 <- forest_dat[forest_dat$year=="2009", ]
forest10 <- forest_dat[forest_dat$year=="2010", ]
forest11 <- forest_dat[forest_dat$year=="2011", ]
forest12 <- forest_dat[forest_dat$year=="2012", ]

# group rows by commGIS and sum the area and diffPix. If the commune only has one row then the results should remain the same

ForAggFun <- function(dat,Year) {
  
  dat %>% group_by(commGIS) %>% summarise_at(c("area","diffPix"), sum) %>% 
  mutate(year = Year) 
}

forest07.agg <- ForAggFun(forest07, 2007)
forest08.agg <- ForAggFun(forest08, 2008)
forest09.agg <- ForAggFun(forest09, 2009)
forest10.agg <- ForAggFun(forest10, 2010)
forest11.agg <- ForAggFun(forest11, 2011)
forest12.agg <- ForAggFun(forest12, 2012)

### Matching socioeconoimc and forest data sets ####


## I need to match the socioeconomic data to the forest cover data so that I only have communes that have a code and name matching in each dataset, and that have non-zero forest cover. After meeting with Nils, I think I need to make sure the final dataset:
# 1) all communes need to exist in the forest_dat data i.e. they need to have a commune code and diffPix value (i.e. a response variable value and exist in the GIS data)
# 2) They can have missing communes in certain years in the socioecon data. Therefore I don't need to subset all of the years to fit the minimum set.    

# I am expecting there to be more communes in the forest data than in any of the socioeconomic data as the forest data is probably the most up to date and "complete" set of communes. The data from the commune database is older, and had a lot of missing data.
str(dat_master)
str(forest_dat)

## How many communes are missing from the different years?

# 2007
forest07 <- forest_dat[forest_dat$year=="2007", ]
missing.07 <- anti_join(forest07,dat.07.agg, by="commGIS")
str(missing.07) 
# 106 communes missing from socioeconomic data in 2007 compared with the forest data

# 2008
forest08 <- forest_dat[forest_dat$year=="2008",]
missing.08 <- anti_join(forest08, dat.08.agg, by="commGIS") 
str(missing.08)
# 98 communes missing

# 2009
forest09 <- forest_dat[forest_dat$year=="2009",]
missing.09 <- anti_join(forest09, dat.09.agg, by="commGIS") 
str(missing.09)
# 98 communes missing

# 2010
forest10 <- forest_dat[forest_dat$year=="2010",]
missing.10 <- anti_join(forest10, dat.10.agg, by="commGIS") 
str(missing.10)
# 98 communes missing

# 2011
forest11 <- forest_dat[forest_dat$year=="2011",]
missing.11 <- anti_join(forest11, dat.11.agg, by="commGIS") 
str(missing.11)
# 293 communes missing. This was expected.  2011 was missing a lot of data in the commune database

# 2012
forest12 <- forest_dat[forest_dat$year=="2012",]
missing.12 <- anti_join(forest12, dat.12.agg, by="commGIS") 
str(missing.12)
# 293 communes missing here too.

# check to see if the 293 missing communes in 2011 and 2012 are the same
compare11_12 <- anti_join(missing.11, missing.12, by="commGIS")
# They are the same missing communes


## Now I will join each year of socioeconomic data to the corresponding year of forest data

# 2007
join07 <- inner_join(dat.07.agg, forest07.agg, by="commGIS")
join07 <- join07 %>% dplyr::select(-year.y) %>% dplyr::rename(year=year.x)

# 2008
join08 <- inner_join(dat.08.agg, forest08.agg, by="commGIS")
join08 <- join08 %>% dplyr::select(-year.y) %>% dplyr::rename(year=year.x)

# 2009
join09 <- inner_join(dat.09.agg, forest09.agg, by="commGIS")
join09 <- join09 %>% dplyr::select(-year.y) %>% dplyr::rename(year=year.x)

# 2010
join10 <- inner_join(dat.10.agg, forest10.agg, by="commGIS")
join10 <- join10 %>% dplyr::select(-year.y) %>% dplyr::rename(year=year.x)

# 2011
join11 <- inner_join(dat.11.agg, forest11.agg, by="commGIS")
join11 <- join11 %>% dplyr::select(-year.y) %>% dplyr::rename(year=year.x)

# 2012
join12 <- inner_join(dat.12.agg, forest12.agg, by="commGIS")
join12 <- join12 %>% dplyr::select(-year.y) %>% dplyr::rename(year=year.x)

# merge datasets
dat_merge <- rbind(join07,join08,join09,join10,join11,join12)
str(dat_merge)


### Removing false zeros ####

# false zeros are 0's that cannot contribute to your hypothesis. In this context that means communes that have no forest to start with. If they have no forest, then they cannot lose any

# load forest pixels data
for_pix <- read.csv("Data/commune/forested_pixels_07-12.csv")
str(for_pix)
for_pix$year <- as.character(for_pix$year)


# First correct for communes with multiple rows (due to multiple polygons per commune)
# first split them into years
pix07 <- for_pix[for_pix$year=="2007", ]
pix08 <- for_pix[for_pix$year=="2008", ]
pix09 <- for_pix[for_pix$year=="2009", ]
pix10 <- for_pix[for_pix$year=="2010", ]
pix11 <- for_pix[for_pix$year=="2011", ]
pix12 <- for_pix[for_pix$year=="2012", ]

# group rows by commGIS and sum the area and diffPix. If the commune only has one row then the results should remain the same

PixAggFun <- function(dat,Year) {
  
  dat %>% group_by(commGIS) %>% summarise_at(c("ForPix"), sum) %>% 
  mutate(year = Year) 
}

pix07.agg <- PixAggFun(pix07, 2007)
pix08.agg <- PixAggFun(pix08, 2008)
pix09.agg <- PixAggFun(pix09, 2009)
pix10.agg <- PixAggFun(pix10, 2010)
pix11.agg <- PixAggFun(pix11, 2011)
pix12.agg <- PixAggFun(pix12, 2012)

# Initial analyses removed all communes with 0 forest cover. This was because the response variable was going to be CHANGE in forest cover, and therefore communes with zero forest were unable to lose any, and thus were false zeros.  The code below removes these. Skip down to the merging code a few lines down if you do not want to remove false zeros. I am doing this becuase the response I am now using is raw forest pixels, and so communes with zero forest cover are real zeros.

# before merging together, I need to remove false zeros, starting with 2007
pix07.agg <- pix07.agg %>% filter(ForPix >0)

# Now I need to remove those communes from all the other years too
pix08.agg <- semi_join(pix08.agg, pix07.agg, by="commGIS")
pix09.agg <- semi_join(pix09.agg, pix07.agg, by="commGIS")
pix10.agg <- semi_join(pix10.agg, pix07.agg, by="commGIS")
pix11.agg <- semi_join(pix11.agg, pix07.agg, by="commGIS")
pix12.agg <- semi_join(pix12.agg, pix07.agg, by="commGIS")

# now check to see if there are any further false zeros in subsequent years. e.g. there is a commune that had forest in 2010 but lost it all by 2011. That loss will still be accounted for in the 2010 diffPix, but we don't want that zero to carry over into the 2011 diffPix as there wa already no forest in 2011.
pix08.agg %>% filter(ForPix==0) # none
pix09.agg %>% filter(ForPix==0) # none
pix10.agg %>% filter(ForPix==0) # none
pix11.agg %>% filter(ForPix==0) # 1 commune

# remove that 1 commune
pix11.agg <- pix11.agg %>% filter(ForPix >0)

# subset 2012 to match
pix12.agg <- semi_join(pix12.agg, pix11.agg, by = "commGIS")

# check 2012
pix12.agg %>% filter(ForPix==0) # none


# merge together to make a corrected for_pix
for_pix <- rbind(pix07.agg,pix08.agg,pix09.agg,pix10.agg,pix11.agg,pix12.agg)
for_pix$year <- as.character(for_pix$year)
str(for_pix)
length(forest_dat$commGIS)
length(unique(forest_dat$commGIS))


# attach to dat_merge
dat_merge <- inner_join(dat_merge, for_pix, by=c("year", "commGIS"))
str(dat_merge)




#### Additional variables --------------------------------------------------------------
  ## population density ####

# now that I have merged the forest data and socioeconoimc data I have total population and commune area, and so can calculate population density

# convert area from m2 to km2
dat_merge$areaKM <- dat_merge$area / 1000000
dat_merge$pop_den <- dat_merge$tot_pop / dat_merge$areaKM
dat_merge <- dat_merge %>% select(-area)
str(dat_merge)

  ## elevation ####

# load mean and median elevation data
elevation <- read.csv("Data/commune/commune_elevation.csv")
str(elevation)
elevation <- elevation %>% dplyr::rename(commGIS=CODEKHUM)
elevation <- elevation %>% select(-med_elev)

# First correct for communes with multiple rows (due to multiple polygons per commune)
# group rows by commGIS and mean the elevation. 
elevation <- elevation %>% group_by(commGIS) %>% summarise_at("mean_elev", mean)

# attach to dat_merge
dat_merge <- left_join(dat_merge, elevation, by="commGIS")
str(dat_merge)
length(dat_merge$mean_elev[is.na(dat_merge$mean_elev)])

  ## distance to border ####

# this is the distance from the centre of each commune to the nearest border (in KM)

dist_border <- read.csv("Data/commune/dist_border.csv")

# First correct for communes with multiple rows (due to multiple polygons per commune)
# group rows by commGIS and mean the distance. 
dist_border <- dist_border %>% group_by(commGIS) %>% summarise_at("distancekm", mean)
str(dist_border)

# attach to dat_merge
dat_merge <- left_join(dat_merge, dist_border, by="commGIS")
str(dat_merge)
length(dat_merge$distancekm[is.na(dat_merge$distancekm)])
dat_merge <- dat_merge %>% dplyr::rename(dist_border=distancekm)
length(dat_merge$dist_border)

  ## Dominant habitat ####

# load and tidy data
hab07 <- read.csv("Data/commune/habitat_07.csv", header=TRUE)
hab07 <- hab07 %>% mutate(CP = CP + CP.1 + CP.2 + CP.3) %>% select(-CP.1, -CP.2, -CP.3) %>% 
                   mutate(FBD = FBD + FBD.1 + FBD.2) %>% select(-FBD.1, -FBD.2) %>% 
                   mutate(MTSH = MTSH + MTSH.1) %>% select(-MTSH.1) %>% 
                   mutate(SL = SL + SL.1) %>% select(-SL.1) %>% 
                   mutate(NF = NF + NF.1 + NF.2) %>% select(-NF.1, -NF.2) %>% 
                   mutate(B = B + B.1 + B.2) %>% select(-B.1, -B.2)

hab08 <- read.csv("Data/commune/habitat_08.csv", header=TRUE)
hab08 <- hab08 %>% mutate(CP = CP + CP.1 + CP.2 + CP.3) %>% select(-CP.1, -CP.2, -CP.3) %>% 
                   mutate(FBD = FBD + FBD.1 + FBD.2) %>% select(-FBD.1, -FBD.2) %>% 
                   mutate(MTSH = MTSH + MTSH.1) %>% select(-MTSH.1) %>% 
                   mutate(SL = SL + SL.1) %>% select(-SL.1) %>% 
                   mutate(NF = NF + NF.1 + NF.2) %>% select(-NF.1, -NF.2) %>% 
                   mutate(B = B + B.1 + B.2) %>% select(-B.1, -B.2)

hab09 <- read.csv("Data/commune/habitat_09.csv", header=TRUE)
hab09 <- hab09 %>% mutate(CP = CP + CP.1 + CP.2 + CP.3) %>% select(-CP.1, -CP.2, -CP.3) %>% 
                   mutate(FBD = FBD + FBD.1 + FBD.2) %>% select(-FBD.1, -FBD.2) %>% 
                   mutate(MTSH = MTSH + MTSH.1) %>% select(-MTSH.1) %>% 
                   mutate(SL = SL + SL.1) %>% select(-SL.1) %>% 
                   mutate(NF = NF + NF.1 + NF.2) %>% select(-NF.1, -NF.2) %>% 
                   mutate(B = B + B.1 + B.2) %>% select(-B.1, -B.2)

hab10 <- read.csv("Data/commune/habitat_10.csv", header=TRUE)
hab10 <- hab10 %>% mutate(CP = CP + CP.1 + CP.2 + CP.3) %>% select(-CP.1, -CP.2, -CP.3) %>% 
                   mutate(FBD = FBD + FBD.1 + FBD.2) %>% select(-FBD.1, -FBD.2) %>% 
                   mutate(MTSH = MTSH + MTSH.1) %>% select(-MTSH.1) %>% 
                   mutate(SL = SL + SL.1) %>% select(-SL.1) %>% 
                   mutate(NF = NF + NF.1 + NF.2) %>% select(-NF.1, -NF.2) %>% 
                   mutate(B = B + B.1 + B.2) %>% select(-B.1, -B.2)

hab11 <- read.csv("Data/commune/habitat_11.csv", header=TRUE)
hab11 <- hab11 %>% mutate(CP = CP + CP.1 + CP.2 + CP.3) %>% select(-CP.1, -CP.2, -CP.3) %>% 
                   mutate(FBD = FBD + FBD.1 + FBD.2) %>% select(-FBD.1, -FBD.2) %>% 
                   mutate(MTSH = MTSH + MTSH.1) %>% select(-MTSH.1) %>% 
                   mutate(SL = SL + SL.1) %>% select(-SL.1) %>% 
                   mutate(NF = NF + NF.1 + NF.2) %>% select(-NF.1, -NF.2) %>% 
                   mutate(B = B + B.1 + B.2) %>% select(-B.1, -B.2)

hab12 <- read.csv("Data/commune/habitat_12.csv", header=TRUE)
hab12 <- hab12 %>% mutate(CP = CP + CP.1 + CP.2 + CP.3) %>% select(-CP.1, -CP.2, -CP.3) %>% 
                   mutate(FBD = FBD + FBD.1 + FBD.2) %>% select(-FBD.1, -FBD.2) %>% 
                   mutate(MTSH = MTSH + MTSH.1) %>% select(-MTSH.1) %>% 
                   mutate(SL = SL + SL.1) %>% select(-SL.1) %>% 
                   mutate(NF = NF + NF.1 + NF.2) %>% select(-NF.1, -NF.2) %>% 
                   mutate(B = B + B.1 + B.2) %>% select(-B.1, -B.2)

# function to add habitat variable and then remove multiple rows from communes that had multiple polygons, by taking the most frequently occurring habitat category
DomHabFun <- function(dat, Year){
  
  dat$habitat <- colnames(dat[ ,2:18])[max.col(dat[ ,2:18],ties.method="random")]
  
  dat %>% 
    group_by(commGIS, habitat) %>%
              dplyr::mutate(new_col = n()) %>%
              arrange(commGIS, desc(new_col)) %>%
              group_by(commGIS) %>%
              filter(row_number()==1) %>% 
              ungroup() %>% 
              select(-new_col) %>% 
              mutate(year = Year)
  
}

hab07.red <- DomHabFun(hab07, 2007)
hab08.red <- DomHabFun(hab08, 2008)
hab09.red <- DomHabFun(hab09, 2009)
hab10.red <- DomHabFun(hab10, 2010)
hab11.red <- DomHabFun(hab11, 2011)
hab12.red <- DomHabFun(hab12, 2012)

hab_all_yrs <- rbind(hab07.red,hab08.red,hab09.red,hab10.red,hab11.red,hab12.red)
hab_all_yrs$year <- as.character(hab_all_yrs$year)
hab_all_yrs <- hab_all_yrs[ ,c(1,19:20)]

# merge with main data
dat_merge <- left_join(dat_merge, hab_all_yrs, by = c("year", "commGIS"))

  ## Distance to provincial captial (any) ####

# This variable is the distance from the centre of each commune to the nearest provincial capital. I have decided to use any provincial capital as some communes will be closer to a provincial capital of a different province, but the effects of urbanisation, access to services, transport networks, markets, government buildings etc. will be the same regardless of which capital it is. 

# load data
dist_provCap <- read.csv("Data/commune/dist_provCap.csv", header=T)
str(dist_provCap)

# First correct for communes with multiple rows (due to multiple polygons per commune)
# group rows by commGIS and mean the distance. 
dist_provCap <- dist_provCap %>% group_by(commGIS) %>% summarise_at("Distance", mean)
str(dist_provCap)

# attach to dat_merge
dat_merge <- left_join(dat_merge, dist_provCap, by="commGIS")
str(dat_merge)
length(dat_merge$Distance[is.na(dat_merge$Distance)])
dat_merge <- dat_merge %>% dplyr::rename(dist_provCap = Distance)
length(dat_merge$dist_provCap)

  ## ELC presence ####

# This variable is a dummy variable (0,1) that indicated whether a commune had an ELC granted (based on contract dates) in that year, or any previous year (including 2006 for 2007). I have assumed that if a commune had an ELC in a previous year, then that ELC remained (i.e. wasn't cancelled) in all subsequent years.

# load in data
elc_dat <- read.csv("Data/commune/ELC_in_communes_yrs.csv", header=T)
str(elc_dat)

# First correct for communes with multiple rows (due to multiple polygons per commune)
# group rows by commGIS and take the maximum value (so if there is a 1 then the value will be 1). 

# split up the years
elc_07 <- elc_dat %>% filter(year==2007)
elc_08 <- elc_dat %>% filter(year==2008)
elc_09 <- elc_dat %>% filter(year==2008)
elc_10 <- elc_dat %>% filter(year==2010)
elc_11 <- elc_dat %>% filter(year==2011)
elc_12 <- elc_dat %>% filter(year==2012)

# remove duplicate rows
elcFun <- function(dat, Year){
  
  dat %>% group_by(commGIS) %>% summarise_at("elc", max) %>% mutate(year=Year)
  
}

elc_07 <- elcFun(elc_07, 2007)
elc_08 <- elcFun(elc_08, 2008)
elc_09 <- elcFun(elc_09, 2009)
elc_10 <- elcFun(elc_10, 2010)
elc_11 <- elcFun(elc_11, 2011)
elc_12 <- elcFun(elc_12, 2012)

# join back together
elc_dat_clean <- rbind(elc_07, elc_08, elc_09, elc_10, elc_11, elc_12)
str(elc_dat_clean)
elc_dat_clean$year <- as.character(elc_dat_clean$year)

# merge with main data
dat_merge <- left_join(dat_merge, elc_dat_clean, by = c("year", "commGIS"))
length(dat_merge$elc[is.na(dat_merge$elc)])

  ## Protected areas ####

# I will create two variables here.  The first will be a dummy (0,1) variable that will represent whether a commune is partly or entirely inside ANY kind of PA.  The second will be a factor, with the levels representing the different category of PA. There are a few cases where a PA has been created in a year within the study period. Therefore I have had to include all years as there are difference between years

# load data
pa_dat <- read.csv("Data/commune/Communes_in_PAs.csv", header=T)
str(pa_dat)
head(pa_dat)

# merge all columns into one
pa_dat[] <- t(apply(pa_dat, 1, function(x) c(x[!is.na(x)], x[is.na(x)])))

# remove unwanted columns and rename others
pa_dat <- pa_dat %>% select(year, commGIS, PA_cat = MULTI)

# replace NAs with "none"
pa_dat$PA_cat[is.na(pa_dat$PA_cat)] <- "none" 

# PA_cat to factor
pa_dat$PA_cat <- as.factor(pa_dat$PA_cat)

# split into years and remove duplicate rows caused by multiple polygons

PAFun <- function(dat, Year){
  

  dat %>% 
    filter(year==Year) %>% 
    group_by(commGIS, PA_cat) %>%
              dplyr::mutate(new_col = n()) %>%
              arrange(commGIS, desc(new_col)) %>%
              group_by(commGIS) %>%
              filter(row_number()==1) %>% 
              ungroup() %>% 
              select(-new_col) 
              
}

pa07 <- PAFun(pa_dat, 2007)
pa08 <- PAFun(pa_dat, 2008)
pa09 <- PAFun(pa_dat, 2009)
pa10 <- PAFun(pa_dat, 2010)
pa11 <- PAFun(pa_dat, 2011)
pa12 <- PAFun(pa_dat, 2012)

# merge years back together
pa_dat_red <- rbind(pa07, pa08, pa09, pa10, pa11, pa12)
str(pa_dat_red)

# add dummy variable (i.e. 1 = PA, 0 = no PA)
pa_dat_red$PA <- ifelse(pa_dat_red$PA_cat != "none", 1, 0)

# merge with main data
pa_dat_red$commGIS <- as.integer(pa_dat_red$commGIS)
dat_merge <- left_join(dat_merge, pa_dat_red, by = c("year", "commGIS"))
str(dat_merge)
length(dat_merge$PA_cat[is.na(dat_merge$PA_cat)])
length(dat_merge$PA[is.na(dat_merge$PA)])

#### Clean, format, and error check data -----------------------------------------------
  ## Load working version of data ####

# save working version of the data with ALL communes (i.e. communes with zero forest NOT removed)
write.csv(dat_merge, file="Data/commune/dat_merge_allComs.csv")


# load working version of the data 

# if using the data where communes with 0 forest have been removed
dat_merge <- read.csv("Data/commune/dat_merge.csv", header=T)

# if using data where no communes have been removed
dat_merge <- read.csv("Data/commune/dat_merge_allComs.csv", header=T)

# get rid of annoying diagostic warnings
dat_merge <- as.data.frame(dat_merge)

str(dat_merge)
dat_merge <- dat_merge %>% select(-X)

# year to factor
dat_merge$year <- as.factor(dat_merge$year)

  ## re-arrange variables into their sets ####  

# The sets are:

# 1) Reference vars (year,Province, Commune, commGIS, areaKM) #5
# 2) Response vars (ForPix, diffPix) #2
# 3) Population demographics (tot_pop, family, male_18_60, fem_18_60, pop_over61, tot_ind, prop_ind,
                            # pop_den) #8
# 4) Education (F6_24_sch, M6_24_sch, F15_45_ill, M15_45_ill) #4
# 5) Employment (numPrimLivFarm, propPrimLivFarm, propPrimSec, propSecSec, propTerSec, propQuatSec) #6
# 6) Economic security (Les1_R_Land, Les1_F_Land, buff_fam, pig_fam) #4
# 7) Access to services (dist_sch, garbage, KM_Comm, KM_Heal_cent) #4
# 8) Social justice (land_confl, crim_case) #2
# 9) Health (inf_mort, U5_mort) #2 
# 10) Migration (Pax_migt_in, Pax_migt_out) #2
# 11) Environmental additional (mean_elev, habitat) #2
# 12) Human additional (dist_border, dist_provCap, elc, PA, PA_cat) #5

# habitat removed here because I am not using it in my models now (see mix_models_real script)
dat_merge <- dat_merge %>% select(year,Province, Commune, commGIS, areaKM,
                                  ForPix, diffPix,
                          tot_pop, family, male_18_60, fem_18_60, pop_over61, tot_ind, prop_ind, pop_den,
                                  F6_24_sch, M6_24_sch, F15_45_ill, M15_45_ill,
                      propPrimLivFarm, propPrimSec, propSecSec, propTerSec, propQuatSec,
                                  Les1_R_Land, Les1_F_Land, buff_fam, pig_fam,
                                  dist_sch, garbage, KM_Comm, KM_Heal_cent,
                                  land_confl, crim_case,
                                  inf_mort, U5_mort,
                                  Pax_migt_in, Pax_migt_out,
                                  mean_elev,
                                  dist_border, dist_provCap, elc, PA, PA_cat)

  ## Error checking ####

# If I/you have had to change the raw data (e.g. to fix any error(s) in the raw data), then you need to re-run the code above that creates dat_merge. You then need to run all the below chunks that have RUN in the header.  Don't just run the whole chunk though - open the chunk and check which lines need to be run. If the raw data has not changed, just load dat_merge from the latest "LOAD LATEST VERSION" chunk (i.e the lowest down the list)

## the code that creates dat_merge are in the following chunks:

# Load socioeconomic variable and commune data
# Assign commune code
# Aggregate to commune level
# Forest cover data
# Correcting duplicate communes in forest_dat
# Matching socioeconoimc and forest data sets
# Removing false zeros (There are instructions in this section if you are not removing false zeros)
# Additional variables (and sub chunks)

# load dat_merge
dat_merge <- read.csv("Data/commune/dat_merge.csv", header = T)
str(dat_merge)
dat_merge <- dat_merge[ ,-1]

    # area ####
hist(dat_merge$areaKM)
dat_merge %>% filter(areaKM >2000)
# some very large communes 

large.area <- dat_merge %>% filter(areaKM >2000) %>% select(commGIS)
area.shp <- subset(com.shp, CODEKHUM %in% large.area)

# Add areaKM to shapefile attribute table
com.shp$areaKM <- com.shp$AREA / 1000000

ggplot(com.shp)+
  geom_sf(aes(fill=AREA > 2000))



    # ForPix ####


length(dat_merge$ForPix[is.na(dat_merge$ForPix)])
hist(dat_merge$ForPix)
dat_merge %>% filter(ForPix < 10) %>% select(year, Province, Commune, ForPix)
dat_merge %>% filter(ForPix == 0) %>% select(year, Province, Commune, ForPix)
# Lots of communes with very few or zero forest pixels.


    # diffPix ####

length(dat_merge$diffPix[is.na(dat_merge$diffPix)])
hist(dat_merge$diffPix)
dat_merge %>% filter(diffPix < -800) %>% select(year, Province, Commune, commGIS, diffPix)
# one commune with large losses

# check
dat_merge %>% filter(commGIS==60705) %>% select(year, Province, Commune, commGIS, diffPix, ForPix)
# Checked forest cover layer in QGIS and this looks correct

# check other large losses
dat_merge %>% filter(diffPix < -400) %>% select(year, Province, Commune, commGIS, diffPix)
# These are correct

# what proportion of observations are 0? (total obs = 4244)
length(dat_merge$diffPix[dat_merge$diffPix==0])
3631/4244*100
# 85.5% of observations are zero. 

# which communes have a positive change in forest cover
dat_merge %>% filter(diffPix >0) %>% select(year, Province, Commune, commGIS, diffPix)
# most are only a few pixels. The largest gains are in Mondulkiri, and are pre most ELCs. Becuase the Bunong practise rotational agriculture this is not unbelievable 


    # tot_pop (RUN) ####

hist(dat_merge$tot_pop)

dat_merge %>% filter(tot_pop < 500)
# one commune with a population less than 500 - Chumnoab.  It has an area of 572km2. Is that a small commune?
dat_merge %>% filter(areaKM <600)
# no - most communes are smaller than that
summary(dat_merge$areaKM)
# I have checked in GIS.  I don't have any reason not to believe the data

summary(dat_merge$tot_pop)

# where are the communes with the largest populations
dat_merge %>% filter(tot_pop > 20000) %>% select(year, commGIS, Province, Commune, tot_pop)
# Ii can believe these. Kampong Cham, Kandal are both around Phnom Penh, Battambang is the third largest city, Siem Reap is the secon largest, and Otdar Meanchey is on the Thai border

# check histo per year
ggplot(dat_merge, aes(dat_merge$tot_pop))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# slight dip in 2011 and 2012

# plot all populations
ggplot(dat_merge, aes(x=year, y=tot_pop, group=Commune,colour=Commune))+
  geom_line()+
  theme(legend.position="none")

# Plot populations by province
ggplot(dat_merge, aes(x=year, y=tot_pop, group=Commune,colour=Commune))+
  geom_line()+
  facet_wrap(vars(Province), nrow = 2, ncol=12)+
  theme(legend.position="none")
# Most obvious strange one is in Battambang

# find the dodgy one in Battambang
dat_merge %>% filter(Province=="Battambang" & tot_pop > 30000) %>% select(Province,Commune,tot_pop)
# Boeng Pring

dat_merge %>% filter(Commune=="Boeng Pring") %>% select
# Clearly an error here. I will change the 2008 and 2010 values to the mean of the two years either side

# check there are no other tot_pop values that are the same
dat_merge %>% filter(tot_pop == 33525) %>% select(Province, Commune, tot_pop)
# no

# replace values
dat_merge <- dat_merge %>% mutate(tot_pop = replace(tot_pop, which(tot_pop==33525 & year==2008), 9390))
dat_merge <- dat_merge %>% mutate(tot_pop = replace(tot_pop, which(tot_pop==33525 & year==2010), 9614))


# find the massive increase in Kampong Cham
dat_merge %>% filter(Province=="Kampong Cham" & tot_pop >25000) %>% select(year,Commune,tot_pop)
dat_merge %>% filter(Province=="Kampong Cham" & Commune=="Kaoh Roka") %>% select(year,Commune,tot_pop)
# going from 8400 to 30,000 in one year is ridiculous.  There was a steady increase in population over th years, with a bigger incrase between 2010 and 2011.  It looks suspiciously like a typo - if the "2" wasn't there then tot_pop would be 9724 which would fit. However I don't know that so I will replace the value with the mean

dat_merge <- dat_merge %>% mutate(tot_pop = replace(tot_pop, tot_pop==29784, 7489))

# check the huge drop in tot_pop in Battambang
ggplot(dat_merge[dat_merge$Province=="Battambang",], 
       aes(x=year, y=tot_pop, group=Commune,colour=Commune))+
       geom_line()

dat_merge %>% filter(Province=="Battambang") %>% select(year,Commune,tot_pop)
dat_merge %>% filter(Province=="Battambang" & Commune=="Sdau") %>% 
              select(year,Commune,tot_pop)
# it is a large drop in population, but the numbers are not identical or repeated (often a giveaway that it is an error), and I can't see any obvious suggestion that it is not correct.  I will leave it as it is


# Find the odd "peak" in Sihanouk
dat_merge %>% filter(Province=="Preah Sihanouk" & Commune=="Sangkat lek Bei") %>% select(year,tot_pop)
# Sangkat lek Bei only exists in 2008, 2009, 2010. I can't find any evidence of the commune on the internet, so I think this is a historic one that no longer exists. Becasue of this, plus the odd shape (varying populatin of > 5000 between years) I will remove it.

dat_merge <- dat_merge %>% filter(Commune != "Sangkat lek Bei")

# find massive spike in Siem Reap
dat_merge %>% filter(Province=="Siem Reap" & tot_pop > 35000) %>% select(year, Commune,tot_pop)
dat_merge %>% filter(Province=="Siem Reap" & Commune=="Kouk Chak") %>% select(year,tot_pop)
# right so what I think has happened is that the Commune Svay Dangkum was merged with Kouk Chak in 2010, because the former had a very high population previously, but then disappears in 2010, and the Kouk Chak suddenly shoots up the same population size as the one that has disappeared.  

# changing Trapeang Prasat as 2008 and 2010 have identical values (see Pax_migt_in section below for reasons). I will change them to the mean of the two years either side
dat_merge <- dat_merge %>% mutate(tot_pop = replace(tot_pop, 
                                                    which(tot_pop==16356 & year==2008),
                                                    14167))

dat_merge <- dat_merge %>% mutate(tot_pop = replace(tot_pop, 
                                                    which(tot_pop==16356 & year==2010),
                                                    18506))




    # family ####

hist(dat_merge$family)
# same shape as tot_pop which is reassuring

# check there are no stupidly small values
dat_merge %>% filter(family < 100) %>% select(commGIS,Province, Commune,family,tot_pop)
# same commune that had the smallest popualtion

# check that total population and number of families are closely related
plot(dat_merge$family, dat_merge$tot_pop)
# yes

id <- identify(dat_merge$family, dat_merge$tot_pop) # outlier points are 590,763,1325,2063,3663

dat_merge[c(590,763,1325,2063,3663), c(1:4,8:9)]
# interesting.  these outliers are Siem Reap (x3), Battambang and Kampong Cham. Siem Reap and Battambang are large cities with industry and tourism. My guess is that lots of people are temporary economic in-migrants who don't necessarily have/take family with them. Their families likely stay out in the provinces.

# check histo per year
ggplot(dat_merge, aes(dat_merge$family))+
  geom_histogram()+
  facet_grid(cols = vars(year))

    # male_18_60 ####

hist(dat_merge$male_18_60)

# make sure there are no stupidly low numbers
dat_merge %>% filter(male_18_60 <500)
# 372 

# of those above, what are the proprtions of males to the total population
dat_merge %>% filter(male_18_60 <500) %>% select(Province,Commune,tot_pop,male_18_60) %>% 
  mutate(prop_males = male_18_60/tot_pop*100) 
# vast majority are between 20-30% of the population are males. The provinces tend to be rural provinces. I would guess that a lot of the working age males head to the cities for work.

# make sure there are no weird values where there are more males than the total population
dat_merge %>% filter(male_18_60 > tot_pop)
# no

# check histo for years
ggplot(dat_merge, aes(dat_merge$male_18_60))+
  geom_histogram()+
  facet_grid(cols = vars(year))


    # fem_18_60 ####

# I identified some errors in the raw data for this variable in 2008. I have gone back and corrected them.

hist(dat_merge$fem_18_60)

# I think all errors in the raw data now fixed 

# check histo for years
ggplot(dat_merge, aes(dat_merge$fem_18_60))+
  geom_histogram()+
  facet_grid(cols = vars(year))

    # pop_over61 ####

hist(dat_merge$pop_over61)

dat_merge %>% filter(pop_over61 > 3000) %>% select(year,Province,Commune,pop_over61,tot_pop)
# well below tot_pop so no reason to doubt it

# check histo for years
ggplot(dat_merge, aes(dat_merge$pop_over61))+
  geom_histogram()+
  facet_grid(cols = vars(year))

    # tot_ind & prop_ind (RUN) ####

### adding prop_ind to the annual shapefiles for plotting
com.shp.07 <- com.shp.07 %>% dplyr::rename(commGIS = CODEKHUM)
com.shp.07 <- left_join(com.shp.07,prop_ind07, by="commGIS")

com.shp.08 <- com.shp.08 %>% dplyr::rename(commGIS = CODEKHUM)
com.shp.08 <- left_join(com.shp.08,prop_ind08, by="commGIS")

com.shp.09 <- com.shp.09 %>% dplyr::rename(commGIS = CODEKHUM)
com.shp.09 <- left_join(com.shp.09,prop_ind09, by="commGIS")

com.shp.10 <- com.shp.10 %>% dplyr::rename(commGIS = CODEKHUM)
com.shp.10 <- left_join(com.shp.10,prop_ind10, by="commGIS")

com.shp.11 <- com.shp.11 %>% dplyr::rename(commGIS = CODEKHUM)
com.shp.11 <- left_join(com.shp.11,prop_ind11, by="commGIS")

com.shp.12 <- com.shp.12 %>% dplyr::rename(commGIS = CODEKHUM)
com.shp.12 <- left_join(com.shp.12,prop_ind12, by="commGIS")

# plot prop_ind > 0.8 for all years
prop_ind_plot_07 <- ggplot(com.shp.07)+
  geom_sf(aes(fill = prop_ind > 0.8))
prop_ind_plot_08 <- ggplot(com.shp.08)+
  geom_sf(aes(fill = prop_ind > 0.8))
prop_ind_plot_09 <- ggplot(com.shp.09)+
  geom_sf(aes(fill = prop_ind > 0.8))
prop_ind_plot_10 <- ggplot(com.shp.10)+
  geom_sf(aes(fill = prop_ind > 0.8))
prop_ind_plot_11 <- ggplot(com.shp.11)+
  geom_sf(aes(fill = prop_ind > 0.8))
prop_ind_plot_12 <- ggplot(com.shp.12)+
  geom_sf(aes(fill = prop_ind > 0.8))

(prop_ind_plot_07 | prop_ind_plot_08) / 
  (prop_ind_plot_09 | prop_ind_plot_10) /
  (prop_ind_plot_11 | prop_ind_plot_12)


## data checking

hist(dat_merge$tot_ind)
dat_merge %>% filter(tot_ind>tot_pop) %>% select(year,Province,Commune,tot_pop,tot_ind)
# There are 30 communes where tot_ind is greater than tot_pop.  I have checked in the raw data and the issues are in the raw data, not in my summaries.  There is not much I can do about that, except align the two values (tot_pop and tot_ind) so that the indigenous values are not greater than the total population. THis will give a prop_ind of 1, but seeing as the tot_ind is so high, this isn't ridiculous

# I will use the larger indigenous total as the "correct" value.

# create test dataset to test the code
test <- dat_merge %>% filter(prop_ind>0.5) %>% select(year,Province, Commune,tot_ind,tot_pop)

test <- test %>% 
          mutate(tot_pop = replace(tot_pop, tot_pop<tot_ind, tot_ind[tot_pop<tot_ind]))
# that works

# do on the main dataset
dat_merge <- dat_merge %>% 
              mutate(tot_pop = replace(tot_pop, tot_pop<tot_ind, tot_ind[tot_pop<tot_ind]))

# check
dat_merge %>% filter(tot_ind>tot_pop) # 0 rows


## now for prop_ind
dat_merge %>% filter(prop_ind>1) %>% select(year,Province,Commune,prop_ind,tot_pop,tot_ind)
# 44 occasions. But becuase of what I have done above, I can correct these

# first create new prop_ind using corrected tot_pop and tot_ind, then replace prop_ind values
dat_merge <- dat_merge %>% 
              mutate(new_prop_ind = tot_ind/tot_pop) %>% 
              mutate(prop_ind = replace(prop_ind, prop_ind>1, new_prop_ind[prop_ind>1])) %>% 
              select(-new_prop_ind)
# check
dat_merge %>% filter(prop_ind>1) # 0 rows

# check histo for years
ggplot(dat_merge, aes(dat_merge$prop_ind))+
  geom_histogram()+
  facet_grid(cols = vars(year))


    # Comparing demographics ####


## NO NEED TO RE-RUN THE BELOW CODE. I CORRECTED THE ERRORS IN THE RAW DATA AND HAVE RE-MADE dat_merge. I HAVE KEPT THE BELOW CODE IN JUST FOR REFERENCE.

# Now I want to check to make sure the totals for the demographic categories aren't greater than tot_pop
demog <- dat_merge %>% select(year,Province,Commune,commGIS,
                              tot_pop,family,male_18_60,fem_18_60,pop_over61,tot_ind)

demog <- demog %>% mutate(totals = male_18_60+fem_18_60+pop_over61)
demog %>% filter(totals>tot_pop) 
# there are 727 cases where the sum of the individial demography variables are greater than the tot_pop

# by how much are they off?
demog.diff <- demog %>% filter(totals>tot_pop) 
demog.diff <- demog.diff %>% mutate(difference = totals-tot_pop)
summary(demog.diff$difference)
# some very large differences

demog.diff %>% filter(difference==15420)
length(demog.diff$year[demog.diff$year==2008])
# Ok, they are all 2008.  Something dodgy has happened to 2008. 

# lets look at the village level data
str(socioecon.dat)
demog.vill <- socioecon.dat %>% filter(Year=="2008") %>% select(Province, Commune,tot_pop,
                                                                male_18_60,fem_18_60,pop_over61)

demog.vill <- demog.vill %>% mutate(totals = male_18_60+fem_18_60+pop_over61) 
demog.vill %>% filter(totals>tot_pop)                
# 13725 villages in the raw data have tot_pop less than the sum of the others. There are only 13939 villages in the the 2008 data, so this is not great.

# I will import the raw data taken straight from the CDB file (rather than my collated data) and see if the issues is with the actualy data or whether I have made a mistake
vill.demog.08 <- read.csv("Data/commune/2008_vill_demog.csv", header=T)
str(vill.demog.08)
vill.demog.08 <- vill.demog.08 %>% 
                  mutate(tot_pop = FEM_TOT+MAL_TOT) %>% 
                  mutate(m18_60 = MAL_18_24+MAL_25_35+MAL_36_45+MAL_46_60) %>% 
                  mutate(f18_60 = FEM_18_24+FEM_25_35+FEM_36_45+FEM_46_60) %>% 
                  mutate(pop_over61 = M_over61+F_over61)

vill.demog.08 <- vill.demog.08 %>% mutate(totals = m18_60+f18_60+pop_over61)
vill.demog.08 %>% filter(totals>tot_pop)
# ok so the raw data is fine.  I have done something wrong when extracting

# I have now re-extracted the demog data from the 2008 CDB.  I will check it
new.demog.2008 <- read.csv("Data/commune/2008_new_demog.csv", header=TRUE)
str(new.demog.2008)

new.demog.2008 <- new.demog.2008 %>% mutate(totals=m_18_60+f_18_60+pop_over61)
new.demog.2008 %>% filter(totals>tot_pop)
# ok so that has worked.  There are no cases when the sum of the individual categories are greater than tot_pop.  I will therefore replace those variables in the master data




    # pop_den ####

hist(dat_merge$pop_den)
# a few larger values

dat_merge %>% filter(pop_den>2000) %>% select(Province,Commune,commGIS,pop_den,areaKM,tot_pop) 
# Phnom Penh. Acceptable

dat_merge %>% filter(pop_den>1500) %>% select(Province,Commune,commGIS,pop_den,areaKM,tot_pop) 
# mostly Phnom Penh, plus Sihanouk and Kampong Cham. Believable 

# check the small values
dat_merge %>% filter(pop_den<1) %>% select(year,Province,Commune,commGIS,pop_den,areaKM,tot_pop)
# Koh Kong and Mondulkiri - fine.

# check histo per year
ggplot(dat_merge, aes(dat_merge$pop_den))+
  geom_histogram()+
  facet_grid(cols = vars(year))


    # F6_24_sch ####

# are there any with propotion > 1
dat_merge %>% filter(F6_24_sch>1) %>% select(year,Province,Commune,commGIS) # none

# any super low?
dat_merge %>% filter(F6_24_sch < 0.1) %>% select(year,Province,Commune,commGIS)
# only a handful each year.  Rattankiri and Stung Treng. Probably the two most remote and rural provinces. Believable

# check histo for years
ggplot(dat_merge, aes(dat_merge$F6_24_sch))+
  geom_histogram()+
  facet_grid(cols = vars(year))

    # M6_24_sch ####

# are there any with propotion > 1
dat_merge %>% filter(M6_24_sch>1) %>% select(year,Province,Commune,commGIS,M6_24_sch) # 223
dat_merge %>% filter(M6_24_sch>1, year==2009) %>% select(year,Province,Commune,commGIS,M6_24_sch)

# there were errors, but I have fixed them in the raw data

# check histo for years
ggplot(dat_merge, aes(dat_merge$M6_24_sch))+
  geom_histogram()+
  facet_grid(cols = vars(year))


    # F15_45_ill ####

hist(dat_merge$F15_45_ill)

# are there any over 1?
dat_merge %>% filter(F15_45_ill > 1) %>% select(Province, Commune)
# no

# highest values
dat_merge %>% filter(F15_45_ill > 0.9) %>% select(Province, Commune)
# there are 11 communes with values higher than 0.9.  The majority are in Rattanikiri, and a couple in Preah Vihear.  This makes sense - they are two of the poorest and most remote provinces where illiteracy is likely to be high

# check histo for years
ggplot(dat_merge, aes(dat_merge$F15_45_ill))+
  geom_histogram()+
  facet_grid(cols = vars(year))


    # M15_45_ill ####

hist(dat_merge$M15_45_ill)
# similar shape to F15_45_ill

# are there any over 1?
dat_merge %>% filter(M15_45_ill > 1) %>% select(Province, Commune)
# no

# highest values
dat_merge %>% filter(M15_45_ill > 0.9) %>% select(Province, Commune)
# 3 communes - again Rattanikiri and Preah Vihear

# check histo for years
ggplot(dat_merge, aes(dat_merge$M15_45_ill))+
  geom_histogram()+
  facet_grid(cols = vars(year))


    # numPrimLivFarm & propPrimLivFarm ####

hist(dat_merge$propPrimLivFarm)
hist(dat_merge$numPrimLivFarm)
# a few large values

dat_merge %>% filter(numPrimLivFarm > 10000) %>% select(Province, Commune, numPrimLivFarm)
# 5 communes with large values.  Kampong Cham, and Pursat I can believe - they are the agricultural provinces.  Otdar Meanchey I'm not sure about

# check the values are not larger than tot_pop
dat_merge %>% filter(numPrimLivFarm > 10000) %>% select(year,Province, Commune, numPrimLivFarm, tot_pop)
# no

# one commune (Kor) is large in 2011 and 2012, but the other one (Kraek) is only large in 2011.  I want to check the values for the other years
dat_merge %>% filter(Province=="Kampong Cham", Commune=="Kraek") %>% select(year, numPrimLivFarm)
# As I suspected, the value for 2011 is wildly different to the other years. And it goes back down in 2012 so I don't believe it. 

# plot it
ggplot(dat_merge, aes(x=year, y=numPrimLivFarm))+
  geom_point(data=subset(dat_merge, Commune=="Kraek"))+
  ylim(0,16000)
# 2011 is clearly an error.  There appears to be an upward trend, so I will correct the value to fall between the 2010 and 2012 values

# replace value
dat_merge$numPrimLivFarm[dat_merge$year==2011 & dat_merge$Commune=="Kraek"] <- 7500

# now check the other large values - Kampong Cham > Kor
dat_merge %>% filter(Province=="Kampong Cham", Commune=="Kor") %>% select(year, numPrimLivFarm)
# it seems odd that the value shoots up from 5513 in 2010 to 12821 in 2011, and then backdown to 12346 in 2012.  But I am not sure whether this is an error or not, so hard to know what to do. I will leave it as it is

# other large values - Pursat > Ta Lou
dat_merge %>% filter(Province=="Pursat", Commune=="Ta Lou") %>% select(year, numPrimLivFarm)
# a very large jump between 2010 and 2011. Maybe I'll check all numPrimLivFarm values between those years to see if this is a national trend

ggplot(dat_merge, aes(x=year, y=numPrimLivFarm))+
  geom_point()
# This appears to be a national trend.  I will just double check the raw data to make sure this is represented in the raw data, and that it's not just an error made by me

# Ok so I think the issue is that 2007-2010 the question actually refers to "number of families" whereas in 2011 and 2012 is refers to individuals.  So the numbers are obviously going to be larger. I think I can deal with this because I have the raw values (numPrimLivFarm) and the reference values ("family" and "male_18_60" & "fem_18_60"). Therefore I can get propPrimLivFarm in 2007-2010 by dividing numPrimLivFarm by family, and in 2011-2012 by dividing numPrimLivFarm by the sum of male_18_60 and fem_18_60. That should give me the correct propPrimLivFarm values (although I will miss anyone over the age of 60 who is still a farmer). I will then need to remove numPrimvLivFarm.

# first let me check to see if propPrimLivFarm has the same pattern
ggplot(dat_merge,aes(x=year, y=propPrimLivFarm))+
  geom_point()
# hmm.  odd

# lets try the fix above

# for 2007 - 2010 I will divide numPrimLivFarm by family
# pull out data
farmdat <- dat_merge[ ,c("year","commGIS","numPrimLivFarm","family","male_18_60","fem_18_60")]

# split years
farmdat_07_10 <- farmdat[farmdat$year=="2007"|farmdat$year=="2008"|farmdat$year=="2009"|farmdat$year=="2010", ]
farmdat_11_12 <- farmdat[farmdat$year=="2011"|farmdat$year=="2012", ]

# create new propPrimLivFarm 2007-2010
farmdat_07_10$propPrimLivFarm <- farmdat_07_10$numPrimLivFarm / farmdat_07_10$family

# sum male_18_60 and fem_18_60 for 2011 and 2012
farmdat_11_12$pop <- farmdat_11_12$male_18_60 + farmdat_11_12$fem_18_60

# create new propPrimLivFarm 2011 and 2012
farmdat_11_12$propPrimLivFarm <- farmdat_11_12$numPrimLivFarm / farmdat_11_12$pop

# remove pop so the number of columns match
farmdat_11_12 <- farmdat_11_12[ ,-7]

# put back together
farmdat.new <- rbind(farmdat_07_10, farmdat_11_12)
str(farmdat.new)

# check what the new variable looks like
ggplot(farmdat.new, aes(x=year, y=propPrimLivFarm))+
  geom_point()
# better than before, but 2011 and 2012 have quite a few values over 1. This will probably be because there are people who identify as farmers who are under 18 or over 60.  The only thing I can do is change any values >1 to 1.  

# change values >1 to 1
farmdat.new <- farmdat.new %>% 
                mutate(propPrimLivFarm = replace(propPrimLivFarm, propPrimLivFarm>1, 1))
str(farmdat.new)

# remove unwanted columns
farmdat.new <- farmdat.new %>% select(year, commGIS, propPrimLivFarm_new = propPrimLivFarm)

# merge with main dataset
dat_merge <- left_join(dat_merge, farmdat.new, by = c("year", "commGIS"))
str(dat_merge)

# replace propPrimLivFarm
dat_merge <- dat_merge %>% select(-propPrimLivFarm) %>% 
              dplyr::rename(propPrimLivFarm = propPrimLivFarm_new)

# remove numPrimLivFarm, as this is no longer a useable variable because of the differences in questions
dat_merge <- dat_merge %>% select(-numPrimLivFarm)

# save file
write.csv(dat_merge, file="Data/commune/dat_merge.csv")

# check histo for years
ggplot(dat_merge, aes(dat_merge$propPrimLivFarm))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# slightly different shape in 2011 and 2012. 

    # propPrimSec (RUN) ####

hist(dat_merge$propPrimSec)
# some values > 1

dat_merge %>% filter(propPrimSec>1) %>% select(year,Province,Commune,tot_pop,propPrimSec)
# 198 records.  All 2011 and 2012. Appears to be large chunks of entire provinces.  I'm guessing a mismatch with tot_pop.

# I've checked the raw data and the errors are in the raw data, not in my summaries. None of the values are much over 1, so I will just change them to 1
dat_merge <- dat_merge %>% mutate(propPrimSec = replace(propPrimSec, propPrimSec>1, 1))
str(dat_merge)

# BELOW ISSUE WAS WITH THE RAW DATA - NOW FIXED. BELOW CODE LEFT FOR REFERENCE

# histogram has an odd hump around 0.2. 
dat_merge %>% filter(propPrimSec >0.1 & propPrimSec <0.3) %>% select(year,Province,Commune,tot_pop,propPrimSec)
# 764 records. Seems to be a lot in 2008

dat_merge %>% filter(year=="2008") %>% 
  filter(propPrimSec >0.1 & propPrimSec <0.3) %>% 
  select(year,Province,Commune,tot_pop,propPrimSec)
# 713 are from 2008

# check histogram for just 2008
primsec.08 <- dat_merge %>% filter(year==2008) %>% select(Province,Commune,propPrimSec, tot_pop)
hist(primsec.08$propPrimSec)

# compare 2008 histogram with other years
ggplot(data = dat_merge, aes(dat_merge$propPrimSec))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# There is clearly something wrong with 2008.  I'll check the raw data



    # propSecSec ####

hist(dat_merge$propSecSec)
# very low numbers. Not that surprising.  Cambodia is an agricultural economy (and society), and so I wouldn't expect high numbers here.

# Check the high numbers 
dat_merge %>% filter(propSecSec > 0.6) %>% select(year,Province,Commune,propSecSec)
# one commune with all the high values in 2007, 2009, 2010

# plot all the years for that commune
ggplot(dat_merge, aes(x=year,y=propSecSec))+
  geom_point(data = subset(dat_merge, Commune=="Kaoh Dach"))
# 2011 and 2012 are missing

dat_merge %>% filter(Commune=="Kaoh Dach") %>% select(year,Province, Commune,propSecSec)
# 2011 and 2012 missing

# check village data
socioecon.dat %>% filter(Commune=="Kaoh Dach") %>% select(Year,Province, Commune)
# It's not in the village data so I must have removed it previously.

# I will double check the 2008 raw data as it's the anomoly

# I had made a mistake in the 2008 data, which I have now corrected. Check that 2008 histogram is similar to other years
ggplot(dat_merge, aes(dat_merge$propSecSec))+
  geom_histogram()+
  facet_grid(cols=vars(year))


    # LOAD LATEST VERSION ####

write.csv(dat_merge, file="Data/commune/dat_merge.csv")

dat_merge <- read.csv("Data/commune/dat_merge.csv", header = TRUE)
str(dat_merge)
dat_merge <- dat_merge %>% select(-X)

    # propTerSec ####

hist(dat_merge$propTerSec)
# this looks fine

# check larger values
dat_merge %>% filter(propTerSec > 0.3) %>% select(year,Province,Commune)

# plot the values for Kracheh > Chhloung
ggplot(dat_merge, aes(x=year,y=propTerSec))+
  geom_point(data = subset(dat_merge, Commune=="Chhloung"))
# previously there was an issue with 2008, which was due to my error in data extraction. This has been fixed. Now 2007, 2011, and 2012 look odd. 

# plot histograms of propTerSec for each year 
ggplot(dat_merge, aes(dat_merge$propTerSec))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# histograms don't look too bad. Definitely a change in shape in 2011 and 2012. I will need to keep an eye on this variable and remove it if necessary.


    # propQuatSec (RUN) ####

hist(dat_merge$propQuatSec)
# some values over 1

# which values are over 1
dat_merge %>% filter(propQuatSec >1) %>% select(year,Province,Commune,propQuatSec)
# 7 communes, values only slightly over 1.  Must be mismatch between summed people in this sector and the tot_pop value.  I will change them to 1

dat_merge <- dat_merge %>% mutate(propQuatSec = replace(propQuatSec, propQuatSec>1, 1))

# what are the other high values
dat_merge %>% filter(propQuatSec > 0.7) %>% select(year,Province,Commune,propQuatSec)

# check some of the communes
ggplot(dat_merge,aes(x=year,y=propQuatSec))+
  geom_point(data = subset(dat_merge, Commune=="Chob"))
# 2011 and 2012 again looking different 

ggplot(dat_merge,aes(x=year,y=propQuatSec))+
  geom_point(data = subset(dat_merge, Commune=="Setbou"))
# same

# plot all propQuatSec values per year to compare 2008 with the other years

ggplot(dat_merge, aes(dat_merge$propQuatSec))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# 2011 and 2012 seem to have a large increase in low values

    # Les1_R_Land (RUN) ####

hist(dat_merge$Les1_R_Land)
# values over 1

dat_merge %>% filter(Les1_R_Land >1) %>% select(year, Province, Commune, Les1_R_Land)
# only 1 commune in one year.  I will change it to 1

dat_merge <- dat_merge %>% mutate(Les1_R_Land = replace(Les1_R_Land, Les1_R_Land>1, 1))

# check histograms by year
ggplot(dat_merge, aes(dat_merge$Les1_R_Land))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# Looks ok

    # Les1_F_Land (RUN) ####

hist(dat_merge$Les1_F_Land)
# very different shape to Les1_R_Land. I suppose far fewer people will have absolutely no land to plant anything.  Most people will have a small vegetabloe patch or garden, even if they don't have enough land to plant rice.

# check no values above 1
dat_merge %>% filter(Les1_F_Land >1)
# none

# check histogram for each year
ggplot(dat_merge, aes(dat_merge$Les1_F_Land))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# 2011 and 2012 have different histo shapes. I'm a bit worried that the change in the question types in the CDB is causing issues in classification. This might cause jumps in values from 2010 to 2011

# take a sample from dat_merge
spl <- dat_merge %>% filter(Commune=="Sochet"|Commune=="Mukh Paen"|Commune=="Svay Chek"|Commune=="Phluk")

ggplot(spl, aes(x=year, y=Les1_F_Land))+
  geom_point()+
  facet_wrap(~Commune, ncol = 2)
# I think in a lot of cases the values are 0 for 2007-2010 and then big spike in 2011 and 2012.

ggplot(dat_merge, aes(x=year, y=Les1_F_Land))+
  geom_point()
# Either there is a massive jump in the number of people losing farming land, or the data is dodgy.  I can't think of a reasonable explanation for why there would be such a spike.

# I will double check the raw data for 2011 and 2012, but if the raw data and my extractions are correct, I will need to drop this variable.

# I have double checked the raw data and I haven't made any errors in extraction or conversio to proportions. Therefore I think there are potentially changes in the questions or data collection methods in 2011.  Therefore I will remove this variable. 

dat_merge <- dat_merge %>% select(-Les1_F_Land)

    # buff_fam (RUN) ####

hist(dat_merge$buff_fam)
# no obvious problems

dat_merge %>% filter(buff_fam > 1)
# none

# check histo for each year
ggplot(dat_merge, aes(dat_merge$buff_fam))+
  geom_histogram()+
  facet_grid(cols = vars(year))
# WOAH.  Something very dodgy here.  2007, 2008, 2009, 2010 all look vaguely similar. 2011 is fucked (so wa 2008 but that was an error on my part which I have fixed in the raw data).  2012 looks believable but totally different to the other years. 

# I've re-done the 2011 data extraction and this is what the raw data says. Therefore I will drop this variable

dat_merge <- dat_merge %>% select(-buff_fam)


    # LOAD LATEST VERSION ####

write.csv(dat_merge, file="Data/commune/dat_merge.csv")

dat_merge <- read.csv("Data/commune/dat_merge.csv", header = TRUE)
str(dat_merge)
dat_merge <- dat_merge %>% select(-X)

    # pig_fam ####

hist(dat_merge$pig_fam)
# looks sensible

dat_merge %>% filter(pig_fam > 1)
# none

# histo for each year
ggplot(dat_merge, aes(dat_merge$pig_fam))+
  geom_histogram()+
  facet_grid(cols = vars(year))

# there was an issue with the 2008 raw data - fixed now

    # dist_sch ####

hist(dat_merge$dist_sch)
# surprisingly not totally unreasonable

# check where the larger values are from
dat_merge %>% filter(dist_sch > 60) %>% select(year,Province,Commune,dist_sch)
# mostly the same commune just different years - Mondulkiri province which is believable 

dat_merge %>% filter(dist_sch > 40) %>% select(year,Province,Commune,dist_sch)
# Mondulkiri, Rattankiri, Preah Vihear, Stung Streng - all believable



    # garbage ####

hist(dat_merge$garbage)
# not many folk with access to garbage collection.  I would be surprised with anything else

dat_merge %>% filter(garbage>1)
# none

# check communes with high values
dat_merge %>% filter(garbage>0.6) %>% select(year,Province, Commune,garbage)
# what I was expecing - phnom penh mostly


    # KM_Comm ####

hist(dat_merge$KM_Comm)
# looks fine

max(dat_merge$KM_Comm)

# check where high values are from
dat_merge %>% filter(KM_Comm > 30) %>% select(year,Province,Commune,KM_Comm)
# mostly same two places, over the years.  A couple of new places appear in 2011 but there is no way t verify this really - it is perfectly feasible that the comune office moved or a new one was built elsewhere. That happens a lot

    # KM_Heal_cent ####

hist(dat_merge$KM_Heal_cent)
# some quite large value here, but not necessarily wrong

# check loactions of high values
dat_merge %>% filter(KM_Heal_cent>50) %>% select(Province,Commune,KM_Heal_cent)
# again it's all the large, remote provinces - Mondulkiri, PReah Vihear, Stung Treng etc. Not implausible, and no real way of checking or correcting.

    # land_confl ####

hist(dat_merge$land_confl)
# handful of very large values. Not sure how to check this, but I will look at the proportion to the population

dat_merge %>% filter(land_confl > 100) %>% select(year,Province,Commune,land_confl,tot_pop)
# all the communes with high values have relatively large populations, so there is no reason to doubt the numbers

    # crim_case ####

hist(dat_merge$crim_case)
# Mostly very low, which I can believe. Cambodia has a relatively low crime rate, plus what crime there is a lot will go un-reported

# split by year
ggplot(dat_merge, aes(pop_den))+
  geom_histogram()+
  facet_wrap(dat_merge$year, nrow=2)
# 2009 looks odd compared to the other years. I will check in the raw data

# error with raw data in 2009 - fixed now.

# check high values
dat_merge %>% filter(crim_case > 0.05) %>% select(year,Province,Commune,crim_case,tot_pop)
# Pailin is on the Thai border, Battambang is a very large city, Kampong Cham in what Phnom Penh sprawls into. Same with Kampong Speu.  Sihanouk - no surprises there. All those make sens to me.  Not sure about Rattanikiri or Kracheh though.  NO way of verifying or checking though, so will have to go with it.

    # inf_mort (RUN) ####

hist(dat_merge$inf_mort)
# very low values, which is a good thing I suppose.  

dat_merge %>% filter(inf_mort ==0) %>% select(year,Province, Commune, inf_mort)
#  hmmm, this smells like bullshit to me.  1645 communes had zero infant deaths in a given year.  No chance.  This will be poor/under/no reporting.  NO doubt there will be taboos about reporting such things, and in the rural provinces, there won't be any mechanism or requirement for reporting, as most births will be at home with traditional midwives.

# I will drop this variable as I just don't believe it

dat_merge <- dat_merge %>% select(-inf_mort)


    # U5_mort (RUN) ####

hist(dat_merge$U5_mort)
# looks similar to inf_mort

dat_merge %>% filter(U5_mort == 0) %>% select(year,Province,Commune)
# 1443 communes with zero under 5 deaths.  The problem here is that a lot of the communes are in the really remote provinces, which suggests to me that this is a reporting problem.

# drop the variable as I don't trust it

dat_merge <- dat_merge %>% select(-U5_mort)


    # Pax_migt_in (RUN) ####

hist(dat_merge$Pax_migt_in)

# check outlier
dat_merge %>% filter(Pax_migt_in > 5000) %>%  select(year,Province,Commune,Pax_migt_in,tot_pop)
# This is defo an error - the number of migrants is more than 2000 people larger than the total population

# check the value for the rest of the province
dat_merge %>% filter(Province=="Kampong Cham" & year==2011) %>% 
              select(year,Commune,Pax_migt_in) 
# the rest of the communes seem reasonable. I will replace the value for Cheyyou with the Province mean

paxmean <- mean(dat_merge$Pax_migt_in[dat_merge$Province=="Kampong Cham"|dat_merge$year==2011])
dat_merge <- mutate(dat_merge, Pax_migt_in = replace(Pax_migt_in, Pax_migt_in==16341, paxmean))


# check other outliers
dat_merge %>% filter(Pax_migt_in > 3000) %>%  select(year,Province,Commune,Pax_migt_in,tot_pop)
# this could be true, if it wasn't for the fact that the values are identical for 2008 and 2010

dat_merge %>% filter(Province=="Pailin" & Commune=="Stueng Kach") %>% select(year,Pax_migt_in,tot_pop)
# those two years look like errors - they don't fit the rest of the values, and are identical which is clearly wrong. I will set the two Pax values to the provincial mean for each year

mean(dat_merge$Pax_migt_in[dat_merge$Province=="Pailin"|dat_merge$year==2008])
mean(dat_merge$Pax_migt_in[dat_merge$Province=="Pailin"|dat_merge$year==2010])
# hmm ok so the reason the values are the same is because the Pax_migt_in values for the province are the identical for those two years. 

dat_merge %>% filter(Province=="Pailin" & Commune=="Stueng Kach") %>% select(year,Pax_migt_in,tot_pop)
# ok so instead I am going to set those two values as the mean for the commune acorss all years, excluding the two dogy values

# mean for
(293+468+292+368)/4

# replace values
dat_merge <- dat_merge %>% mutate(Pax_migt_in = replace(Pax_migt_in,Pax_migt_in==4018, 355)) 

dat_merge %>% filter(Province=="Pailin") %>% select(year,Commune,tot_pop)

# check other outliers
dat_merge %>% filter(Pax_migt_in > 2000) %>%  select(year,Province,Commune,Pax_migt_in,tot_pop)
# looks like a similar issue with Trapeang Prasat in Otdar Meanchey

# check the commune
dat_merge %>% filter(Commune=="Trapeang Prasat") %>%  select(year,Commune,Pax_migt_in,tot_pop)

# plot it
ggplot(dat_merge[dat_merge$Commune=="Trapeang Prasat",], aes(x=year, y=tot_pop))+
  geom_line()+
  geom_line(aes(y=Pax_migt_in))
# plot doesn't make it look unreasonable, but the identical numbers do. The rest of the migration figures suggest decreasing migration. I will set the populatin and migration values for those two years to be the mean of the two years either side

# Pax_migt_in
dat_merge <- dat_merge %>% mutate(Pax_migt_in = replace(Pax_migt_in, 
                                                        which(Pax_migt_in==2584 & year==2008),
                                                        880))

dat_merge <- dat_merge %>% mutate(Pax_migt_in = replace(Pax_migt_in, 
                                                        which(Pax_migt_in==2584 & year==2010),
                                                        521))

# I will change the population in the tot_pop section

# all the other large Pax_migt_in values (> 2000 look reasonable)

    # Pax_migt_out ####

hist(dat_merge$Pax_migt_out)
# some large values that need checking

dat_merge %>% filter(Pax_migt_out > 2000) %>% select(year,Province,Commune,Pax_migt_out,tot_pop)
# actually don't look unreasonable. Not ridiculously large relative to the total population.  All in Banteay Meanchey though
# plot the province

ggplot(dat_merge[dat_merge$Province=="Banteay Meanchey",], aes(x=year, y=tot_pop, group=Commune))+
  geom_line()+
  geom_line(aes(y=Pax_migt_out))
# doesn't look unreasonable. Jsut looks like a large increase in migration out of the province in 2010 onwards, but it is consistent across all communes

# check more large values
dat_merge %>% filter(Pax_migt_out > 1500) %>% select(year,Province,Commune,Pax_migt_out,tot_pop)
# no obvious errors jumping out at me

    # mean_elev ####

hist(dat_merge$mean_elev)

ggplot(dat_merge, aes(x=year, y=mean_elev, group=Commune))+
  geom_line()+
  facet_wrap(dat_merge$Province,nrow=2, ncol=12)
# No communes have changing elevation which is good!  All look fine

    # LOAD LATEST VERSION ####

write.csv(dat_merge, file="Data/commune/dat_merge.csv")

dat_merge <- read.csv("Data/commune/dat_merge.csv")
str(dat_merge)
dat_merge <- dat_merge %>% select(-X)
dat_merge$year <- as.factor(dat_merge$year)

    # habitat ####

ggplot(dat_merge, aes(x=year, y=habitat, group=Province))+
  geom_count()+
  facet_wrap(dat_merge$Province, nrow=2, ncol=12)
# can't see any obvious issues

    # dist_border ####

hist(dat_merge$dist_border)
unique(dat_merge$Province[dat_merge$dist_border>100])

ggplot(dat_merge, aes(y=dist_border))+
  geom_boxplot()+
  facet_wrap(dat_merge$Province, nrow=2, ncol=12)
# some potential outliers worth investigating

dat_merge %>% filter(Province=="Koh Kong" & dist_border > 50) %>% select(year,Commune,dist_border)
dat_merge %>% filter(Province=="Koh Kong") %>% select(Commune,dist_border)
hist(dat_merge$dist_border[dat_merge$Province=="Koh Kong"])
# Koh Kong looks fine - the shape of the province means the data make sense

dat_merge %>% filter(Province=="Mondul Kiri" & dist_border > 75) %>% select(year,Commune,dist_border)
# Makes sense after checking commune map on QGIS

dat_merge %>% filter(Province=="Prey Veng" & dist_border > 50) %>% select(year,Commune,dist_border)
hist(dat_merge$dist_border[dat_merge$Province=="Prey Veng"])
# slightly odd gap in distances between the largest and the next largest

dat_merge %>% filter(Province=="Prey Veng" & dist_border > 40) %>% 
  select(year,Commune,dist_border) %>% 
  arrange(dist_border)
# ok so I think the issue is that not all of the communes are present (probably because the missing ones didn't have any forest (false zeros)). So this is fine

dat_merge %>% filter(Province=="Pursat" & dist_border < 25) %>% 
  select(year,Commune,dist_border) %>% 
  arrange(dist_border)
# ok this is also fine. Pursat has only a few, large, communes.  Thma Da and ANlong Reab are the two closest to the coast. 

#
    # dist_provCap ####

hist(dat_merge$dist_provCap)
# check some of the larger values

dat_merge %>% filter(dist_provCap >70) %>% select(year,Province,Commune,dist_provCap)

# plot histograms for provinces with large values
pvs <- c("Kampong Cham","Koh Kong","Kracheh","Mondul Kiri",
          "Pursat","Preah Vihear","Stung Treng","Otdar Meanchey")
provCapProv <- dat_merge %>% filter(Province %in% pvs)

ggplot(provCapProv, aes(dist_provCap))+
  geom_histogram()+
  facet_wrap(provCapProv$Province, nrow=2, ncol=4)
# when the provincial histograms are examined along with the map in QGIS with the provincial captials, these all make sense. They are due to the shapes of the provinces and the locations of the provincial captials


# plot all provinces
ggplot(dat_merge, aes(y=dist_provCap))+
  geom_boxplot()+
  facet_wrap(dat_merge$Province, nrow=2, ncol=12)


    # elc ####

ggplot(dat_merge, aes(x=year, y=elc))+
  geom_count()+
  facet_wrap(dat_merge$Province, nrow=2, ncol=12)


# although in QGIS there are two ELCs in Battambang, the communes are not in the dat-merge dataset, therefore were removed at an earlier stage (e.g because of no forest)

# All of the rest with no ELCs have none in the GIS layer so are correct. 

    # PA ####

ggplot(dat_merge, aes(x=year, y=PA))+
  geom_count()+
  facet_wrap(dat_merge$Province, nrow=2, ncol=12)

# checked against GIS - all correct

    # PA_cat ####

ggplot(dat_merge, aes(x=year, y=PA_cat, colour=PA_cat))+
  geom_count()+
  facet_wrap(dat_merge$Province, nrow=2, ncol=12)
# all look fine


#### Variable selection - correlations & PCA -------------------------------------------


# I will look at the correlations within the different sets.  If there are correlated variables (>0.5) then those variables will be put into a PCA, and the variable that contributes the most to the top priciple component will be selected. 


  # Population demographics ####


# Population demographics (tot_pop, family, male_18_60, fem_18_60, pop_over61, 
                        # tot_ind, prop_ind, pop_den)

popDenCor <- cor(dat_merge[8:15], use = "complete.obs")

# as expected, total population, famliy, male, female, and over 61 population are all highly correlated with each other. 

# total indigneous and proportion indigenous are obviously correlated with each other, but not really with anything else which is good. I think proportion indigenous is the more important variable here, as it describes the ethnic makeup of the commune.

# population density is also not highly correlated with anything. This is becasue commune size is so variable and is nothing to do with total population. 

# plot all
datPop <- dat_merge %>% select(tot_pop,family,male_18_60,fem_18_60, pop_over61)
pairs(datPop)

# One argument would be to use the population of males, as in this cultural setting males are the most likely to be engaged in activities that might cause deforestation. But in reality, if there are more males then there will be more females and families (as we see in the correlation plots). And total population will also provide information about unrecorded "factors" such as urbanisation, resource demand, expansion of villages and towns etc. 


# I think tot_pop is the most appropriate, but lets use PCA to see which variables out of the population ones above explain the most variation 
datPop <- dat_merge %>% select(tot_pop,family,male_18_60,fem_18_60, pop_over61)
popPCA <-  PCA(datPop, scale.unit=TRUE, ncp=5, graph=TRUE)

# eigenvalues
eig.val <- get_eigenvalue(popPCA)

# scree plot
PopScree <- fviz_eig(popPCA, addlabels = TRUE, ylim = c(0, 80))
Popvars <- get_pca_var(popPCA)

# scree plot with cos2
Popvars_plot_CorrCos2 <- fviz_pca_var(popPCA, col.var = "cos2",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                       repel = TRUE)
Popvars_plot_CorrCos2

# cos2 values
Popvars_cos2 <-corrplot(Popvars$cos2, is.corr=FALSE)

# bar plot of cos2
Popvars_cos2Bar <- fviz_cos2(popPCA, choice = "var", axes = 1:2)

# Contributions of variables to PC1
Popvars_contrib_PC1 <- fviz_contrib(popPCA, choice = "var", axes = 1)

# scree plot with colours by contribution
Popvars_plot_corrContrib <- fviz_pca_var(popPCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
Popvars_plot_corrContrib


## tot_pop is the most powerful variable in the set. It contributes the most to PC1 (which is the only PC that has any power). This makes sense really - all of the other variables are simply different sections within tot_pop, and so by definition tot_pop will contain all of the information of the others.  



## population demographic variables selected are tot_pop, prop_ind, pop_den.


  # Education ####


# Education (F6_24_sch, M6_24_sch, F15_45_ill, M15_45_ill)
datEdu <- dat_merge %>% select(F6_24_sch, M6_24_sch, F15_45_ill, M15_45_ill)

EduCor <- cor(datEdu, use = "complete.obs")
# as I expected, these are all correlated. 

# plot all
pairs(datEdu)

# so really, all I actually need is one of the variables. I am going to use M6_24_sch, becasue in this cultural context, males are by far the most likely to be conducting activities that contribute to forest loss (land clearance, logging etc).  Because of the highly correlated nature of the set, I can say with quite a lot of confidence that when there are more males in school, there are also more females. And the more males that are in school, the higher the literacy rates are for boys and girls.  

# education var selected is  M6_24_sch
#
  # Employment ####


# Employment (propPrimLivFarm, propPrimSec, propSecSec, propTerSec, propQuatSec)

EmpCor <- cor(dat_merge[20:24], use = "complete.obs")

# negative correlation between propPrimSec and propTerSec, and propPrimSec and propQuatSec

# This makes sense - as the proportion of people employed in the primary sector increases, the proportion of people in the tertiary and quaternary sector decreases - the proportions of these variables altogether make up the theoretical "whole" popopulation, and so as one decreases, some of the others must increase, and vice versa.  In the more rural provinces, I am expecting higher proprtions of people in the primary sector, and fewer people in the other sectors, whereas in more urban/developed provinces I will expect the opposite. I also expect that in communes/provinces with higher population density (generally more urbanised areas), I will see higher proportions of secondary, tertiary, and quaternary.

# plot the relationships
datEmp <- dat_merge[ ,20:24]
pairs(datEmp)
# despite a low R value, there is a clear relationship between propPrimLivFarm and propPrimSec, which is what I was expecting - the two variables are covering the same thing, just propPrimSec includes extra information (fishing, NTFP etc.).  Because propPrimSec includes more information of interest, I am inclinde to use this variable. But I will have a lok at the PCA results. 

# I will also need to see the PCA results for propTerSec and propQuatSec, as they are correlated with propPrimSec.  Again, I am inclined to just use PropPrimSec, as this includes information on occupations that are relevant to deforestation, and I can now say that as propPrimSec goes up (down) the other sectors are likely to down (up).

# PCA
empPCA <-  PCA(datEmp, scale.unit=TRUE, ncp=5, graph=TRUE)

# eigenvalues
eig.val <- get_eigenvalue(empPCA)

# scree plot
EmpScree <- fviz_eig(empPCA, addlabels = TRUE, ylim = c(0, 80))
Empvars <- get_pca_var(empPCA)

# scree plot with cos2
Empvars_plot_CorrCos2 <- fviz_pca_var(empPCA, col.var = "cos2",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                       repel = TRUE)
Empvars_plot_CorrCos2

# cos2 values
Empvars_cos2 <-corrplot(Empvars$cos2, is.corr=FALSE)

# bar plot of cos2
Empvars_cos2Bar <- fviz_cos2(empPCA, choice = "var", axes = 1:2)

# Contributions of variables to PC1
Empvars_contrib_PC1 <- fviz_contrib(empPCA, choice = "var", axes = 1)

# Contributions of variables to PC2
Empvars_contrib_PC2 <- fviz_contrib(empPCA, choice = "var", axes = 2)

# scree plot with colours by contribution
Empvars_plot_corrContrib <- fviz_pca_var(empPCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
Empvars_plot_corrContrib

## propPrimSec and propSecSec have emerged as the winners. This is good - propPrimSec is probably the most intuitively useful variable, and becasue of the correlation with TerSec and QuatSec we can say certain things about them even if they are not included.  Not sure whether SecSec will end up being useful, but it is not correlated with anything so no harm in including it. 

### employment variables selected are propPrimSec and propSecSec



  # Economic security ####

# Economic security (Les1_R_Land, pig_fam). Les_1_F_Land and buff_fam had to be removed as they were unreliable (See data cleaning section)

datEcSec <- dat_merge %>% select(Les1_R_Land, pig_fam)

# I imagine that these two variables will be slightly correlated - the communes with more people that have little or no rice land are likely to be the more urbanised areas, where they are also less likely to keep pigs. Likewise, if a family has pigs, they are also likely to have some land for rice, and vice versa.  

EcSeccorr <- cor(datEcSec, use = "complete.obs")
# huh, ok, perhaps not.

# plot
plot(datEcSec)
# wow, that is literally the least amount of correlation I have ever seen. Interestingly, there a a bunch of communes where families have no rice land at all, but do keep pigs. Could these represent pig farms, where the primary income is raising pigs and therefore no need for cultivated land? 

# This makes the decision easy - both variables are retained. 

# one thing to keep in mind is the issue of families living in cities or urbanised areas, but still having "family" farms out in the provinces. The commune databse doesn't specifiy whether the question relates to families having no rice land in the commune they reside in, or whether they have no rice land AT ALL.  If it is the former, you could expect that there will be a low proportion of families with no rice land even in urban areas, becasue the families are reporting that they do have rice land elsewhere.

  # Access to services ####


# Access to services (dist_sch, garbage, KM_Comm, KM_Heal_cent)

datAccSec <- dat_merge %>% select(dist_sch, garbage, KM_Comm, KM_Heal_cent) 

# I reckon that dist_sch, KM_Comm, and KM_Heal_cent may be in some way correlated, as distances to all of these things are likely to be large for communes in larger, more, remote, more sparsley populated Provinces. First because these provinces tend to be the more rural, poorer provinces and so access to services will be lower, but also because in these larger provinces, the communes tend to be larger too, meaning distances will be larger. garbage basically doesn't really exist outisde of the major urban centres, so not really sure about this one.

AccSeccorr <- cor(datAccSec, use = "complete.obs")
# distance to school and distance to health centre are correlated

# plot them
pairs(datAccSec)
# If I had to choose one to pick with no other information, I would probably select dist_sch. This is becuase so much of the illegal forest clearance and logging is done by young lads, who are not in school.  Large distances to school I would imagine being a factor in them not going.  I'll run the PCA anyway and see what it looks like

# PCA
AccSecPCA <-  PCA(datAccSec, scale.unit=TRUE, ncp=5, graph=TRUE)

# eigenvalues
eig.val <- get_eigenvalue(AccSecPCA)

# scree plot
AccSecScree <- fviz_eig(AccSecPCA, addlabels = TRUE, ylim = c(0, 80))
AccSecvars <- get_pca_var(AccSecPCA)

# scree plot with cos2
AccSecvars_plot_CorrCos2 <- fviz_pca_var(AccSecPCA, col.var = "cos2",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                       repel = TRUE)
AccSecvars_plot_CorrCos2

# cos2 values
AccSecvars_cos2 <-corrplot(AccSecvars$cos2, is.corr=FALSE)

# bar plot of cos2
AccSecvars_cos2Bar <- fviz_cos2(AccSecPCA, choice = "var", axes = 1:2)

# Contributions of variables to PC1
AccSecvars_contrib_PC1 <- fviz_contrib(AccSecPCA, choice = "var", axes = 1)

# Contributions of variables to PC2
AccSecvars_contrib_PC2 <- fviz_contrib(AccSecPCA, choice = "var", axes = 2)

# scree plot with colours by contribution
AccSecvars_plot_corrContrib <- fviz_pca_var(AccSecPCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
AccSecvars_plot_corrContrib

## Take home message is that ideally I choose to drop either dist_sch or KM_Heal_cen. Garbage and KM_Comm can stay for now.  For the reasons I describe above, I am going to selet dist_sch to move forward with

## Final access to services variables are dist_sch, KM_Comm, garbage
  # Social justice ####

# Social justice (land_confl, crim_case)

# I don't necesarily think that these two will be correlated.  I imagine that criminal cases will be higher in more densely populated, urban communes, whereas land conflicts will be in more rural areas.

datSocJus <- dat_merge %>% select(land_confl, crim_case)

# check correlation
SocJuscorr <- cor(datSocJus, use = "complete.obs")
# no correlation

# plot
ggplot(datSocJus, aes(x=land_confl, y=crim_case))+
  geom_point()
# some quite large outliers here

## this means both variables will be taken forward

  # Health ####

# this no longer exists as I had to remove both variables due to poor data quality

  # Migration ####

# Migration (Pax_migt_in, Pax_migt_out)

# I don't think these will be correlated because if a commune has a lot of in-migration then surely there wouldn't be much out-migration because people are normally pulled into an area for work, and so if an area has a lot of available opportunitie then not many people would be leaving.  I imagine that a lot of the more urban communes will have in-migration, although there will be some more rural areas that will be pulling people in because of things like land concessions that require a lot of man-power (and associated services)

datMig <- dat_merge %>% select(Pax_migt_in, Pax_migt_out)

# correlation
migCorr <- cor(datMig, use = "complete.obs")
# not entirely uncorrelated, but nothing major

# plot them
ggplot(datMig, aes(x=Pax_migt_in, y=Pax_migt_out))+
  geom_point()
# although the R is fairly low, there is a slight relationship in the form that I predicted - as in-migration increases, generally out-migration decreases.

### both variables are selected to go forward


  # Environmental additional ####

# Environmental additional (mean_elev, habitat) 

# no need to worry about correlation here, but interesting to plot them

datEnv <- dat_merge %>% select(mean_elev, habitat)

ggplot(datEnv, aes(x=habitat, y=mean_elev))+
  geom_boxplot()

# the highest elevation cluster of communes are broadleaved, evergreen forest. This is what I would expect becuase of places like Seima, the Cardamoms, and Stung Treng.  The habitat with the highest median elevation is mosaic (natural), i.e. mosaic of natural and cropland but natural cover is >50%.  no data (ND) and grassland (GL) have virtually no points, so I wonder whether I will need to take these communes out at some point.  Mosaic (inclusing trees, shrubs, herbaceous cover etc.) has the second highest meadiun elevation and also the second highest cluster of high elevation communes. Cropland (CP) has generally low elevation communes (low median), although it does have a cluster of higher elevation communes, between 100 and 200m.  Shrubland and broadleaved evergreen forest appear to have the most number of communes. 

hist(datEnv$mean_elev)

  # Human additional ####

# Human additional (dist_border, dist_provCap, elc, PA, PA_cat)

datHum <- dat_merge %>% select(dist_border, dist_provCap, elc, PA, PA_cat)
# I don't think I can see any reason why dist_border and dist_ProvCap would be correlated. Distance to border will be affected by the location of the commune within the province,and the location of the province in the country. Whereas distance to provincial capital will be mostly affected by the size of the province, and the location of the commune within it.  I don't really see a relationship between size of province and location of province either.  

# correlation for 2 continous vars
Humcorr <- cor(datHum[ ,1:2], use = "complete.obs")
# no correlation

# plot 2 continuous vars
ggplot(datHum, aes(x=dist_border, y=dist_provCap))+
  geom_point()
# absolutely nowt

### ELC

# Two continous vars split by elc
ggplot(datHum, aes(x=dist_border, y=dist_provCap, group=elc, colour=elc))+
  geom_point()

# boxplot with dist_border and ELC presence
ggplot(datHum, aes(x=as.factor(elc), y=dist_border))+
  geom_boxplot()
# in general, communes with ELCs are closer to a border. This is what I expected - ELCs tend to be placed in the more remote, rural provinces (which also have the most forest and natural cover).

# boxplot with dist_ProvCap and ELC presence
ggplot(datHum, aes(x=as.factor(elc), y=dist_provCap))+
  geom_boxplot()
# communes with ELCs on average are further away from provincial capitals. ELcs tend to be placed in more remote, forested areas, so this doesn't come as a big surprise. 

# count plot comparing ELC presence and PA presence
ggplot(datHum, aes(x=as.factor(elc), y=as.factor(PA)))+
  geom_count()
# Interesting. Obviously most communes have neither ELC nor PA.  But then next biggest category is communes with no ELC and a PA.  I suppose this is a good thing!  The next biggest category is communes with and ELC and no PA.  I suppose this is also a good thing!

# facet wrap contunous vars, split by ELC presence
ggplot(datHum, aes(x=dist_border, y=dist_provCap))+
  geom_point()+
  facet_wrap(datHum$elc, nrow=2)
# no obvious relationship. The only thing I can see is that there is slightly more variation (spread) in distance to provincial capital in communes that are close to the border. One explanation for this would be that the communes closer to a border are larger, and so the avergae distance to the capital is larger. Although this doesn't explain the spread in the other direction.  Perhaps it's just that there are more communes with small dist_border values and so there is more deviation jsut because there are more points.


### PA 

# continuous vars split by PA presence
ggplot(datHum, aes(x=dist_border, y=dist_provCap))+
  geom_point()+
  facet_wrap(datHum$PA, nrow=2)
# slight wiggly shape in the communes with no PA. No obvious other pattern though

# now include PA_cat
ggplot(datHum, aes(x=dist_border, y=dist_provCap, group=PA_cat, colour=PA_cat))+
  geom_point()

# boxplot of dist_border and PA presence
ggplot(datHum, aes(x=as.factor(PA), y=dist_border))+
  geom_boxplot()
# interestingly, there is no real difference here

# boxplot of dist_ProvCap and PA presence
ggplot(datHum, aes(x=as.factor(PA), y=dist_provCap))+
  geom_boxplot()
# This is probably what I would expect - communes with PAs are more remote and further away from urban centres.  They are also more likely to be in the larger, more remote provinces where ditances in general are larger. 

# count plot of PA and PA_cat
ggplot(datHum, aes(x=as.factor(PA), y=PA_cat))+
  geom_count()
# most PAs are wildlife sanctuaries, followed by multi-use areas, then National Parks

# plot both continuous vars split by PA_cat
ggplot(datHum, aes(x=dist_border, y=dist_provCap))+
  geom_point()+
  facet_wrap(datHum$PA_cat, nrow=4)
# gap in communes with MUA between 30-75km from a border.  Communes with more than one category (MULTI) tend to have higher values of distance to capital. This could suggest that communes with multiple PAs are in more remote communes in the larger, more remote provinces. This fits with my expectation. National parks and Ramsar sites do not exit in communes further away from the border than 100kmm which means they all exist in provinces close to a border, than than in the middle of the country. For wildlife sanctuaries, there are two "groups" of communes. The first group includes communes that are close to a border (generally <50km), and they have a wider spread of dist_ProvCap values. The second group have larger dist_border values (>75km) and also higher dist_ProvCap values. 

## Correlation between all remaining variables ####

# Above I have removed certain variables within sets, but now I need to ensure there is no correlation between variables in different sets (see Harrison et al 2018)

# load data
dat <- read.csv("Data/commune/dat_use.csv")

# extract numeric vars to be tested
dat1 <- dat[ ,c(9:24, 26:27)]

# correaltion matrix
allvarcor <- cor(dat1, use = "everything")
write.csv(allvarcor, file="socioecon_vars_corr.csv")

# none of the final variables are correlated with each other (max is 0.4)

#### FINAL DATASET ----------------------------------------------------------------------



str(dat)

# based on the above variable exploration and selection, I will remove the vars I don't want
dat_use <- dat_merge %>% select(year,Province, Commune,commGIS,areaKM,ForPix,diffPix,
                                tot_pop, prop_ind, pop_den,
                                M6_24_sch,
                                propPrimSec, propSecSec,
                                Les1_R_Land, pig_fam,
                                dist_sch, garbage, KM_Comm,
                                land_confl, crim_case,
                                Pax_migt_in, Pax_migt_out,
                                mean_elev, habitat,
                                dist_border, dist_provCap, elc, PA, PA_cat)

str(dat_use)

# write file
write.csv(dat_use, file="Data/commune/dat_use.csv")
