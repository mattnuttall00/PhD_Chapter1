#### This is the data aggregating and data cleaning code for the socioeconomic analysis of chapter 2 (data chapter 1) in my PhD.  The data are from the Commune Database of Cambodia for the years 2007-2012. The land cover data are from the European Space Agency Climate Change Initiative satellite.

##  To skip data aggregation process load 'dat_merge' file from "clean, format, and error check data" section (line 640) 

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

#### Socioeconomic data -----------------------------------------------------------------
### Load socioeconomic variable and commune data ####

socioecon.dat <- read.csv("Data/commune/socioecon_vars_07-12.csv", header=T) 

str(socioecon.dat)

socioecon.dat$Year <- as.factor(socioecon.dat$Year)
socioecon.dat$VillGis <- as.factor(socioecon.dat$VillGis)

commDat <- read.csv("Data/commune/commGIS.csv")
commDat$province <- as.character(commDat$province)
commDat$commune <- as.character(commDat$commune)
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

# Commune shapefile
com.shp <- readOGR(dsn = 'Spatial_data/boundary_khum.shp')

# set CRS to UTM zone 48, Indian 1960
proj4string(com.shp) <- CRS("+init=epsg:3148")

# explore shapefile
plot(com.shp, bg="transparent", add=T)
length(com.shp$CODEKHUM)
nrow(com.shp)
summary(com.shp)
head(com.shp@data)

# are there any polygons with area=0
length(com.shp$AREA[com.shp$AREA==0])
length(com.shp$AREA[com.shp$AREA<5000])

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


## Now I will join each year of socioeconoimc data to the corresponding year of forest data

# 2007
join07 <- inner_join(dat.07.agg, forest07.agg, by="commGIS")
join07 <- join07 %>% select(-year.y) %>% dplyr::rename(year=year.x)

# 2008
join08 <- inner_join(dat.08.agg, forest08.agg, by="commGIS")
join08 <- join08 %>% select(-year.y) %>% dplyr::rename(year=year.x)

# 2009
join09 <- inner_join(dat.09.agg, forest09.agg, by="commGIS")
join09 <- join09 %>% select(-year.y) %>% dplyr::rename(year=year.x)

# 2010
join10 <- inner_join(dat.10.agg, forest10.agg, by="commGIS")
join10 <- join10 %>% select(-year.y) %>% dplyr::rename(year=year.x)

# 2011
join11 <- inner_join(dat.11.agg, forest11.agg, by="commGIS")
join11 <- join11 %>% select(-year.y) %>% dplyr::rename(year=year.x)

# 2012
join12 <- inner_join(dat.12.agg, forest12.agg, by="commGIS")
join12 <- join12 %>% select(-year.y) %>% dplyr::rename(year=year.x)

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
pix09 <- for_pix[for_pix$year=="2008", ]
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
dat_merge$pop_den <- dat_merge$areaKM / dat_merge$tot_pop
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

# load working version of the data
dat_merge <- read.csv("Data/commune/dat_merge.csv", header=T)
str(dat_merge)
dat_merge <- dat_merge %>% select(-X)

# year to factor
dat_merge$year <- as.factor(dat_merge$year)

### re-arrange variables into their sets ####  

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

dat_merge <- dat_merge %>% select(year,Province, Commune, commGIS, areaKM,
                                  ForPix, diffPix,
                          tot_pop, family, male_18_60, fem_18_60, pop_over61, tot_ind, prop_ind, pop_den,
                                  F6_24_sch, M6_24_sch, F15_45_ill, M15_45_ill,
                      numPrimLivFarm, propPrimLivFarm, propPrimSec, propSecSec, propTerSec, propQuatSec,
                                  Les1_R_Land, Les1_F_Land, buff_fam, pig_fam,
                                  dist_sch, garbage, KM_Comm, KM_Heal_cent,
                                  land_confl, crim_case,
                                  inf_mort, U5_mort,
                                  Pax_migt_in, Pax_migt_out,
                                  mean_elev, habitat,
                                  dist_border, dist_provCap, elc, PA, PA_cat)

### Error checking ####

# load dat_merge
dat_merge <- read.csv("Data/commune/dat_merge.csv", header = T)

# area
hist(dat_merge$areaKM)
dat_merge %>% filter(areaKM >2000)
# some very large communes - I have checked - they are in Mondulkiri are are correct

# ForPix
length(dat_merge$ForPix[is.na(dat_merge$ForPix)])
hist(dat_merge$ForPix)
dat_merge %>% filter(ForPix < 10) %>% select(year, Province, Commune, ForPix)
# Lots of communes with small number of forest pixels.

# diffPix
length(dat_merge$diffPix[is.na(dat_merge$diffPix)])
hist(dat_merge$diffPix)
dat_merge %>% filter(diffPix < -800) %>% select(year, Province, Commune, commGIS, diffPix)
