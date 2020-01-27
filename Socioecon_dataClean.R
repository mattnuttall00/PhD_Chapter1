#### This is the data aggregating and data cleaning code for the socioeconomic analysis of chapter 2 (data chapter 1) in my PhD.  The data are from the Commune Database of Cambodia for the years 2007-2012. The land cover data are from the European Space Agency Climate Change Initiative satellite.

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

# Join tables
dat_master <- rbind(dat.07.agg,dat.08.agg,dat.09.agg,dat.10.agg,dat.11.agg,dat.12.agg)
str(dat_master)


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
forest_dat$khum_name <- as.factor(forest_dat$khum_name)
colnames(forest_dat)[2] <- "commGIS"
str(forest_dat)
head(forest_dat)

  ## Names ####

# there are some missing commune names in the forest cover data. I will first try and get those names from the socioeconomic data 
sum(is.na(forest_dat$khum_name)) # 216

# try and match the comm codes with dat_master and pull out the commune names
dat_master$Commune <- as.character(dat_master$Commune)
forest_dat$khum_name[is.na(forest_dat$khum_name)] <- dat_master$Commune[match(forest_dat$codekhum,
                                                                              dat_master$commGIS)]
sum(is.na(forest_dat$khum_name)) # now 16

forest_dat %>% filter(is.na(khum_name))
# there are some that are only NA in certain years which means the name exists in other years

forest_dat %>% filter(codekhum==90607)
dat_master %>% filter(commGIS==90607)
dat_master %>% filter(Commune=="Boeng Kak Muoy")

forest_dat <- forest_dat %>% 
              mutate(khum_name = ifelse(khum_name == is.na(khum_name),
                                  dat_master$Commune[match(dat_master$commGIS,forest_dat$codekhum)],
                                  khum_name))

forest_dat <- forest_dat %>% 
              mutate(khum_name = ifelse(khum_name == is.na(khum_name),
                                    replace(
                                    khum_name,
                                    is.na(khum_name),
                                    dat_master$Commune[match(forest_dat$codekhum,dat_master$commGIS)]),
                                  khum_name))

forest_dat <- forest_dat %>% 
              mutate(khum_name = replace(khum_name,
                                    is.na(khum_name),
                                    dat_master$Commune[match(forest_dat$codekhum,dat_master$commGIS)]))

forest_dat$khum_name <- ifelse(is.na(forest_dat$khum_name),
                               dat_master$Commune[match(forest_dat$codekhum,dat_master$commGIS)],
                               forest_dat$khum_name)

forest_dat %>% filter(is.na(khum_name))
dat_master %>% filter(commGIS==10102)

df1 <- data.frame(year = rep(c(2007,2008,2009,2010,2011), each=2),
                  comm_name = c("a","b",NA,"d","e",NA,"g","h",NA,"j"),
                  code_com = c(1:10))

df2 <- data.frame(comCode = c(1:10),
                  comm_name = letters[1:10])

df1$comm_name[is.na(df1$comm_name)] <- df2$comm_name[match(df1$code_com,df2$comCode)]

df1$comm_name <- ifelse(is.na(df1$comm_name),
                        df2$comm_name[match(df2$comCode,df1$code_com)],
                        df1$comm_name)
df1

df1$comm_name <- replace(df1$comm_name, is.na(df1$comm_name), 
                         df2$comm_name[match(df1$code_com,df2$comCode)])

df1 <- df1 %>% mutate(comm_name = ifelse(is.na(df1$comm_name),
                                        df2$comm_name[match(df1$code_com,df2$comCode)],
                                        comm_name))

df1 <- df1 %>% mutate(comm_name = ifelse(is.na(df1$comm_name),
                                        replace(comm_name, is.na(comm_name),
                                          df2$comm_name[match(df1$code_com,df2$comCode)]),
                                        comm_name))

df1$comm_name[is.na(df1$comm_name)] = df2$comm_name[match(df1$code_com, df2$comCode)]
df1

  ## Matching socioeconoimc and forest data sets ####


## I need to match the socioeconomic data (dat_master) to the forest cover data (forest_dat) so that I only have communes that have a code and name matching in each dataset, and that have non-zero forest cover. Although I need to check with Nils about what to do with afforestation

# I am expecting there to be more communes in the forest data than in any of the socioeconomic data as the forest data is probably the most up to date and "complete" set of communes. The data from the commune database is older, and had a lot of missing data.
str(dat_master)
str(forest_dat)

# I think I need to identify the "minimum set".  This is the year in dat_master that has the fewest communes able to be matched to the forest data.  Then I will need to match all other years to that minimum set. This will ensure I have no "missing" data between years when it comes to the modelling - i.e. all years will have exactly the same communes. 

# 2007
forest07 <- forest_dat[forest_dat$year=="2007", ]
missing.07 <- anti_join(forest07,dat.07.agg, by="commGIS")
str(missing.07) 
# 121 communes missing from socioeconomic data in 2007 compared with the forest data

# 2008
forest08 <- forest_dat[forest_dat$year=="2008",]
missing.08 <- anti_join(forest08, dat.08.agg, by="commGIS") 
str(missing.08)
# 110 communes missing

# 2009
forest09 <- forest_dat[forest_dat$year=="2009",]
missing.09 <- anti_join(forest09, dat.09.agg, by="commGIS") 
str(missing.09)
# 110 communes missing

# 2010
forest10 <- forest_dat[forest_dat$year=="2010",]
missing.10 <- anti_join(forest10, dat.10.agg, by="commGIS") 
str(missing.10)
# 110 communes missing

# 2011
forest11 <- forest_dat[forest_dat$year=="2011",]
missing.11 <- anti_join(forest11, dat.11.agg, by="commGIS") 
str(missing.11)
# 311 communes missing. This was expected.  2011 was missing a lot of data in the commune database

# 2012
forest12 <- forest_dat[forest_dat$year=="2012",]
missing.12 <- anti_join(forest12, dat.12.agg, by="commGIS") 
str(missing.12)
# 311 communes missing here too.

# check to see if the 310 missing communes in 2011 and 2012 are the same
compare11_12 <- anti_join(missing.11, missing.12, by="commGIS")
# They are the same missing communes

# SO I need to subset forest_dat to match 2011 (or 2012), and then match all the other years to that
merge11 <- inner_join(dat.11.agg,forest11, by="commGIS")
length(dat.11.agg$commGIS)
length(merge11$commGIS)
# There were also apparently some communes in the CD data that weren't in the forest data
str(merge11)

merge11 <- merge11 %>% dplyr::select(-year.y,-khum_name) %>% rename(year = year.x)

# need to subset 2012 socioecon data to 2011 data, then merge with 2012 forest data
sub12 <- semi_join(dat.12.agg, merge11, by="commGIS")

# maybe I need to check the number of rows left once I've matched all socioecon years to forest data in order to find the minimum set