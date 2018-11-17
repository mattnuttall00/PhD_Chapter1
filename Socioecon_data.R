#### SOCIOECONOMIC DATA------------------------------------------------------------------------------------------------------
#### Load libraries and data ####

library('cowplot')
library('tidyverse')

dat <- read.csv("Socioeconomic_variables.csv")
commDat <- read.csv("commGIS.csv")


#### Grouping and summarising indigenous group variables ####
# Subset indigenous group columns
dat.ig <- dat[,10:73]

# This new dataframe has 64 columns, representing 64/4 = 16 groups
# Groups start at columns 1,5,9,13 etc, so we first need to create a sequence of numbers representing this
grp.start <- seq(from=1,to=61,by=4)

# We now write a loop to sum age group columns for each indigenous group (represented by its start column)
dat.ig2 <- as.data.frame(matrix(NA,nrow(dat.ig),ncol=16)) 
for (g in 1:length(grp.start)){
  sub <- dat.ig[,grp.start[g]:(grp.start[g]+3)]
  ag.sum <- apply(sub,1,sum)
  dat.ig2[,g] <- ag.sum
}

# From this latest data frame, create a vector representing the proportion of indigenous people in total population
tot.pop <- dat$tot_pop
tot.ig <- rowSums(dat.ig2,na.rm=T)
prop.ig <- tot.ig/tot.pop

# Note that you would get the same by doing...
#prop.ig <- rowSums(dat[,10:73],na.rm=T)/dat$tot_pop

# Then add to initial dataset and remove unwanted columns
dat2 <- dat
dat2$Prop_Indigenous <- prop.ig
dat2 <- dat2[,-c(10:73)]


#### Adding unique Commune Codes ####
## add commGIS codes

dat2$CommCode <- NA   # Create an empty column in dat2 to store codes
lgth <- vector()

for (i in 1:nrow(dat2)){   # For each row in dat2
  cm <- as.character(dat2$Commune[i])    # What is the commune name?
  pv <- as.character(dat2$Province[i])   # What is the province name?
  cc <- subset(commDat,province==pv & commune==cm)  # Subset the line in commDat that matches these two values
  lgth[i] <- length(cc[,1])
  
    dat2$CommCode[i] <- cc[,1]  # Insert the associated commune code into dat2
  }


dat2 <- dat2[,-4]    # Remove column 4 as it should be empty (no villages)


#dt <- data.frame(PV=dat2[which(lgth>1),'Province'],CM=dat2[which(lgth>1),'Commune'])
#dt



#### Data cleaning and removal of errors ####

# tidyverse
as.tibble(dat2)

## KM_Comm errors ####

# There is one village in Prey Veng > Preaek Krabau with a KM_Comm = 600.  The other two villages in the Commune have a value of 2 (and the Commune is tiny), and so I am going to replace the 600 with the mean for the commune which is 2. 
dat2 <- dat2 %>% 
        mutate(KM_Comm = replace(KM_Comm, KM_Comm == 600, 2))

# There is also a Commune with a mean KM_Comm of 51.  It's Koh Kong > Dang Tong 
dat2 %>% 
  filter(Commune == "Dang Tong") %>% 
  filter(KM_Comm < 10) %>% 
  summarise(mean = mean(KM_Comm))
# This is being caused by one village with a KM_Comm of 200, when the other village values are tiny.  The mean for the commune (excluding the outlier) is 1.99643.  I will replace the 200km value with the mean for the commune
dat2 <- dat2 %>% 
        mutate(KM_Comm = replace(KM_Comm, KM_Comm == 200, 1.99643))

# Checking two communes in Battambang which have values way higher than the rest of the Province
dat2 %>% 
  filter(Province == "Battambang") %>% 
  filter(Commune == "Serei Maen Cheay") %>% 
  select(KM_Comm) 
  
dat2 %>% 
  filter(Province == "Battambang") %>% 
  filter(Commune == "Chakrei") %>% 
  select(KM_Comm) 
# Serei Maen Cheay is a small commune, and there is no way that there can be KM_Comm distances ranging between 0 and 56.  I cannot find Chakrei commune in the GIS layer, so cannot visually assess. But again, there is no way a commune can have a range of 1 - 60 in KM_Comm.  I think the best thing to do here is to use the provincial mean.  So I will take the mean KM_Comm for Battambang 
dat2 %>% 
  filter(Province=="Battambang") %>% 
  filter(!Commune %in% c("Serei Maen Cheay", "Chakrei")) %>%
  summarise(mean = mean(KM_Comm))
# which is 4.54, and replace the KM_Comm values with this mean for the above two communes

dat2 <- dat2 %>%
        mutate(KM_Comm = ifelse(Commune=="Serei Maen Cheay", 
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.54), 
                                KM_Comm))
dat2 <- dat2 %>%
        mutate(KM_Comm = ifelse(Commune=="Chakrei", 
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.54), 
                                KM_Comm))

# Another set of outliers are in Kampong Cham > Trapeang Pring.  I have found the commune and the 12 villages in GIS, and have double checked the KM_Comm values, which are incorrect.  The village of Trapeang Pring (in the commune Trapeang Pring) has a KM_Comm value of 0, indicating that this is the village which has the commune office.  I will use GIS to get the correct distances from the other villages to Trapeang Pring, and replace them in the data

dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040701,
                                replace(KM_Comm, KM_Comm==KM_Comm, 10),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040702,
                                replace(KM_Comm, KM_Comm==KM_Comm, 8.5),
                                KM_Comm))

dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040703,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.3),
                                KM_Comm))

dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040704,
                                replace(KM_Comm, KM_Comm==KM_Comm, 3.8),
                                KM_Comm))

dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040706,
                                replace(KM_Comm, KM_Comm==KM_Comm, 6.1),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040707,
                                replace(KM_Comm, KM_Comm==KM_Comm, 11.9),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040708,
                                replace(KM_Comm, KM_Comm==KM_Comm, 12.5),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040709,
                                replace(KM_Comm, KM_Comm==KM_Comm, 13.7),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040710,
                                replace(KM_Comm, KM_Comm==KM_Comm, 6.7),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040711,
                                replace(KM_Comm, KM_Comm==KM_Comm, 10.5),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==3040713,
                                replace(KM_Comm, KM_Comm==KM_Comm, 9.7),
                                KM_Comm))

# More outliers in Kampong Chhnang > Chhnok Tru and Krang Skear.  I have found Chhnok Tru in GIS, and the 25km value for Kampong Preah village is clearly off.  It should be 1.3.  I will change it
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==4010202,
                                replace(KM_Comm, KM_Comm==KM_Comm, 1.3),
                                KM_Comm))
# I have found Krang Skear in GIS.  The villages with incorrect KM_Comm values are Kdol, Krang Skear Tboung, Anh Chanh, Ou Lpouv, Domnak Ompil, Phnom Taous, Domnak Khlong. I will edit them in dat2
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 4080606,
                                replace(KM_Comm, KM_Comm==KM_Comm, 8.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 4080607,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 4080609,
                                replace(KM_Comm, KM_Comm==KM_Comm, 5.4),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 4080610,
                                replace(KM_Comm, KM_Comm==KM_Comm, 8),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 4080611,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.3),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 4080612,
                                replace(KM_Comm, KM_Comm==KM_Comm, 8.6),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 4080613 ,
                                replace(KM_Comm, KM_Comm==KM_Comm, 8.2),
                                KM_Comm))

## Kampong Speu
# I have found the commune Trapeang Chour in GIS, but my village GIS layer doesnt have the same number of villages as in the data.  But based on the size of the commune, there are defnitely villages that have erroneous values for KM_Comm. Because the village GIS layer is incomplete, I am not sure whether taking a mean of the existing villages is appropriate as I am not sure which values are incorrect.  I think I will use the provincial mean (3.17) at the commune level (excluding Trapeang Chour, Ta Sal, Traeng Trayueng).

# Find the mean
dat2 %>% 
  filter(Province=="Kampong Speu") %>% 
  filter(!Commune %in% c("Trapeang Chour", "Ta Sal", "Traeng Trayueng")) %>% 
  summarise(mean = mean(KM_Comm))

# RUN ALL dat2 EDITS FOLLOWED BY AGGREGATING TO COMMUNE LEVEL CODE BEFORE RUNNING dat_master CODE

# Replace KM_Comm values in Trapeang Chour with the provincial mean
dat_master <- dat_master %>% 
              mutate(KM_Comm = ifelse(CommCode==50403,
                                      replace(KM_Comm, KM_Comm==KM_Comm, 3.17),
                                      KM_Comm))
# Ta Sal
# Again the village GIS layer is missing loads of villages, and the KM_Comm values for the commune are definitely incorrect (lots of values are larger than the maximum distance possible in the commune). I will do the same as above
dat_master <- dat_master %>% 
              mutate(KM_Comm = ifelse(CommCode==50405,
                                      replace(KM_Comm, KM_Comm==KM_Comm, 3.17),
                                      KM_Comm))

# I have found Traeng Trayueng in GIS, and the commune is actually very large.  And the KM_Comm values are actually within the scale of the commune, and none of them are beyond the realms of possibility.  I think I will leave them as they are.  

## Kampong Thom

# Mean Ritth
# I have found the commune on GIS, and the village layer matches the data.  I can therefore correct the erroneous KM_Comm values
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 6060404,
                                replace(KM_Comm, KM_Comm==KM_Comm, 16.7),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 6060406,
                                replace(KM_Comm, KM_Comm==KM_Comm, 27.5),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 6060407,
                                replace(KM_Comm, KM_Comm==KM_Comm, 28.3),
                                KM_Comm))

# Peam Bang
# The GIS layer only has 1 village, so I am not sure which values are incorrect. I think I will replace the values with the provincial mean (excluding Peam Bang)

# Find the mean
dat2 %>% 
  filter(Province=="Kampong Thom") %>% 
  filter(!Commune == ("Peam Bang")) %>% 
  summarise(mean = mean(KM_Comm))
# Replace KM_Comm values in Peam Bang with the provincial mean
dat_master <- dat_master %>% 
              mutate(KM_Comm = ifelse(CommCode==60807,
                                      replace(KM_Comm, KM_Comm==KM_Comm, 3.3),
                                      KM_Comm))

## Koh Kong

# Andoung Tuek - the village GIS layer matches with data.  I will use the GIS layer to correct the values
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 9010101,
                                replace(KM_Comm, KM_Comm==KM_Comm, 6.5),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 9010102,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.7),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 9010104,
                                replace(KM_Comm, KM_Comm==KM_Comm, 2.4),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 9010105,
                                replace(KM_Comm, KM_Comm==KM_Comm, 6.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 9010106,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.5),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 9010107,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.6),
                                KM_Comm))

# Preaek Khsach - The GIS data match the data.  The only outlier is the village of Preaek Khsach - I will replace the value with a value calculated via GIS
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 9020301,
                                replace(KM_Comm, KM_Comm==KM_Comm, 10),
                                KM_Comm))

## Kracheh

# Kampong Damrei - GIS matches village data, so I will correct the values
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10010401,
                                replace(KM_Comm, KM_Comm==KM_Comm, 14),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10010402,
                                replace(KM_Comm, KM_Comm==KM_Comm, 13.1),
                                KM_Comm))

# Thma Andaeuk - One obvious outlier.  GIS layer matches village data so I will replace the outlier with the correct value
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10020802,
                                replace(KM_Comm, KM_Comm==KM_Comm, 0.5),
                                KM_Comm))

# Ou Krieng - GIS village layer does not match data. I can't replace the values myself, so I will replace all values with the provincial mean

# finding the provincial mean (excluding the two uncorrected communes)
dat2 %>% 
  filter(Province=="Kracheh") %>% 
  filter(!Commune == c("Ou Krieng","Roluos Mean Chey")) %>% 
  summarise(mean = mean(KM_Comm))

dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10040501,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.43),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10040502,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.43),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10040503,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.43),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10040504,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.43),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10040505,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.43),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10040506,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.43),
                                KM_Comm))

# Roluos Mean Chey - The village GIS layer almost matches the data.  All of the village values are correct except the one missing village, and the value is too large.  I will change that village value to the mean of the rest of the commune

# Finding the mean of the villages in Roluos Mean Chey excluding Srae Toung
dat2 %>% 
  filter(Province=="Kracheh") %>% 
  filter(Commune=="Roluos Mean Chey") %>% 
  filter(!VillGis == 10040604) %>% 
  summarise(mean = mean(KM_Comm))

# Replacing erroneous value
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 10040604,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.33),
                                KM_Comm))

## Mondul Kiri

# Srae Chhuk - Village GIS layer does not match data so I can't replace erroneous values with the correct ones.  I will replace all values with provincial mean

# Finding the mean of communes in Mondulkiri excluding the 5 outliers
dat2 %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(!Commune %in% c("Srae Chhuk", "Srae Khtum", "Srae Preah", "Roya", "Romonea")) %>% 
  select(KM_Comm) %>%  
  summarise(mean = mean(KM_Comm))

# replace values
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010301 ,
                                replace(KM_Comm, KM_Comm==KM_Comm, 3.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010302,
                                replace(KM_Comm, KM_Comm==KM_Comm, 3.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010303,
                                replace(KM_Comm, KM_Comm==KM_Comm, 3.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010304,
                                replace(KM_Comm, KM_Comm==KM_Comm, 3.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010305,
                                replace(KM_Comm, KM_Comm==KM_Comm, 3.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010306,
                                replace(KM_Comm, KM_Comm==KM_Comm, 3.2),
                                KM_Comm))

# Srae Khtum - The village GIS layer almost matches, but a couple of missing villages. I know this commune well, and the mean distance is more than the provincial mean (3.2).  I will replace the values with the mean calculated from the village GIS layer, as I know those are accurate. O Am is commune capital so the values are 8.7, 6.2, 7.6, 8.3, 15.2, 8.6, and 0. The mean is therfore 7.7
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010401,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.6),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010402,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.6),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010403,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.6),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010404,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.6),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010406,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.6),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010407,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.6),
                                KM_Comm))

# Srae Preah.  The GIS village layer is missing some villages.  There is one obvious incorrect value, for Pu Kong.  All of the others seem reasonable.  I will replace the Pu Kong value with the mean of the rest of the villages

# Find the mean
dat2 %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune=="Srae Preah") %>% 
  filter(VillGis != 11010503) %>% 
  summarise(mean = mean(KM_Comm))

# Replace value for Pu Kong
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 11010503,
                                replace(KM_Comm, KM_Comm==KM_Comm, 6.5),
                                KM_Comm))

# Roya - One very clear outlier (Kdaoy), and potentially one other (Roveak). The GIS village layer and the data do not match completely, but the Commune centre is Roya which is on one edge of the commune. The commune is very large (~80km) and so actually the values are quite plausible. Therefore I will not make any changes. 

## Ratanak Kiri

#  Pa Tang - Village GIS layer and data almost match.  The obvious outlier (village: Ul) is in both layers so I can update with the correct value. The other outlier is the village of Chang Rea. The value is incorrect because the commune is smaller than the value, and the village isn't in the GIS layer. I will replace it with the mean of the commune (using new Ul value) whihc is 2.6
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 16050501,
                                replace(KM_Comm, KM_Comm==KM_Comm, 1.3),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 16050504,
                                replace(KM_Comm, KM_Comm==KM_Comm, 2.6),
                                KM_Comm))

## Ratanak Kiri

# Slaeng Spean - The village GIS layer is missing a bunch of villages, and they are the villages that are causing the commune to be an outlier. They are clearly incorrect values as they are larger than the commune itself. The rest of the villages are correct.  I will replace the incorrect village values with the mean for the commune (excl. the bad values) which is 4.2
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 17120609,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 17120610,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 17120611,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==  17120612,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 17120613,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 17120614,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.2),
                                KM_Comm))

## Stung Treng

# Kbal Romeas - two possible outlier villages - Srae Sranok & Chrab.  Village GIS layer matches the data so I can correct the values
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19010201,
                                replace(KM_Comm, KM_Comm==KM_Comm, 7.7),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19010202,
                                replace(KM_Comm, KM_Comm==KM_Comm, 16.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19010204,
                                replace(KM_Comm, KM_Comm==KM_Comm, 30.5),
                                KM_Comm))

# Ta Lat - Village GIS layer matches the data so I can correct the values
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19010701,
                                replace(KM_Comm, KM_Comm==KM_Comm, 5.8),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19010703,
                                replace(KM_Comm, KM_Comm==KM_Comm, 14.2),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19010704,
                                replace(KM_Comm, KM_Comm==KM_Comm, 17.4),
                                KM_Comm))

# Santepheap - Village GIS layer matches the data so I can correct the values
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19030301,
                                replace(KM_Comm, KM_Comm==KM_Comm, 17.9),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19030302,
                                replace(KM_Comm, KM_Comm==KM_Comm, 17),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis== 19030305,
                                replace(KM_Comm, KM_Comm==KM_Comm, 4.5),
                                KM_Comm))
dat2 <- dat2 %>% 
        mutate(KM_Comm = ifelse(VillGis==  19030306,
                                replace(KM_Comm, KM_Comm==KM_Comm, 0.5),
                                KM_Comm))


## KM_Market errors ####

# Replace erroneous KM_Market values for Khpob ateav and Preaek Ambel communes.  See "KM_Market" section below for more details.  Mean KM_Market in Kandal Province (excl. incorrect values) = 5.137493
dat2 %>% 
  filter(Province == "Kandal") %>% 
  filter(KM_Market < 50) %>% 
  summarise(mean = mean(KM_Market))

dat2 <- dat2 %>% 
          filter(Province=="Kandal") %>% 
          mutate(KM_Market = ifelse(KM_Market > 50, 5.137493, KM_Market))

# Kandal > K'am Samnar
dat2 %>%
  filter(Province=="Kandal") %>% 
  filter(Commune=="K'am Samnar") %>% 
  select(KM_Market)

# find mean (5.04)
dat2 %>% 
  filter(Province == "Kandal") %>% 
  filter(!Commune == "K'am Samnar") %>% 
  summarise(mean = mean(KM_Market))

# replace values
dat2 <- dat2 %>%
        mutate(KM_Market = ifelse(Commune=="K'am Samnar", 
                                replace(KM_Market, KM_Market==KM_Market, 5.04), 
                                KM_Market))


# Below was just to test the above code
#kandal.test <- test %>% filter(Province=="Kandal")  
#hist(kandal.test$KM_Market)
#kandal.test %>% 
  #select(KM_Market) %>% 
  #filter(KM_Market > 50)


## KM_Heal_cent errors ####

# Large outlier is Pursat > Ta Lou
dat %>% 
  filter(Province=="Pursat") %>% 
  filter(Commune == "Ta Lou") %>% 
  select(VillGis,Village,KM_Heal_cent)

# There are 2 villages which are causing the problem, with silly values.  I will change the values to the mean of that commune (excl. the outliers) which is 5.8

dat2 <- dat2 %>% 
        mutate(KM_Heal_cent = replace(KM_Heal_cent, KM_Heal_cent == 3333333333, 5.8))
dat2 <- dat2 %>% 
        mutate(KM_Heal_cent = replace(KM_Heal_cent, KM_Heal_cent == 700, 5.8))

# Next outlier is Pursat > Phteah Prey
dat %>% 
  filter(Province=="Pursat") %>% 
  filter(Commune == "Phteah Prey") %>% 
  select(VillGis,Village,KM_Heal_cent)
# There are 4 ludicrous values. One of the other villages has a value of 0 KM, which suggests that village is where the health centre is. I can correct the erroneous values with the correct values using GIS

dat2 <- dat2 %>%
        mutate(KM_Heal_cent = ifelse(VillGis==15050401, 
                                replace(KM_Heal_cent, KM_Heal_cent==KM_Heal_cent, 3.7), 
                                KM_Heal_cent))
dat2 <- dat2 %>%
        mutate(KM_Heal_cent = ifelse(VillGis==15050402, 
                                replace(KM_Heal_cent, KM_Heal_cent==KM_Heal_cent, 3.6), 
                                KM_Heal_cent))
dat2 <- dat2 %>%
        mutate(KM_Heal_cent = ifelse(VillGis==15050403, 
                                replace(KM_Heal_cent, KM_Heal_cent==KM_Heal_cent, 3.6), 
                                KM_Heal_cent))
dat2 <- dat2 %>%
        mutate(KM_Heal_cent = ifelse(VillGis==15050409, 
                                replace(KM_Heal_cent, KM_Heal_cent==KM_Heal_cent, 3.1), 
                                KM_Heal_cent))

# Next outliers - Kampong Thom > Sralau
dat %>% 
  filter(Province=="Kampong Thom") %>% 
  filter(Commune == "Sralau") %>% 
  select(VillGis,Village,KM_Heal_cent)
# One erroneous value - the village of Serei Sameakki Khang Tboung (1900). I can't find the commune on the GIS layer, and so I will use the mean of the rest of the commune taken from the data

# Finding the mean (1.97)
dat2 %>% 
  filter(Province=="Kampong Thom") %>% 
  filter(Commune=="Sralau") %>% 
  filter(!VillGis == 6011501) %>%
  summarise(mean = mean(KM_Heal_cent))

# replace the value
dat2 <- dat2 %>%
        mutate(KM_Heal_cent = ifelse(VillGis==6011501, 
                                replace(KM_Heal_cent, KM_Heal_cent==KM_Heal_cent, 1.97), 
                                KM_Heal_cent))

# Stung Treng > Ta Lat
dat %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune == "Sekong") %>% 
  select(VillGis,Village,KM_Heal_cent)
# All of the villages have very high values.  The adjacent communes all also have large values, which suggests that the values are accurate. Except Sekong commune which is  directly north. It's values are lower, and the distances from Ta Lat villages to the village in Sekong which has a health centre are less than the recorded values.  Therefore I think the values are incorrect. Safest thing to do is to replace values with the mean for the province

# Have a look at the mean value for the province
dat2 %>% 
  filter(Province=="Stung Treng") %>% 
  filter(!Commune == "Ta Lat") %>%
  summarise(mean = mean(KM_Heal_cent))

# replace values
dat2 <- dat2 %>%
        mutate(KM_Heal_cent = ifelse(Commune=="Ta Lat", 
                                replace(KM_Heal_cent, KM_Heal_cent==KM_Heal_cent, 16.12), 
                                KM_Heal_cent))

# Mondul Kiri > Srae Chhuk
dat %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune == "Srae Chhuk") %>% 
  select(VillGis,Village,KM_Heal_cent)
# There are some very high values, and the neighbouring communes have small values, so I think they are incorrect.  The provincial mean excluding Srae Chhuk is 11.8, suggesting that Srae Chhuk is an outlier. I will change the values to the provincial mean

# Look at the Provincial mean
dat2 %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(!Commune == "Srae Chhuk") %>%
  summarise(mean = mean(KM_Heal_cent))

# replace values
dat2 <- dat2 %>%
        mutate(KM_Heal_cent = ifelse(Commune=="Srae Chhuk", 
                                replace(KM_Heal_cent, KM_Heal_cent==KM_Heal_cent, 11.82), 
                                KM_Heal_cent))

# Koh Kong >  Ta Tey Leu
dat %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune == "Ruessei Chrum") %>% 
  select(VillGis,Village,KM_Heal_cent)



## inf_mort errors ####

# Ratanak Kiri > Serei Mongkol
dat %>% 
  filter(Province=="Ratanak Kiri") %>% 
  filter(Commune == "Serei Mongkol") %>% 
  select(VillGis,Village,inf_mort)
# There is one village (Neang Dei) with a value 3.5 times higher than the other villages.

# Provincial mean
dat2 %>% 
  filter(Province=="Ratanak Kiri") %>% 
  filter(!VillGis == 16040103) %>%
  summarise(mean = mean(inf_mort))
# The value for Neang Dei is an order of magnitude higher than the rest of the Province.  Therefore I believe its an incorrect outlier.  I will change it to the mean of the commune (0.0049)

# Commune mean
dat2 %>% 
  filter(Province=="Ratanak Kiri") %>% 
  filter(Commune=="Serei Mongkol") %>% 
  filter(!VillGis == 16040103) %>%
  summarise(mean = mean(inf_mort))

# replace value
dat2 <- dat2 %>%
        mutate(inf_mort = ifelse(VillGis==16040103, 
                                replace(inf_mort, inf_mort==inf_mort, 0.0049), 
                                inf_mort))


## U5_mort errors ####
dat %>% 
  filter(Province=="Battambang") %>% 
  filter(Commune == "Ta Pon") %>% 
  select(VillGis,Village,U5_mort)
# There is one village (Svay Sa) which has a value nerly 150 times higher than the next village.  This can't be true, so I will replace that value with the mean for the commune

# Find commune mean (0.00033)
dat2 %>% 
  filter(Province=="Battambang") %>%
  filter(Commune=="Ta Pon") %>%
  filter(!VillGis==2080302) %>%
  summarise(mean = mean(U5_mort))

# replace value
dat2 <- dat2 %>%
        mutate(U5_mort = ifelse(VillGis==2080302, 
                                replace(U5_mort, U5_mort==U5_mort, 0.00033), 
                                U5_mort))

## Prop_Indigenous errors ####

# Three communes with proportions over 1

# Mondul Kiri > Bu Sra
dat2 %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune == "Bu Sra") %>% 
  select(VillGis,Prop_Indigenous)
# One village (VIllGIS = 11040403) has a prop of 3.25

dat %>% 
  filter(Province=="Mondul Kiri") %>%
  filter(Commune=="Bu Sra") %>%
  select()
  filter(!VillGis==2080302) %>%
  summarise(mean = mean(U5_mort))
# The problem is that the number of indigeous people is much higher that the total population. The dodgy number is Phnong females over 18.  I think the fairest way to deal with it is to change the value to the mean of the commune

# Finding the mean (0.89)  
dat2 %>% 
  filter(Province=="Mondul Kiri") %>%
  filter(Commune=="Bu Sra") %>%
  filter(!VillGis==11040403) %>%
  summarise(mean = mean(Prop_Indigenous))
  
# replacing value
dat2 <- dat2 %>%
        mutate(Prop_Indigenous = ifelse(VillGis==11040403, 
                                replace(Prop_Indigenous, Prop_Indigenous==Prop_Indigenous, 0.887), 
                                Prop_Indigenous))

# Preah Vihear > Prame
dat2 %>% 
  filter(Province=="Preah Vihear") %>% 
  filter(Commune == "Prame") %>% 
  select(VillGis,Prop_Indigenous)
# Village 13070102 has proportion of 1.192.  The issue will be the same as above. I will replace it with the mean of the commune

# Finding the mean (0.96)  
dat2 %>% 
  filter(Province=="Preah Vihear") %>%
  filter(Commune=="Prame") %>%
  filter(!VillGis==13070102) %>%
  summarise(mean = mean(Prop_Indigenous))

# replacing value
dat2 <- dat2 %>%
        mutate(Prop_Indigenous = ifelse(VillGis==13070102, 
                                replace(Prop_Indigenous, Prop_Indigenous==Prop_Indigenous, 0.96), 
                                Prop_Indigenous))

# Stung Treng > Santepheap
dat2 %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune == "Santepheap") %>% 
  select(VillGis,Prop_Indigenous)
# One village with prop over 1 - villGIS = 19030304. Will do same as above

# Finding the mean (0.99)  
dat2 %>% 
  filter(Province=="Stung Treng") %>%
  filter(Commune=="Santepheap") %>%
  filter(!VillGis==19030304) %>%
  summarise(mean = mean(Prop_Indigenous))

# replacing value
dat2 <- dat2 %>%
        mutate(Prop_Indigenous = ifelse(VillGis==19030304, 
                                replace(Prop_Indigenous, Prop_Indigenous==Prop_Indigenous, 0.99), 
                                Prop_Indigenous))

#
## dist_sch errors ####

## Kampong Thom > Peam Bang
dat2 %>% 
  filter(Province=="Kampong Thom") %>% 
  filter(Commune == "Peam Bang") %>% 
  select(VillGis,dist_sch)
# The values are all very high, and all of the surrounding communes have low values.  I will replace the values with the provincial mean

# Finding the mean (3.2)
dat2 %>% 
  filter(Province=="Kampong Thom") %>%
  filter(!Commune=="Peam Bang") %>%
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Peam Bang", 
                                replace(dist_sch, dist_sch==dist_sch, 3.2), 
                                dist_sch))

## Koh Kong > Ta Tey Leu
dat2 %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune == "Ta Tey Leu") %>% 
  select(VillGis,dist_sch)
# All high and identical values. Difficulty is that some of the surrounding communes also have some larger values. The commune is large, and so the distances are theoretically believeable. But the provincial mean is a much more realistically low (15.5). And the mean for the other large communes is also much lower. However, the three large communes which are all next to each other (Ta Tey Leu, Pralay, and  Ruessei Chrum) all have the high values.  This suggests a pattern, and one which I do not want to alter.  I will leave the values as they are for now

# find the mean
dat2 %>% 
  filter(Province=="Koh Kong") %>%
  filter(!Commune=="Ta Tey Leu") %>%
  summarise(mean = mean(dist_sch))

# Koh Kong > Pralay
dat2 %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune == "Pralay") %>% 
  select(VillGis,dist_sch)

#  Koh Kong > Ruessei Chrum
dat2 %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune == "Ruessei Chrum") %>% 
  select(VillGis,dist_sch)

# Mondul Kiri > Chong Phlah 
dat2 %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune == "Chong Phlah") %>% 
  select(VillGis,dist_sch)
# the values are high but not implausible. And some of the communes around it also have quite high values.  I will leave it for now

# Mondul Kiri > Srae Chhuk 
dat2 %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune == "Srae Chhuk") %>% 
  select(VillGis,dist_sch)
# Some very high values, one specifically (villGIS = 11010303). The commune is tiny so there is no way these values are correct. I am going to replace the values with the provincial mean

# find the provincial mean (17.28)
dat2 %>% 
  filter(Province=="Mondul Kiri") %>%
  filter(!Commune=="Srae Chhuk") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Srae Chhuk", 
                                replace(dist_sch, dist_sch==dist_sch, 17.28), 
                                dist_sch))

# Mondul Kiri > Nang Khi Loek 
dat2 %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune == "Nang Khi Loek") %>% 
  select(VillGis,dist_sch)
# The values are too large for a small commune, especially when the adjacent communes have small values.  I am going to replace the values with the new provincial mean

# find the provincial mean (15.53)
dat2 %>% 
  filter(Province=="Mondul Kiri") %>%
  filter(!Commune=="Nang Khi Loek") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Nang Khi Loek", 
                                replace(dist_sch, dist_sch==dist_sch, 15.53), 
                                dist_sch))

# Preah Vihear > Yeang 
dat2 %>% 
  filter(Province=="Preah Vihear") %>% 
  filter(Commune == "Yeang") %>% 
  select(VillGis,dist_sch)
# The values are way to high, and all of the adjacent communes have small values. I will replace the values with the provincial mean

# find the provincial mean (7.51)
dat2 %>% 
  filter(Province=="Preah Vihear") %>%
  filter(!Commune=="Yeang") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Yeang", 
                                replace(dist_sch, dist_sch==dist_sch, 7.51), 
                                dist_sch))

# Pursat > Ou Saom 
dat2 %>% 
  filter(Province=="Pursat") %>% 
  filter(Commune == "Ou Saom") %>% 
  select(VillGis,dist_sch)
# The commune GIS layer doesn't have this commune listed (although there are some communes missing names), so I can't assess the distances. They are significantly higher than the provincial mean. I will change the values to the provincial mean

# Provincial mean (10.42)
dat2 %>% 
  filter(Province=="Pursat") %>%
  filter(!Commune=="Ou Saom") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Ou Saom", 
                                replace(dist_sch, dist_sch==dist_sch, 10.42), 
                                dist_sch))

# Pursat > Thma Da 
dat2 %>% 
  filter(Province=="Pursat") %>% 
  filter(Commune == "Thma Da") %>% 
  select(VillGis,dist_sch)
# This commune also can't be found on the GIS layer, and the values are clearly incorrect. I will do the same as above

# Provincial mean (10.6)
dat2 %>% 
  filter(Province=="Pursat") %>%
  filter(!Commune=="Thma Da") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Thma Da", 
                                replace(dist_sch, dist_sch==dist_sch, 10.6), 
                                dist_sch))

# Ratanak Kiri > Nhang
dat2 %>% 
  filter(Province=="Ratanak Kiri") %>% 
  filter(Commune == "Nhang") %>% 
  select(VillGis,dist_sch)
# Large values, but the surrounding communes also have large values. These values are plausible considering the location - remote part of Ratankiri.  I will leave these values as they are for now

# Ratanak Kiri > Seda
dat2 %>% 
  filter(Province=="Ratanak Kiri") %>% 
  filter(Commune == "Seda") %>% 
  select(VillGis,dist_sch)
# Some very large values. The values in the surrounding communes are smaller, which makes me question the values. The provincial mean seems to reflect the values of the surrounding values, so I will change the values to the provincial mean

# find the provincial mean (22)
dat2 %>% 
  filter(Province=="Ratanak Kiri") %>%
  filter(!Commune=="Seda") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Seda", 
                                replace(dist_sch, dist_sch==dist_sch, 22), 
                                dist_sch))


# Ratanak Kiri > Saom Thum
dat2 %>% 
  filter(Province=="Ratanak Kiri") %>% 
  filter(Commune == "Saom Thum") %>% 
  select(VillGis,dist_sch)
# Clearly two of the values are incorrect. The surrounding communes have reasonable values, which further supports the idea that these values are incorrect. I will replace with the provincial mean

# find the provincial mean (21.6)
dat2 %>% 
  filter(Province=="Ratanak Kiri") %>%
  filter(!Commune=="Saom Thum") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Saom Thum", 
                                replace(dist_sch, dist_sch==dist_sch, 21.6), 
                                dist_sch))

# Stung Treng > Srae Kor
dat2 %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune == "Srae Kor") %>% 
  select(VillGis,dist_sch)
# Values are way too high, and all of the surrounding communes are much lower and plausible - closer to the provincial mean.  I will replace the values with the provincial mean

# find the provincial mean (17.5)
dat2 %>% 
  filter(Province=="Stung Treng") %>%
  filter(!Commune=="Srae Kor") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Srae Kor", 
                                replace(dist_sch, dist_sch==dist_sch, 17.5), 
                                dist_sch))

# Stung Treng > Anlong Phe
dat2 %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune == "Anlong Phe") %>% 
  select(VillGis,dist_sch)
# Values are way too high, and all of the surrounding communes are much lower and plausible - closer to the provincial mean.  I will replace the values with the provincial mean

# find the provincial mean (16)
dat2 %>% 
  filter(Province=="Stung Treng") %>%
  filter(!Commune=="Anlong Phe") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Anlong Phe", 
                                replace(dist_sch, dist_sch==dist_sch, 16), 
                                dist_sch))

# Stung Treng > Ou Svay
dat2 %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune == "Ou Svay") %>% 
  select(VillGis,dist_sch)
# These values don't make sense based on the size of the commune and the values of the surrounding communes. I will replace with the provincial mean 

# find the provincial mean (14.7)
dat2 %>% 
  filter(Province=="Stung Treng") %>%
  filter(!Commune=="Ou Svay") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Ou Svay", 
                                replace(dist_sch, dist_sch==dist_sch, 14.7), 
                                dist_sch))

# Stung Treng > Preah Rumkel
dat2 %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune == "Preah Rumkel") %>% 
  select(VillGis,dist_sch)
# # These values don't make sense based on the size of the commune and the values of the surrounding communes. I will replace with the provincial mean 

# find the provincial mean (11.5)
dat2 %>% 
  filter(Province=="Stung Treng") %>%
  filter(!Commune=="Preah Rumkel") %>%  
  summarise(mean = mean(dist_sch))

# Replace values
dat2 <- dat2 %>%
        mutate(dist_sch = ifelse(Commune=="Preah Rumkel", 
                                replace(dist_sch, dist_sch==dist_sch, 11.5), 
                                dist_sch))

#
#### Aggregating to the Commune level ####
# Variables that need to be summed up to Commune level
dat3 <- dat2 %>% 
   
  select(CommCode,tot_pop,family,male_18_60,fem_18_60,pop_over61,numPrimLivFarm,Fish_man,ntfp_fam,
         land_confl,Pax_migt_in,Pax_migt_out) %>%  
  group_by(CommCode) %>%
  summarise_all(funs(sum)) 

# Variables that need to be meaned up to the Commune level
dat4 <- dat2 %>% 
  select(CommCode,F6_24_sch,M6_24_sch,F18_60_ill,M18_60_ill,propPrimLivFarm,fam_prod,Cloth_craft
         ,Trader,serv_prov,T18_60_uncjob,Les1_R_Land,No_R_Land,Les1_F_Land,No_F_Land,cow_fam,
         pig_fam, garbage,KM_Market,KM_Comm,YR_Pp_well,wat_safe,wat_pipe,crim_case,
         KM_Heal_cent,inf_mort,U5_mort,Prop_Indigenous) %>% 
  group_by(CommCode) %>%
  summarise_all(funs(mean)) 

# Variables that need the median taken up to the Commune level
dat5 <- dat2 %>% 
  select(CommCode, dist_sch) %>% 
  group_by(CommCode) %>% 
  summarise_all(funs(median))

# Join all of the above                
dat6 <- left_join(dat3,dat4,by = "CommCode")
dat7 <- left_join(dat6, dat5, by = "CommCode")

# Aggregate the admin variables up to the Commune level
admindat <- dat2 %>% 
  select(CommCode,Province, Commune) %>% 
  group_by(CommCode) %>% 
  distinct(CommCode, .keep_all=TRUE)

# Join tables
dat_master <- left_join(admindat, dat7, by = "CommCode")
str(dat_master)

# Remove T18_60_uncjob (because very unreliable data. Lots of raw values are greater than the total population)

dat_master <- dat_master %>% 
  select(-T18_60_uncjob)

# Remove YR_Pp_well (see explanation below under data exploration section)
dat_master <- dat_master %>% 
  select(-YR_Pp_well)

# Change CommCode to a factor
dat_master$CommCode <- as.factor(dat_master$CommCode)

# Quality control against raw data
dat_master %>% 
  filter(CommCode=="20401") %>% 
  select(F18_60_ill)


##----------------------------------------------------------------------------------------------------------------------
#### Data exploration ####

## Subset data by Province ####
battambang <- dat_master %>% 
  filter(., Province == "Battambang") 
banteay_meanchey <- dat_master %>% 
  filter(., Province == "Banteay Meanchey")
kampong_speu <- dat_master %>% 
  filter(., Province == "Kampong Speu")
kep <- dat_master %>% 
  filter(., Province == "Kep")
otdar_meanchey <- dat_master %>% 
  filter(., Province == "Otdar Meanchey")
preah_vihear <- dat_master %>% 
  filter(., Province == "Preah Vihear")
siem_reap <- dat_master %>% 
  filter(., Province == "Siem Reap")
kampong_thom <- dat_master %>% 
  filter(., Province == "Kampong Thom")
koh_kong <- dat_master %>% 
  filter(., Province == "Koh Kong")
pailin <- dat_master %>% 
  filter(., Province == "Pailin")
prey_veng <- dat_master %>% 
  filter(., Province == "Prey Veng")
stung_treng <- dat_master %>% 
  filter(., Province == "Stung Treng")
kampong_cham <- dat_master %>% 
  filter(., Province == "Kampong Cham")
kampot <- dat_master %>% 
  filter(., Province == "Kampot")
kratie <- dat_master %>% 
  filter(., Province == "Kracheh")
phnom_penh <- dat_master %>% 
  filter(., Province == "Phnom Penh")
pursat <- dat_master %>% 
  filter(., Province == "Pursat")
svay_rieng <- dat_master %>% 
  filter(., Province == "Svay Rieng")
kampong_chhnang <- dat_master %>% 
  filter(., Province == "Kampong Chhnang")
kandal <- dat_master %>% 
  filter(., Province == "Kandal")
mondulkiri <- dat_master %>% 
  filter(., Province == "Mondul Kiri")
preah_sihanouk <- dat_master %>% 
  filter(., Province == "Preah Sihanouk")
ratanakiri <- dat_master %>% 
  filter(., Province == "Ratanak Kiri")
takeo <- dat_master %>% 
  filter(., Province == "Takeo") 

## Total population ####
qplot(dat_master$tot_pop, geom = "histogram", xlab = "total pop")

# Four outliers which suggest populations over 50,000 people
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., tot_pop > 50000)
# these make sense

levels(dat_master$Province)

## histograms for total population by province
ggplot(dat_master, aes(tot_pop))+
  geom_histogram()+
  facet_wrap(~Province)


## Family ####
qplot(dat_master$family, geom = "histogram")

# one outlier which suggests somehwere with more than 15,000 families
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., family > 15000)
# Paoy Pet again

# Histograms for number of famililes by province
ggplot(dat_master, aes(family))+
  geom_histogram()+
  facet_wrap(~Province)

## male_18_60 ####
qplot(dat_master$male_18_60, geom = "histogram")

# Several large outliers
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., male_18_60 > 30000)
# Paoy Pet and Phnom Penh, which makes sense

# histograms for number of males ages 18-60 by Province
ggplot(dat_master, aes(male_18_60))+
  geom_histogram()+
  facet_wrap(~Province)

## fem_18_60 ####
qplot(dat_master$fem_18_60, geom = "histogram")

# Looks like there are several outliers again
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., fem_18_60 > 30000)
# Phnom Penh and Paoy Pet again

# histograms for number of females ages 18-60 by Province
ggplot(dat_master, aes(fem_18_60))+
  geom_histogram()+
  facet_wrap(~Province)


## pop_over61 ####
qplot(dat_master$pop_over61, geom = "histogram")

# Several outliers again
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., pop_over61 > 3000)
# All Phnom Penh

# histograms for number of people over 61 by Province
ggplot(dat_master, aes(pop_over61))+
  geom_histogram()+
  facet_wrap(~Province)


## numPrimLivFarm ####
qplot(dat_master$numPrimLivFarm, geom = "histogram")

# There are a few outliers here, and I'm curious as to where in the country they are. I'm guessing the central lowlands, where rice farming is huge
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., numPrimLivFarm > 4000)
# Note this is number of families NOT proportion of families

# histograms for number of families whose primary livelihood is farming, by Province
ggplot(dat_master, aes(numPrimLivFarm))+
  geom_histogram(binwidth = 300)+
  facet_wrap(~Province)

# Big spike in Phnom Penh
ggplot(phnom_penh, aes(numPrimLivFarm))+
  geom_histogram(binwidth = 100)
# This makes sense, as there will be the fewest farmers in the largest city

## propPrimLivFarm ####
qplot(dat_master$propPrimLivFarm, geom = "histogram")

# This paints a very different picture to numPrimLivFarm above.  This suggests that most communes are made up of mostly farmers.  This fits with Cambodia, and the culture of farming (even when you have other jobs)

ggplot(dat_master, aes(propPrimLivFarm))+
  geom_histogram()+
  facet_wrap(~Province)

## Fish_man ####
qplot(dat_master$Fish_man, geom = "histogram")

# Huge spike around 0 and quite a few large outliers.  This makes sense as the number of places where you can make a living as a fisherman are few
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., Fish_man > 1000)

# histograms for number of families whose primary livelihood is fishing, by Province
ggplot(dat_master, aes(Fish_man))+
  geom_histogram()+
  facet_wrap(~Province)

## ntfp_fam ####
qplot(dat_master$ntfp_fam, geom = "histogram")

# Huge spike around 0 and quite a few large outliers.  This makes sense as the number of places where you can make a living collecting NTFPs are few
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., ntfp_fam > 50)
# I'm surprised Mondulkiri and Ratanakiri and Stung Treng are not listed 

# histograms for number of families whose primary livelihood is NTFP collection, by Province
ggplot(dat_master, aes(ntfp_fam))+
  geom_histogram()+
  facet_wrap(~Province)

# Need to check Otdar Meanchey, Preah Sihanouk, as they have outliers
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Province == "Otdar Meanchey") %>% 
  filter(., ntfp_fam > 100) %>% 
  print.default()
# One of the communes in Otdar Meanchey with high value for ntfp_fam (Koun Kriel) kind of makes sense as it is the largest commune in the province, and there are still plently of forested areas.  The other commune (Kouk Mon) however doesnt really have any forested areas, so I'm not sure how you can have much NTFP activity.  Although in this census NTFP is probably a pretty loose term.  Could also plausibly be lots of bamboo, rattan etc.  The value is so large as to cause me much concern.

dat_master %>% 
  group_by(CommCode) %>% 
  filter(Province == "Preah Sihanouk") %>% 
  filter(., ntfp_fam > 100) %>% 
  print.default()
# The one commune with a value of more than 100 is in a semi forested area and the value is only 103, so I'm not that concerned.

## land_confl ####
qplot(dat_master$land_confl, geom = "histogram")

# couple of outliers
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., land_confl > 150)
# Interestingly Paoy Pet is one of the outliers.  This is not that surprising as it's one of the largest settlements.  I wonder if the proximity to the border influences this metric, i.e. are there any land conflict with Thailand?

ggplot(dat_master, aes(land_confl))+
  geom_histogram()+
  facet_wrap(~Province)

# THere are a few provinces with a handful of outliers, but I am inclined to believe these, as the numbers are not beyond what I would expect.  Provinces are: Banteay Meanchey, Battambang, Kampot, Kracheh, Pailin, Pursat, and Siem Reap.  

## Pax_migt_in ####
qplot(dat_master$Pax_migt_in, geom = "histogram")

# Quite a few outliers. 
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., Pax_migt_in > 1500) %>% 
  print(width=Inf)
# Banteay Meanchey (probably people looking for work in Paoy Pet), Kampong Cham (agricultural labour? garment factories?), Phnom Penh, Otdar Meanchey (not sure why this would be), and Pailin (also not sure, but it's on the border with Thailand - so maybe cross border workers)

ggplot(dat_master, aes(Pax_migt_in))+
  geom_histogram()+
  facet_wrap(~Province)

## Pax_migt_out ####
qplot(dat_master$Pax_migt_out, geom = "histogram")

# One massive outlier
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., Pax_migt_out > 1000) %>% 
  print(width=Inf)
# OUtlier is Tonle Basak commune in PP.  Not sure about this.  This is the area where most of the foreigners live...maybe Cambodians sold houses and moved out to make money? 

ggplot(dat_master, aes(Pax_migt_out))+
  geom_histogram()+
  facet_wrap(~Province)

## F6_24_sch ####
qplot(dat_master$F6_24_sch, geom = "histogram")

# There is a stange value of 0
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., F6_24_sch < 0.05) %>% 
  print(width=Inf)
# Apparently Kaoh Peak commune in Ratankiri, 0% of their 6-24 year olds (male and female) are in school.  THis sounds like a Null response in the date.  THe total population of the commune is nearly 3000, with over 600 families.  That means there MUST be chldren between6 and 24 years old, and some of them must be in school.  Although....the nearest school is 21km away.  ANd the whole commune is indigenous. More than half of adult women are illiterate and nearly half of men are. All families rely on farming as their primary livelihood.  They are 53km from the nearest market. PErhaps it's true?

ggplot(dat_master, aes(F6_24_sch))+
  geom_histogram()+
  facet_wrap(~Province)

# Low value for one commune in MDK
dat_master %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(F6_24_sch < 0.25) %>% 
  print(width=Inf)

# double checking there is no value greater than 1
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., F6_24_sch > 1)

## M6_24_sch ####
qplot(dat_master$M6_24_sch, geom = "histogram",main = 1, xlab = "", element_text(size=10))

# That 0 value will be the commune in Ratankiri as above
ggplot(dat_master, aes(M6_24_sch))+
  geom_histogram()+
  facet_wrap(~Province)

# double checking there is no value greater than 1
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., M6_24_sch > 1)

## F18_60_ill ####
qplot(dat_master$F18_60_ill, geom = "histogram")

# the shape is as you would expect. I'm curious as to where the highest levels of illiteracy are
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., F18_60_ill > 0.75) %>% 
  print(width=Inf)

# Poor old Ratanikiri!

ggplot(dat_master, aes(F18_60_ill))+
  geom_histogram()+
  facet_wrap(~Province)

## M18_60_ill ####
qplot(dat_master$M18_60_ill, geom = "histogram")

# very similar shape and simnilar communes
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., M18_60_ill > 0.75) %>% 
  print(width=Inf)

ggplot(dat_master, aes(M18_60_ill))+
  geom_histogram()+
  facet_wrap(~Province)

## fam_prod ####
qplot(dat_master$fam_prod, geom = "histogram")

dat_master %>% 
  group_by(CommCode) %>% 
  filter(fam_prod > 0.05) %>% 
  print(width=Inf)

ggplot(dat_master, aes(fam_prod))+
  geom_histogram()+
  facet_wrap(~Province)

## Cloth_craft ####
qplot(dat_master$Cloth_craft, geom = "histogram")

# one big outlier, I wonder if this is where all the textile factories are
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Cloth_craft > 0.8) %>% 
  print(width=Inf)
# Kandal - I bet that is because of the textile factories

ggplot(dat_master, aes(Cloth_craft))+
  geom_histogram()+
  facet_wrap(~Province)

## Trader ####
qplot(dat_master$Trader, geom = "histogram")

# outlier
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Trader > 0.5) %>% 
  print(width=Inf)
# All in PP which makes sense

ggplot(dat_master, aes(Trader))+
  geom_histogram()+
  facet_wrap(~Province)

## serv_prov ####
qplot(dat_master$Trader, geom = "histogram")

# outlier
dat_master %>% 
  group_by(CommCode) %>% 
  filter(serv_prov > 0.5) %>% 
  print(width=Inf)
# one in Battambang, Phnom Penh, and loads in Sihanoukville.  Urban centres so makes sense

ggplot(dat_master, aes(serv_prov))+
  geom_histogram()+
  facet_wrap(~Province)

## Les1_R_Land ####
qplot(dat_master$Les1_R_Land, geom = "histogram")

ggplot(dat_master, aes(Les1_R_Land))+
  geom_histogram()+
  facet_wrap(~Province)

# Phnom Penh has a big spike around 0. There are 10 communes in PP where no one has less than 1ha of rice land.  Slightly surprising but not impossible
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Province == "Phnom Penh") %>% 
  filter(Les1_R_Land == 0) %>% 
  print(width=Inf)

## No_R_Land ####
qplot(dat_master$No_R_Land, geom = "histogram")
 
# One outlier
dat_master %>% 
  group_by(CommCode) %>% 
  filter(No_R_Land > 0.6) %>% 
  print(width=Inf)
# Kiri Toen commune in Ratanakiri province - 64% of people have no rice land.  Unusual, especially for a rural province.  Perhaps that is the commune that has the capital town (Ban Lang)?  Urban centre?

ggplot(dat_master, aes(No_R_Land))+
  geom_histogram()+
  facet_wrap(~Province)

## Les1_F_Land ####
qplot(dat_master$Les1_F_Land, geom = "histogram")

# few outliers
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Les1_F_Land > 0.7)
# Kandal province

ggplot(dat_master, aes(Les1_F_Land))+
  geom_histogram()+
  facet_wrap(~Province)

## No_F_Land ####
qplot(dat_master$No_F_Land, geom = "histogram")

# All pretty low, which I expected

ggplot(dat_master, aes(No_F_Land))+
  geom_histogram()+
  facet_wrap(~Province)

## cow_fam ####
qplot(dat_master$cow_fam, geom = "histogram")

# Spike at 0
dat_master %>% 
  group_by(CommCode) %>% 
  filter(cow_fam == 0) %>% 
  print(width=Inf)
# Not that unexpected as all the urban centres and towns will have low numbers

# Double checking there are no values >1
dat_master %>% 
  group_by(CommCode) %>% 
  filter(cow_fam > 1) %>% 
  print(width=Inf)

ggplot(dat_master, aes(cow_fam))+
  geom_histogram()+
  facet_wrap(~Province)

## pig_fam ####
qplot(dat_master$pig_fam, geom = "histogram")

ggplot(dat_master, aes(pig_fam))+
  geom_histogram()+
  facet_wrap(~Province)

## garbage ####
qplot(dat_master$garbage, geom = "histogram")

# vast majority of people do not have access to waste collection. Not surprising. Bit surprised that there is a value of 1
dat_master %>% 
  group_by(CommCode) %>% 
  filter(garbage == 1)
# Mostly Phnom Penh and other urban centres.  I feel like a value of 1 is slightly over confident but perhaps it's possible

ggplot(dat_master, aes(garbage))+
  geom_histogram()+
  facet_wrap(~Province)

## KM_Market ####
qplot(dat_master$KM_Market, geom = "histogram")

# Some big distances which I find hard to believe
dat_master %>% 
  group_by(CommCode) %>% 
  filter(KM_Market > 50) %>% 
   print(width=Inf)

# There must be data entry errors.  For example, the commune of Khpob Ateav in Kandal province has a KM_Market value of 279 KM.  It is next door to a commune called Sandar, which has a KM_Market value of 6.67 KM.  And the distance from the centre of Khpob Ateav to the centre of Sandar is ~6km, so the maximum the value for Khpob Ateav should be is ~12km.  
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Commune == "Sandar") %>% 
   print(width=Inf)

# Pralay commune in Koh Kong province has a KM_Market value of 119km.  It's neighbouring communes and their KM_Market values are Ta Tey Leu = 90, Ruessei Chrum = 73.5, Chumnoab = 113.  Neighbouring commune Rokat which is in Pursat Province but next door to Ta Tey Leu has value of 2.5 
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Commune %in% c("Ta Tey Leu", "Ruessei Chrum", "Chumnoab", "Rokat")) %>% 
   print(width=Inf)

# Need to look at original data
qplot(dat$KM_Market, type = "histogram")

# There are two giant value which needs to be removed
dat %>% 
  filter(KM_Market > 700)
# Both in Kandal Province: Khpob ateav and Preaek Ambel. I've had a look at the rest of the values for Kandal Province, and it is really just those two that are clearly erroneous.  For the moment I am going to replace the values for Khpob ateav & Preaek Ambel with the mean of the rest of the communes (excl. Khpob ateav & Preaek Ambel).  I will do it at the top of the script

dat %>% 
  filter(Province == "Kandal") %>% 
  select(KM_Market) %>% 
  filter(KM_Market > 30)

# After removing the two massive outliers...

# Still one outlier (although a more belivable one) of KM_Market = 40
dat_master %>% 
  group_by(CommCode) %>% 
  filter(KM_Market == 40) %>% 
  print(width=Inf)

# It's the commune of K'am Samnar in Kandal Province.  It's neighbouring commune is Sandar
dat_master %>% 
  group_by(CommCode) %>% 
  filter(Province == "Kandal") %>% 
  filter(Commune == "Sandar") %>% 
  print(width=Inf)
# Which has a KM_Market = 6.67.  Using GIS I've checked, and there is no concievable way that Sandar can be 6.67km away from a market and K'am Samnar is ~40. I think I am going to replace this value with the mean as well.

ggplot(dat_master, aes(KM_Market))+
  geom_histogram()+
  facet_wrap(~Province)

## KM_Comm ####
qplot(dat_master$KM_Comm, geom = "histogram")

# One outlier (>200)
dat_master %>% 
  filter(KM_Comm > 200) %>% 
  print(width=Inf)

# 600km outlier removed, now need to check one value >50km
dat_master %>% 
  filter(KM_Comm > 50) %>% 
  print(width=Inf)
# Village with 200km value remived and replaced with mean for the commune (excl. outlier)

ggplot(dat_master, aes(KM_Comm))+
  geom_histogram()+
  facet_wrap(~Province)

### Need to check some more outliers

## Battambang 
dat_master %>% 
  filter((Province=="Battambang")) %>% 
  filter(KM_Comm > 20) %>% 
  print(width=Inf)
# There are two communes with much higher values than the rest of the Province. These have now been dealt with in the data cleaning section at the top

## Kampong Cham
dat_master %>% 
  filter((Province=="Kampong Cham")) %>% 
  filter(KM_Comm > 10) %>% 
  print(width=Inf)
# Trapean Pring is the commune that looks like an outlier.
dat2 %>% 
  filter(Province=="Kampong Cham") %>% 
  filter(Commune=="Trapeang Pring") %>% 
  select(c(VillGis,KM_Comm)) 
# Dealt with in data cleaning section

## Kampong Chhnang
dat_master %>% 
  filter((Province=="Kampong Chhnang")) %>% 
  filter(KM_Comm > 10) %>% 
  print(width=Inf)
# Two outliers

# Chhnok Tru
dat %>% 
  filter(Province=="Kampong Chhnang") %>% 
  filter(Commune=="Chhnok Tru") %>% 
  select(c(VillGis,Village,KM_Comm)) 
# Dealt with in data cleaning section

# Krang Skear
dat %>% 
  filter(Province=="Kampong Chhnang") %>% 
  filter(Commune=="Krang Skear") %>% 
  select(c(VillGis,Village,KM_Comm)) 

## Kampong Speu
dat_master %>% 
  filter((Province=="Kampong Speu")) %>% 
  filter(KM_Comm > 10) %>% 
  print(width=Inf)
# Three communes are outliers - Trapeang Chour, Ta Sal, Traeng Trayueng

# Trapeang Chour
dat %>% 
  filter(Province=="Kampong Speu") %>% 
  filter(Commune=="Trapeang Chour") %>% 
  select(c(VillGis,Village,KM_Comm)) 

# finding the commune code for Trapeang Chour
dat_master %>% 
  filter(Province=="Kampong Speu") %>% 
  filter(Commune=="Trapeang Chour") 

# Ta Sal
dat %>% 
  filter(Province=="Kampong Speu") %>% 
  filter(Commune=="Trapeang Chour") %>% 
  select(c(VillGis,Village,KM_Comm)) 

# finding the commune code for Ta Sal
dat_master %>% 
  filter(Province=="Kampong Speu") %>% 
  filter(Commune=="Ta Sal") 

# Traeng Trayueng
dat %>% 
  filter(Province=="Kampong Speu") %>% 
  filter(Commune=="Traeng Trayueng") %>% 
  select(c(VillGis,Village,KM_Comm)) 

## Kampong Thom
dat_master %>% 
  filter((Province=="Kampong Thom")) %>% 
  filter(KM_Comm > 10) %>% 
  print(width=Inf)
# The outlier commune is Mean Ritth

# Mean Ritth
dat %>% 
  filter(Province=="Kampong Thom") %>% 
  filter(Commune=="Mean Ritth") %>% 
  select(c(VillGis,Village,KM_Comm)) 

# Peam Bang
dat %>% 
  filter(Province=="Kampong Thom") %>% 
  filter(Commune=="Peam Bang") %>% 
  select(c(VillGis,Village,KM_Comm))

## Koh Kong
dat_master %>% 
  filter((Province=="Koh Kong")) %>% 
  filter(KM_Comm > 10) %>% 
  print(width=Inf)
# Two communes are potential outliers - Andoung Tuek and Preaek Khsach

# Andoung Tuek
dat %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune=="Andoung Tuek") %>% 
  select(c(VillGis,Village,KM_Comm))

# Preaek Khsach
dat %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune=="Preaek Khsach") %>% 
  select(c(VillGis,Village,KM_Comm))

## Kracheh
dat_master %>% 
  filter((Province=="Kracheh")) %>% 
  filter(KM_Comm > 15) %>% 
  print(width=Inf)
# Potentially 4 outliers - Kampong Damrei, Thma Andaeuk, Ou Krieng, Roluos Mean Chey

# Kampong Damrei
dat %>% 
  filter(Province=="Kracheh") %>% 
  filter(Commune=="Kampong Damrei") %>% 
  select(c(VillGis,Village,KM_Comm))

# Thma Andaeuk
dat %>% 
  filter(Province=="Kracheh") %>% 
  filter(Commune=="Thma Andaeuk") %>% 
  select(c(VillGis,Village,KM_Comm))

# Ou Krieng
dat %>% 
  filter(Province=="Kracheh") %>% 
  filter(Commune=="Ou Krieng") %>% 
  select(c(VillGis,Village,KM_Comm))

# Roluos Mean Chey
dat %>% 
  filter(Province=="Kracheh") %>% 
  filter(Commune=="Roluos Mean Chey") %>% 
  select(c(VillGis,Village,KM_Comm))

## Mondulkiri
dat_master %>% 
  filter((Province=="Mondul Kiri")) %>% 
  filter(!Commune %in% c("Srae Chhuk", "Srae Khtum", "Srae Preah", "Roya", "Romonea")) %>% 
  print(width=Inf)
# 5 potential outliers - Srae Chhuk, Srae Khtum, Srae Preah, Roya, Romonea

# Srae Chhuk
dat %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune=="Srae Chhuk") %>% 
  select(c(VillGis,Village,KM_Comm))

# Srae Khtum
dat %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune=="Srae Khtum") %>% 
  select(c(VillGis,Village,KM_Comm))

# Srae Preah
dat %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune=="Srae Preah") %>% 
  select(c(VillGis,Village,KM_Comm))

# Roya
dat %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune=="Roya") %>% 
  select(c(VillGis,Village,KM_Comm))

# Romonea
dat %>% 
  filter(Province=="Mondul Kiri") %>% 
  filter(Commune=="Romonea") %>% 
  select(c(VillGis,Village,KM_Comm))
# I'm not sure these are actually outliers - the values are plausible.  I will not change anything.

## Ratanak Kiri
dat_master %>% 
  filter((Province=="Ratanak Kiri")) %>% 
  filter(KM_Comm > 15) %>% 
  print(width=Inf)

# One potential outlier - Pa Tang.
dat %>% 
  filter(Province=="Ratanak Kiri") %>% 
  filter(Commune=="Pa Tang") %>% 
  select(c(VillGis,Village,KM_Comm))

## Siem Reap
dat_master %>% 
  filter((Province=="Siem Reap")) %>% 
  filter(KM_Comm > 10) %>% 
  print(width=Inf)

# One potential outlier - Slaeng Spean
dat %>% 
  filter(Province=="Siem Reap") %>% 
  filter(Commune=="Slaeng Spean") %>% 
  select(c(VillGis,Village,KM_Comm))

ggplot(dat_master, aes(KM_Comm))+
  geom_histogram()+
  facet_wrap(~Province)

## Stung Treng
dat_master %>% 
  filter((Province=="Stung Treng")) %>% 
  filter(KM_Comm > 15) %>% 
  print(width=Inf)
# Three potential outliers - Kbal Romeas, Ta Lat, Santepheap

# Kbal Romeas
dat %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune=="Kbal Romeas") %>% 
  select(c(VillGis,Village,KM_Comm))

# Ta Lat
dat %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune=="Ta Lat") %>% 
  select(c(VillGis,Village,KM_Comm))

# Santepheap
dat %>% 
  filter(Province=="Stung Treng") %>% 
  filter(Commune=="Santepheap") %>% 
  select(c(VillGis,Village,KM_Comm))

## Yr_Pp_well ####
qplot(dat_master$YR_Pp_well, geom = "histogram")

# Check out the 0's and very high values
dat_master %>% 
  filter(YR_Pp_well > 200) %>% 
  select(Province) %>%
  data.frame() %>% 
  distinct(., keep_all=FALSE)

dat_master %>% 
  filter(Province=="Prey Veng") %>% 
  select(c(Commune,YR_Pp_well)) %>% 
  print(n=Inf)

dat %>% 
  filter(Province=="Prey Veng") %>% 
  select(c(Village,Commune,YR_Pp_well)) 

dat %>% 
  filter(Province=="Prey Veng") %>%
  filter(Commune=="Boeng Preah") %>% 
  filter(Village=="Ta Mau") 
  
# I've decided to exclude this variable. The numbers looked implausible, but then I realised the numbers were so high probably becuase of private wells and water pumps, which are common. In which case I'm not sure this is an appropriate variable to represent access to services.  The other water variables (proportion of families with access to clean water & proportion of families with access to piped water) are better.  I will remove it at the top of the script under "aggregating to the commune level"
## wat_safe ####
qplot(dat_master$wat_safe, geom = "histogram")

# Spikes at 0 and 1
dat_master %>% 
  filter(wat_safe == 1) %>% 
  select(Province) %>% 
  print(n=Inf)
# The 1 values are mostly believable. 

dat_master %>% 
  filter(wat_safe == 0) %>% 
  select(Province) %>% 
  print(n=Inf)

dat %>% 
  filter(Province=="Siem Reap") %>% 
  filter(wat_safe == 1) %>% 
  select(VillGis,Village,Commune,wat_safe)

# I can't think of any reason why the 0's and 1's would not be true or belieable.  There are 113 communes (~7%) where all people have access to safe water, whereas there are 143 (~9%) communes where no one has access to safe water.

## wat_pipe ####
qplot(dat_master$wat_pipe, geom = "histogram")

# Histogram looks reasonable

## crim_case ####
qplot(dat_master$crim_case, geom = "histogram")

# Some outliers
dat_master %>% 
  filter(crim_case > 0.010) %>% 
  print(width=Inf)
# Koh Kong>Chi Phat, Pursat>Krapeu Pir, Preah Sihanouk>Kaoh Rung

dat %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune == "Chi Phat") %>% 
  select(VillGis,Village,crim_case)
# There is one village (Sam Lort) that has a crim_case value 10 time greater than the other villages.

dat %>% 
  filter(Province=="Pursat") %>% 
  filter(Commune == "Krapeu Pir") %>% 
  select(VillGis,Village,crim_case)
# There is one village (Krapeu Pir Kraom) that has a value 6 times greater than the other villages

dat %>% 
  filter(Province=="Preah Sihanouk") %>% 
  filter(Commune == "Kaoh Rung") %>% 
  select(VillGis,Village,crim_case)

# I am not sure whether to leave the outliers in, or remove them.  I have no prior knowledge about this sort of thing and so don't know whether these are feasible or not.

## KM_Heal_cent ####
qplot(dat_master$KM_Heal_cent, geom = "histogram")

# Major outlier - Pursat > Ta Lou
dat_master %>% 
  filter(KM_Heal_cent > 1.5e+08) %>% 
  print(width=Inf)

# Next outlier - Pursat > Phteah Prey
dat_master %>% 
  filter(KM_Heal_cent > 300) %>% 
  print(width=Inf)

# Next outliers - Kampong Thom > Sralau, Stung Treng > Ta Lat
dat_master %>% 
  filter(KM_Heal_cent > 100) %>% 
  print(width=Inf)

# Check if any of the other large commune values are erroneous
dat_master %>% 
  filter(KM_Heal_cent > 75) %>% 
  print(width=Inf)

## inf_mort ####
qplot(dat_master$inf_mort, geom = "histogram")

# One major outlier - Ratanak Kiri > Serei Mongkol
dat_master %>% 
  filter(inf_mort > 0.012) %>% 
  print(width=Inf)

## U5_mort ####
qplot(dat_master$U5_mort, geom = "histogram")

# One outlier but it's not outrageous. Battambang > Ta Pon
dat_master %>% 
  filter(U5_mort > 0.02) %>% 
  print(width=Inf)

## Prop_Indigenous ####
qplot(dat_master$Prop_Indigenous, geom = "histogram")

# One problem - three commune have proportions over 1
dat_master %>% 
  filter(Prop_Indigenous > 1) %>% 
  print(width=Inf)
# The communes are Mondul Kiri > Bu Sra, Preah Vihear > Prame, Stung Treng > Santepheap

## dist_sch ####
qplot(dat_master$dist_sch, geom = "histogram")

# Lots of high values
dat_master %>% 
  filter(dist_sch > 50) %>% 
  print(width=Inf)

# after running above cleaning there are still a few communes with high values, but in the cleaning stage above I decided to leave them as they are for various reasons. I can always remove or edit later. 

#### Plots ----------------------------------------------------------------------------------

# response variable = forest cover % change?  Or absolute amount lost? Do we look at forest loss from 2009-2010, or 2010-2011? Do we look at multiple years?

# Scale of response = commune
# If there are more than 1 pixels in a commune - 

#### CCI LAND COVER DATA------------------------------------------------------------------------------------------------------

#### Load data ####
library('tidyverse')

LC_dat <- read_csv("CCI_ForCov_Commune.csv")
str(LC_dat)
LC_dat$perc_change <- as.numeric(LC_dat$perc_change)

# remove #DIV/0! and change them to 0's
LC_dat <- LC_dat %>% 
          mutate(perc_change = replace(perc_change, perc_change == "#DIV/0!", 0))

# histograms of percent change
qplot(LC_dat$perc_change, geom = "histogram")

# checking outliers
LC_dat %>% 
  filter(perc_change > 90)
# Kampong Preah Kokir has percent change of 100.

# Find details of above commune
dat_master %>% 
  filter(Commune=="Kampong Preah")
  filter(CommCode==40105)
