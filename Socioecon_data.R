#### SOCIOECONOMIC DATA------------------------------------------------------------------------------------------------------
#### Load libraries and data ####

library('cowplot')
library('tidyverse')

dat <- read.csv("Socioeconomic_variables_2010.csv")
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
          mutate(KM_Market = ifelse(Province=="Kandal",
                   ifelse(KM_Market > 50, 5.137493, KM_Market),
                   KM_Market))

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
## crim_case errors####

# Koh Kong > Chi Phat
dat2 %>% 
  filter(Province=="Koh Kong") %>% 
  filter(Commune == "Chi Phat") %>% 
  select(VillGis,crim_case)
# One village has a very high number of cases. I will replace it with commune mean (0.003507532)

# Replace values
dat2 <- dat2 %>%
        mutate(crim_case = ifelse(VillGis==9070504, 
                                replace(crim_case, crim_case==crim_case, 0.003507532), 
                                crim_case))

# Pursat > Krapeu Pir
dat2 %>% 
  filter(Province=="Pursat") %>% 
  filter(Commune == "Krapeu Pir") %>% 
  select(VillGis,crim_case)
# One village has a very high number of cases. I will replace it with commune mean (0.003012048)

# Replace values
dat2 <- dat2 %>%
        mutate(crim_case = ifelse(VillGis==15060202, 
                                replace(crim_case, crim_case==crim_case,0.003012048), 
                                crim_case))

# Preah Sihanouk > Kaoh Rung
dat2 %>% 
  filter(Province=="Preah Sihanouk") %>% 
  filter(Commune == "Kaoh Rung") %>% 
  select(VillGis,crim_case)
# two villages have high values, will replace with commune mean (0.01462475)

# Replace values
dat2 <- dat2 %>%
        mutate(crim_case = ifelse(VillGis==18010501, 
                                replace(crim_case, crim_case==crim_case,0.01462475), 
                                crim_case))

# Replace values
dat2 <- dat2 %>%
        mutate(crim_case = ifelse(VillGis==18010502, 
                                replace(crim_case, crim_case==crim_case,0.01462475), 
                                crim_case))

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


## Dealing with KM_Comm errors, but had to do it here once CommCode existed
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

# Replace KM_Comm values in Peam Bang with the provincial mean
dat_master <- dat_master %>% 
              mutate(KM_Comm = ifelse(CommCode==60807,
                                      replace(KM_Comm, KM_Comm==KM_Comm, 3.3),
                                      KM_Comm))


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

# There are a few provinces with a handful of outliers, but I am inclined to believe these, as the numbers are not beyond what I would expect.  Provinces are: Banteay Meanchey, Battambang, Kampot, Kracheh, Pailin, Pursat, and Siem Reap.  

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
# Apparently Kaoh Peak commune in Ratankiri, 0% of their 6-24 year olds (male and female) are in school.  THis sounds like a Null response in the data.  THe total population of the commune is nearly 3000, with over 600 families.  That means there MUST be chldren between6 and 24 years old, and some of them must be in school.  Although....the nearest school is 21km away.  And the whole commune is indigenous. More than half of adult women are illiterate and nearly half of men are. All families rely on farming as their primary livelihood.  They are 53km from the nearest market. Perhaps it's true?

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


#### CCI LAND COVER DATA------------------------------------------------------------------------------------------------------

#### Load data ####
library('tidyverse')

## This is ALL of the data (including communes that start with no forest in 2010). No need to load this anymore
#LC_dat <- read_csv("CCI_ForCov_Commune.csv")
#str(LC_dat)

# Change data classes
#LC_dat$perc_change <- as.numeric(LC_dat$perc_change)
#LC_dat$perc_change_dir <- as.numeric(LC_dat$perc_change_dir)
#LC_dat$CODEKHUM <- as.factor(LC_dat$CODEKHUM)


### This section below is a result of talking to Nils about how to deal with all of the 0's in percent change.  I have decided I am not interested in Communes that have no forest cover in 2010 (and therefore cannot physically lose any forest between 2010 and 2011).  However, I AM interested in Communes that start with forest cover, but do not lose any between 2010 and 2011.  I have discussed with Nils the idea of using a zero-inflated model, which will deal with all the 0's.  Therefore what I have done with LC_dat1 (starting row 2138) below is now not appropriate.  

# Load data that only has forested communes
LC_dat_forest <- read_csv("CCI_ForCov_Commune_forest.csv")
LC_dat_forest$CommCode <- as.character(LC_dat_forest$CommCode)

# Load data for commune centre xy coords
communeXY <- read_csv("Commune_centres.csv")
communeXY$CommCode <- as.character(communeXY$CommCode)

# merge LC_dat_forest and commune xy
LC_dat_forest_merge <- left_join(LC_dat_forest, communeXY, by="CommCode")

# Check for duplicates
LC_dat_forest_merge %>% filter(duplicated(CommCode) | duplicated(CommCode, fromLast = TRUE))

# Remove duplicates
LC_dat_forest <- distinct(LC_dat_forest_merge, CommCode, .keep_all=TRUE)
str(LC_dat_forest)


### Identifying the communes that have no forest in 2010 ####
## Load raw data

## This section is not necessary each time. It was used to create the data that can now be read in above as LC_dat_forest



LC_dat_raw_2010 <- read_csv("ForCov_2010_raw.csv")
LC_dat_raw_2011 <- read_csv("ForCov_2011_raw.csv")

# Remove all communes that start with no forest in 2010 (i.e. structural 0's)
LC_dat_forest_2010 <- LC_dat_raw_2010 %>% 
                      filter(! (HISTO_50==0 & HISTO_60==0 & HISTO_61==0 & HISTO_62==0 & HISTO_70==0 
                            & HISTO_80==0 & HISTO_90==0 & HISTO_100==0))

# Remove the same communes in 2011 (I assume they will be the same)
LC_dat_forest_2011 <- LC_dat_raw_2011 %>% 
                      filter(! (HISTO_50==0 & HISTO_60==0 & HISTO_61==0 & HISTO_62==0 & HISTO_70==0 
                            & HISTO_80==0 & HISTO_90==0 & HISTO_100==0))

# remove unwanted columns & rename CommCode for 2010
LC_dat_forest_2010 <- LC_dat_forest_2010 %>% 
                      select(-c(fid,KHUM50_,KHUM50_ID,PERIMETER,AREA)) %>% 
                      rename(CommCode = CODEKHUM)

# remove unwanted columns & rename CommCode for 2011
LC_dat_forest_2011 <- LC_dat_forest_2011 %>% 
                      select(-c(fid,KHUM50_,KHUM50_ID,PERIMETER,AREA)) %>% 
                      rename(CommCode = CODEKHUM)

# double check that the remaining communes match between 2010 and 2011
anti_join(LC_dat_forest_2011,LC_dat_forest_2010, by="CommCode")

# There is one commune (Kampong Preah Kokir) that is present (i.e. forested) in 2010 but not 2011. I will sort this in Excel
write.csv(LC_dat_forest_2010, file="LC_dat_forest_2010.csv")
write.csv(LC_dat_forest_2011, file="LC_dat_forest_2011.csv")

## Clean data - this is for all data including communes with no forest in 2010 ####
# remove #DIV/0! and change them to 0's, and rename variables to match dat_master
LC_dat <- LC_dat %>% 
          mutate(perc_change = replace(perc_change, perc_change == "#DIV/0!", 0)) %>% 
          mutate(perc_change_dir = replace(perc_change_dir, perc_change_dir == "#DIV/0!", 0)) %>% 
          rename(CommCode = CODEKHUM) %>% 
          rename(Commune = KHUM_NAME)

# checking outliers
LC_dat %>% 
  filter(perc_change == 100)
# Kampong Preah Kokir has percent change of 100. After double checking it is correct.  in 2010 the commune had 4 pixels of forest cover and in 2011 it had 0.  Therefore it has lost 100% of its forest cover.  

# See what the histogram looks like with 0's removed
LC_dat1 <- LC_dat %>% 
            filter(perc_change > 0)

qplot(LC_dat1$perc_change, geom = "histogram")

# Now remove negative change (i.e. where the commune has gained forest) 
LC_dat1 <- LC_dat1 %>% 
           filter(!perc_change < 0 )
       

## Clean data - this is for data that only has communes that were forested in 2010 ####

qplot(perc_change, data=LC_dat_forest, geom="histogram")
# There is still that outlier at 100% change.  I don't like it because it only had a tiny amount of forest to begin with

LC_dat_forest <- LC_dat_forest %>% filter(!Commune=="Kampong Preah Kokir")

## Match commune information between data sets - Using only perc_change > 0 ####

# There is a difference of 11 communes between the data sets. That means there are 11 communes which we do not have a forest loss value for. These will need to be identified and removed.
length(levels(dat_master$CommCode))
length(levels(LC_dat1$CommCode))

## NOTE - by filtering out the 0's and negative values, the data set gets reduced massively to only 140 observations. So need to use LC_dat NOT LC_dat1 when identifying missing communes

# pull out CommCode variables 
dat_master_temp <- dat_master %>% select(CommCode)
LC_dat_temp <- LC_dat %>% select(CommCode)

# compare them
anti_join(dat_master_temp, LC_dat_temp, by="CommCode")

# The commune codes from the GIS layer do not match the commune codes from the commune database very well. There are 90 CommCodes in dat_master that are not in LC_dat. I will try looking at Commune names

# pull out Commune variables
dat_master_temp <- dat_master %>% select(Commune)
LC_dat_temp <- LC_dat %>% select(Commune)
LC_dat1_temp <- LC_dat1 %>% select(Commune)

dat_master_temp$Commune <- as.character(dat_master_temp$Commune)
LC_dat_temp$Commune <- as.character(LC_dat_temp$Commune)
LC_dat1_temp$Commune <- as.character(LC_dat1_temp$Commune)

# compare them (with LC_dat1 as x because we only need to match the communes with perc_change > 0)
comm_mismatch <- anti_join(LC_dat1, dat_master, by="Commune", copy=T)
comm_mismatch

## fixing commune mismatches - Using only perc_change > 0  ####

# There are 11 communes that are in LC_dat (filtered to remove 0's and negatives, n=140) but not in dat_master.  The ones with no Commune name and whose CommCodes don't match any CommCodes in dat_master will be deleted, because I don't know how to identify them in the commune database.  

# CommCode = 160800, no Commune name. Missing from dat_master and can't corroborate so will delete
LC_dat1 <- LC_dat1 %>% 
            filter(!CommCode==160800)

# Commcode = 160102, Commune = Mai Hie. Missing from dat_master and can't corroborate 
dat_master %>% 
  filter(Province=="Ratanak Kiri") %>% 
  select(Commune) %>% 
  arrange(tolower(Commune)) %>% 
  print(n=Inf)
 
LC_dat1 <- LC_dat1 %>% 
            filter(!CommCode==160102)

# CommCode = 190401, Commune = Stung Treng
dat_master %>% 
  filter(Province=="Stung Treng") %>% 
  select(Commune) %>% 
  arrange(tolower(Commune)) %>% 
  print(n=Inf)
# THis commune exists, just has different spelling of the name. I will change the name. There is only one commune with that name so I can use the below code

LC_dat1 <- LC_dat1 %>% 
           mutate(Commune = replace(Commune, Commune=="Steung Traeng", "Stueng Traeng"))

# CommCode = 99, No commune name. Checked in GIS layer and this is actually the Tonle Sap lake. Can delete
LC_dat1 <- LC_dat1 %>%
            filter(!CommCode==99)

# CommCode = 20900, no commune name
dat_master %>% 
  filter(Province=="Battambang") %>% 
  select(Commune) %>% 
  arrange(tolower(Commune)) %>% 
  print(n=Inf)
# After much investigations, I have worked out that this is in fact a district, which is made up of 7 Communes. Those communes exist in dat_master (so I have socioeconomic data for them), but I don't have the boundaries in GIS. Therefore I can't be spatially explicit when matching socioeconoimc data to forest data.  And I've looked at the forest cover layers and there is only one discrete place where forest is lost and so it would not be appropriate to allocate forest loss to the entire district. Therefore I will delete the observation from LC_dat1. 

LC_dat1 <- LC_dat1 %>%
            filter(!CommCode==20900)

# CommCode = 150602, no commune name
dat_master %>% 
  filter(Province=="Pursat") %>% 
  select(Commune) %>% 
  arrange(tolower(Commune)) %>% 
  print(n=Inf)
# Found a matching CommCode in dat_master. Will add it to LC_Dat1
LC_dat1 <- LC_dat1 %>% 
           mutate(Commune = ifelse(CommCode==150602,
                            replace(Commune, is.na(Commune), "Krapeu Pir"),
                            Commune))

# Commcode = 150603, no commune name.  Found a matching commune in dat_master
LC_dat1 <- LC_dat1 %>% 
           mutate(Commune = ifelse(CommCode==150603,
                            replace(Commune, is.na(Commune), "Anlong Reab"),
                            Commune))

# CommCode = 150604, no commune name. Found a matching commune in dat_master
LC_dat1 <- LC_dat1 %>% 
           mutate(Commune = ifelse(CommCode==150604,
                            replace(Commune, is.na(Commune), "Pramaoy"),
                            Commune))

# CommCOde = 150605, no commune name. Found a matching commune in dat_master
LC_dat1 <- LC_dat1 %>% 
           mutate(Commune = ifelse(CommCode==150605,
                            replace(Commune, is.na(Commune), "Thma Da"),
                            Commune))

# CommCode = 150601, no commune name. Found a matching commune in dat_master
LC_dat1 <- LC_dat1 %>% 
           mutate(Commune = ifelse(CommCode==150601,
                            replace(Commune, is.na(Commune), "Ou Saom"),
                            Commune))

# Commcode = 70700, no commune name. It's in Kampot.  
dat_master %>% 
  filter(Province=="Kampot") %>% 
  select(Commune) %>% 
  arrange(tolower(Commune)) %>% 
  print(n=Inf)
# I can't find any missing communes within that district between the two data sets, so I am not able to reconcile the missing data. I will delete it from LC_dat1

LC_dat1 <- LC_dat1 %>%
            filter(!CommCode==70700)

## double check the mismatches again
comm_mismatch <- anti_join(LC_dat1, dat_master, by="Commune", copy=T)
comm_mismatch


## Match Commune info between data sets - Using all perc_change BUT only '10 forested communes ####

# Lets see how badly the data sets match.  I want to know if all of the communes in LC_dat_forest are in dat_master
anti_join(LC_dat_forest, dat_master, by="CommCode")
# There are 37 communes that are in LC_dat_forest but not matched in dat_master.  This will be because of Commune/CommCode mismatches. I will see if I can use Commune name to find the commune in dat_master.  If I can, I will change the CommCode in dat_master. Best to change it there because in the future I am likely to want to find the commune in GIS, so I need those CommCodes to match.

# Change CommCode to CHARACTER to make editing the values easier
dat_master$CommCode <- as.character(dat_master$CommCode)
LC_dat_forest$CommCode <- as.character(LC_dat_forest$CommCode)


# CommCode 10509, Ou Bei Choan
dat_master %>% filter(Commune=="Ou Bei Choan")
dat_master <- dat_master %>% 
              ungroup() %>% 
              mutate(CommCode = replace(CommCode, CommCode=="10504", "10509"))

# CommCode 10210, Sambuor. There are 5 in dat_master with same name. Checked in GIS and the one I'm looking for is in Banteay Meanchey 
dat_master %>% filter(Commune=="Sambuor")
dat_master <- dat_master %>% 
              ungroup() %>% 
              mutate(CommCode = ifelse(Province=="Banteay Meanchey",
                                replace(CommCode, CommCode=="12", "10210"),
                                CommCode)) 
# CommCode 10213, Ta Lam
dat_master %>% filter(Commune=="Ta Lam")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode==15, "10213"))

# CommCode 10301, Nam Tau
dat_master %>% filter(Commune=="Nam Tau")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode==16, "10301"))

# CommCode 10302, Paoy Char
dat_master %>% filter(Commune=="Paoy Char")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode==17, "10302"))


# CommCode 20611, Preaek Chik
dat_master %>% filter(Commune=="Prek Chik")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="21401", "20611"))

# CommCode 20612, Prey Tralach
dat_master %>% filter(Commune=="Prey Tralach")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="21402", "20612"))

# CommCode 31016, Kokir. There are 3 communes with the same name. Used GIS - its the Kampong Cham commune
dat_master %>% filter(Commune=="Kokir")
dat_master <- dat_master %>% 
              ungroup() %>% 
              mutate(CommCode = ifelse(Province=="Kampong Cham",
                                replace(CommCode, CommCode=="31012", "31016"),
                                CommCode))

# CommCode 31209, Veal Mlu
dat_master %>% filter(Commune=="Veal Mlu")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="31201", "31209"))

# CommCode 31513, Tuol Preah Khleang
dat_master %>% filter(Commune=="Tuol Preah Khleang")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="31502", "31513"))

# Commcode 31514, Tuol Sambuor
dat_master %>% filter(Commune=="Tuol Sambuor")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="31511", "31514"))

# CommCode 31615, Peam Chileang
dat_master %>% filter(Commune=="Peam Chileang")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="31603", "31615"))

# CommCode 31616, Roka Po Pram
dat_master %>% filter(Commune=="Roka Po Pram")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="31607", "31616"))

# CommCode 31621, Thma Pechr
dat_master %>% filter(Commune=="Thma Pechr")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="31610", "31621"))

# CommCode 31622, Tonle Bet
dat_master %>% filter(Commune=="Tonle Bet")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="31611", "31622"))

# CommCode 31623, Vihear Luong. There are two. GIS - Kampong Cham
dat_master %>% filter(Commune=="Vihear Luong")
dat_master <- dat_master %>% 
              ungroup() %>% 
              mutate(CommCode = ifelse(Province=="Kampong Cham",
                                replace(CommCode, CommCode=="31702", "31623"),
                                CommCode))

# CommCode 50613, Traeng Trayueng
dat_master %>% filter(Commune=="Traeng Trayueng")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="50612", "50613"))


# CommCode 50808, Yea Angk
dat_master %>% filter(Commune=="Yea Angk")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="50803", "50808"))

# CommCode 60310, Srayov
dat_master %>% filter(Commune=="Srayov")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="60307", "60310"))

# CommCode 60311, Tboung Krapeu
dat_master %>% filter(Commune=="Tboung Krapeu")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="60710", "60311"))

# CommCode 70615, Svay Tong Khang Cheung
dat_master %>% filter(Commune=="Svay Tong Khang Cheung")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="70610", "70615"))

# CommCode 70616, Svay Tong Khang Tboung
dat_master %>% filter(Commune=="Svay Tong Khang Tboung")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="70611", "70616"))

# CommCode 70717, Trapeang Pring. 2 communes - GIS says Kampot
dat_master %>% filter(Commune=="Trapeang Pring")
dat_master <- dat_master %>% 
              ungroup() %>% 
              mutate(CommCode = ifelse(Province=="Kampot",
                                replace(CommCode, CommCode=="70706", "70717"),
                                CommCode))

# CommCode 80710, Sambuor Meas. 2 communes. GIS says Kandal
dat_master %>% filter(Commune=="Sambuor Meas")
dat_master <- dat_master %>% 
              ungroup() %>% 
              mutate(CommCode = ifelse(Province=="Kandal",
                                replace(CommCode, CommCode=="80705", "80710"),
                                CommCode))

# CommCode 90801, Chamkar Luong
dat_master %>% filter(Commune=="Chamkar Luong")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="180401", "90801"))

# CommCode 90803, Ou Bak Roteh
dat_master %>% filter(Commune=="Ou Bak Roteh")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="180403", "90803"))

# CommCode 90804, Stueng Chhay
dat_master %>% filter(Commune=="Stueng Chhay")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="180404", "90804"))

# CommCode 100211, Roka Kandal
dat_master %>% filter(Commune=="Roka Kandal")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="100603", "100211"))

# CommCode 100212, Sambok
dat_master %>% filter(Commune=="Sambok")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="100207", "100212"))


# CommCode 100214, Thma Kreae
dat_master %>% filter(Commune=="Thma Kreae")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="100209", "100214"))

# CommCode 100215, Thmei - there are 5. GIS - Kracheh
dat_master %>% filter(Commune=="Thmei")
dat_master <- dat_master %>% 
              mutate(CommCode = ifelse(Province=="Kracheh",
                                replace(CommCode, CommCode=="100210", "100215"),
                                CommCode))
# Commcode 130705, Prame
dat_master %>% filter(Commune=="Prame")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="130701", "130705"))

# Commcode 130706, Preah Khleang
dat_master %>% filter(Commune=="Preah Khleang")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="130702", "130706"))

# CommCode 160102, Mai Hie. Doesnt exist in dat_master, but it's in Ratanak Kiri in GIS. I've checked all communes in Ratank kiri in dat_master and none of them are similar.  I will just delete it
dat_master %>% filter(Commune=="Mai Hie")
LC_dat_forest <- LC_dat_forest %>% filter(!CommCode=="160102")


# CommCode 100207, Kraoh Trong. 
dat_master %>% filter(Commune=="Kaoh Trong")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="100605", "100207"))
                               
# CommCode 100213, Thma Andaeuk
dat_master %>% filter(Commune=="Thma Andaeuk")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="100208", "100213"))
                              

# CommCode 170715, Trei Nhoar
dat_master %>% filter(Commune=="Trei Nhoar")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="170706", "170715"))

# CommCode 200108, Prey Kokir
dat_master %>% filter(Commune=="Prey Kokir")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="200101", "200108"))


# CommCode 200109, Samraong - there are 9.  GIS - Svay Rieng
dat_master %>% filter(Commune=="Samraong")
dat_master <- dat_master %>% 
              mutate(CommCode = ifelse(Province=="Svay Rieng",
                                replace(CommCode, CommCode=="200102", "200109"),
                                CommCode))
# CommCode 200110, Tuol Sdei
dat_master %>% filter(Commune=="Tuol Sdei")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="200106", "200110"))

# CommCode 200711, Svay Rumpea. There are 2. GIS - Svay Rieng
dat_master %>% filter(Commune=="Svay Rumpea")
dat_master <- dat_master %>% 
              mutate(CommCode = ifelse(Province=="Svay Rieng",
                                replace(CommCode, CommCode=="200701", "200711"),
                                CommCode))
# CommCode 220106, Lumtong
dat_master %>% filter(Commune=="Lumtong")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="220102", "220106"))


# CommCode 100208, Krakor
dat_master %>% filter(Commune=="Krakor")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="100601", "100208"))

# CommCode 130701, Kampong Pranak
dat_master %>% filter(Commune=="Kampong Pranak")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="130801", "130701"))

# CommCode 170706, Krabei Riel
dat_master %>% filter(Commune=="Krabei Riel")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="171012", "170706"))

# CommCode 200101, Bati
dat_master %>% filter(Commune=="Bati")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="200804", "200101"))

# CommCode 200102, Bavet
dat_master %>% filter(Commune=="Bavet")
dat_master <- dat_master %>% 
              mutate(CommCode = replace(CommCode, CommCode=="200801", "200102"))

# CommCOde 200106, Prasat. There are 6. GIS - Svay Rieng
dat_master %>% filter(Commune=="Prasat")
dat_master <- dat_master %>% 
              mutate(CommCode = ifelse(Province=="Svay Rieng",
                                replace(CommCode, CommCode=="200803", "200106"),
                                CommCode))

#

## Subsetting dat_master to match LC_dat_forest and merging dataframes ####

# Join dataframes
dat_working <- left_join(dat_master, LC_dat_forest, by = "CommCode") 
head(dat_working)
str(dat_working)

# remove NAs (i.e. rows that do not exist in LC_dat_forest)
dat_working <- dat_working %>% filter(!is.na(perc_change))

# check it worked
dat_working %>% 
  filter(Province=="Battambang") %>% 
  select(CommCode,Commune.x,Commune.y,perc_change) %>% 
  print(n=Inf)

#### COVARIANCE ANALYSIS SOCIOECONOMIC VARS------------------------------------------------------------------------------------

#### Load libraries & subset data ####
library('corrplot')
library('Hmisc')

demogs <- dat_master %>% 
          select(tot_pop,family,male_18_60, fem_18_60,pop_over61,Prop_Indigenous)

education <- dat_master %>% 
              select(F6_24_sch,M6_24_sch,F18_60_ill,M18_60_ill)

employ <- dat_master %>% 
              select(numPrimLivFarm,propPrimLivFarm,Fish_man,ntfp_fam,fam_prod,Cloth_craft,
                     Trader,serv_prov)

econSec <- dat_master %>% 
              select(Les1_R_Land,No_R_Land,Les1_F_Land,No_F_Land,pig_fam,cow_fam)

service <- dat_master %>% 
              select(dist_sch,garbage,KM_Market,KM_Comm,wat_safe,wat_pipe)

justice <- dat_master %>% 
            select(land_confl,crim_case)

health <- dat_master %>% 
            select(KM_Heal_cent,inf_mort,U5_mort)

migrate <- dat_master %>% 
            select(Pax_migt_in,Pax_migt_out)

## correlations ####

# Demographics. As expected, total population, number of families, number of males and females, and number of people over 61 are all correlated. 
corr_demogs <- rcorr(as.matrix(demogs, type="pearson"))
corr_demogs

# Education. There is a strong positive correlation between the number of females in school and the number of males in school. This is expected. There is also a fairly decent negative relationship between the number of females and males in school and the number of illiterate adults. There is also a strong positive correlation between number of illiterate males and females which is also expected.  
corr_education <- rcorr(as.matrix(education, type="pearson"))
corr_education

# Employment. There is a fairly strong negative correlation between the proportion of families who are farmers and the proportion of families who are traders. Similar (but slightly weaker) correlation between farmers and service providers. weak positive correlation between traders and service providers. 
corr_employ <- rcorr(as.matrix(employ, type="pearson"))
corr_employ

# Economic security. fairly strong positive correlation between proportion of families with pigs and those with cattle
corr_econSec <- rcorr(as.matrix(econSec, type="pearson"))
corr_econSec

# Access to services. No major correlations. Slight positive correlation between distance to markets and distance to schools, and access to safe water and access to piped water
corr_service <- rcorr(as.matrix(service, type="pearson"))
corr_service 

# Social justice. None
corr_justice <- rcorr(as.matrix(justice, type="pearson"))
corr_justice

# Health. None
corr_health <- rcorr(as.matrix(health, type="pearson"))
corr_health

# Migration. None
corr_migrate <- rcorr(as.matrix(migrate, type="pearson"))
corr_migrate

head(dat_master)
length(LC_dat1$perc_change)
head(LC_dat1)



#### MODELLING----------------------------------------------------------------------------------------------------

## Joining dataframe (for data when percent loss > 0) ####

## This section below is old - from before I used only 2010 forested communes

# Join tables
dat_vars_forcov <- left_join(dat_master,LC_dat1, by = "CommCode")
head(dat_vars_forcov)

# filter out the communes that don't have any forest loss
dat_vars_forcov %>% select(perc_change) %>% print(n=100)
dat_vars_forcov <- dat_vars_forcov %>% 
                    filter(!is.na(perc_change))

# Check the length matches LC_dat1
length(LC_dat1$CommCode)

# Check data in Excel
write.csv(dat_vars_forcov, file="dat_vars_forcov.csv")
write.csv(LC_dat1, file="LC_dat1.csv")

# Length not quite matching
comm_mismatch1 <- anti_join(LC_dat1, dat_vars_forcov, by="CommCode", copy=T)
comm_mismatch1
str(dat_vars_forcov)
str(LC_dat1)

# There are 3 communes which have differing CommCodes between dat_master and LC_Dat1.  They are the only communes with those names so I am not worried about duplicate communes. I will leave them out for now
dat_master %>% filter(Commune == "Stueng Chhay")




#### Plots ####
library('cowplot')

# Demographics
p1 <- ggplot(dat_working, aes(x=tot_pop, y=perc_change))+
      geom_point()
p2 <- ggplot(dat_working, aes(x=family, y=perc_change))+
      geom_point()
p3 <- ggplot(dat_working, aes(x=male_18_60, y=perc_change))+
      geom_point()
p4 <- ggplot(dat_working, aes(x=fem_18_60, y=perc_change))+
      geom_point()
p5 <- ggplot(dat_working, aes(x=pop_over61, y=perc_change))+
      geom_point()
p6 <- ggplot(dat_working, aes(x=Prop_Indigenous, y=perc_change))+
      geom_point()

plot_grid(p1,p2,p3,p4,p5,p6)

# Education
p7 <- ggplot(dat_working, aes(x=F6_24_sch, y=perc_change))+
      geom_point()
p8 <- ggplot(dat_working, aes(x=M6_24_sch, y=perc_change))+
      geom_point()
p9 <- ggplot(dat_working, aes(x=F18_60_ill, y=perc_change))+
      geom_point()
p10 <- ggplot(dat_working, aes(x=M18_60_ill, y=perc_change))+
      geom_point()

plot_grid(p7,p8,p9,p10)

# Employment
p11 <- ggplot(dat_working, aes(x=numPrimLivFarm, y=perc_change))+
      geom_point()
p12 <- ggplot(dat_working, aes(x=propPrimLivFarm, y=perc_change))+
      geom_point()
p13 <- ggplot(dat_working, aes(x=Fish_man, y=perc_change))+
      geom_point()
p14 <- ggplot(dat_working, aes(x=ntfp_fam, y=perc_change))+
      geom_point()
p15 <- ggplot(dat_working, aes(x=fam_prod, y=perc_change))+
      geom_point()
p16 <- ggplot(dat_working, aes(x=Cloth_craft, y=perc_change))+
      geom_point()
p17 <- ggplot(dat_working, aes(x=Trader, y=perc_change))+
      geom_point()
p18 <- ggplot(dat_working, aes(x=serv_prov, y=perc_change))+
      geom_point()

plot_grid(p11,p12,p13,p14,p15,p16,p17,p18)

# Economic security
p19 <- ggplot(dat_working, aes(x=Les1_R_Land, y=perc_change))+
      geom_point()
p20 <- ggplot(dat_working, aes(x=No_R_Land, y=perc_change))+
      geom_point()
p21 <- ggplot(dat_working, aes(x=Les1_F_Land, y=perc_change))+
      geom_point()
p22 <- ggplot(dat_working, aes(x=No_F_Land, y=perc_change))+
      geom_point()
p23 <- ggplot(dat_working, aes(x=pig_fam, y=perc_change))+
      geom_point()
p24 <- ggplot(dat_working, aes(x=cow_fam, y=perc_change))+
      geom_point()

plot_grid(p19,p20,p21,p22,p23,p24)

# Access to services
p25 <- ggplot(dat_working, aes(x=dist_sch, y=perc_change))+
      geom_point()
p26 <- ggplot(dat_working, aes(x=garbage, y=perc_change))+
      geom_point()
p27 <- ggplot(dat_working, aes(x=KM_Market, y=perc_change))+
      geom_point()
p28 <- ggplot(dat_working, aes(x=KM_Comm, y=perc_change))+
      geom_point()
p29 <- ggplot(dat_working, aes(x=wat_safe, y=perc_change))+
      geom_point()
p30 <- ggplot(dat_working, aes(x=wat_pipe, y=perc_change))+
      geom_point()

plot_grid(p25,p26,p27,p28,p29,p30)

# Social justice
p31 <- ggplot(dat_working, aes(x=land_confl, y=perc_change))+
      geom_point()
p32 <- ggplot(dat_working, aes(x=crim_case, y=perc_change))+
      geom_point()

plot_grid(p31,p32)

# Health
p33 <- ggplot(dat_working, aes(x=KM_Heal_cent, y=perc_change))+
      geom_point()
p34 <- ggplot(dat_working, aes(x=inf_mort, y=perc_change))+
      geom_point()
p35 <- ggplot(dat_working, aes(x=U5_mort, y=perc_change))+
      geom_point()

plot_grid(p33,p34,p35)

# Migration
p36 <- ggplot(dat_working, aes(x=Pax_migt_in, y=perc_change))+
      geom_point()
p37 <- ggplot(dat_working, aes(x=Pax_migt_out, y=perc_change))+
      geom_point()

plot_grid(p36,p37)


## brms package - Bayesian models ####

library('brms')


brm_mod1 <- brm(perc_change ~ propPrimLivFarm+Trader+Cloth_craft, 
                data = dat_working, 
                family = hurdle_gamma(link = "log", link_shape = "log", link_hu = "logit"))

summary(brm_mod1)
marginal_effects(brm_mod1)


brm_mod2 <- brm(perc_change ~ propPrimLivFarm+Trader+Cloth_craft, 
                data = dat_working, 
                family = hurdle_gamma(link = "log"))

summary(brm_mod2)
marginal_effects(brm_mod2)

brm_mod3 <- brm(perc_change ~ numPrimLivFarm+M18_60_ill+male_18_60, 
                data = dat_working, 
                family = hurdle_lognormal(link = "identity", link_hu = "logit"))

summary(brm_mod3)
marginal_effects(brm_mod3)


# WARNING takes over 3 hours to run
brm_mod4 <- brm(bf(perc_change ~ numPrimLivFarm+M18_60_ill+male_18_60, 
                hu ~ CommCode),
                data = dat_working, 
                family = hurdle_lognormal(link = "identity", link_hu = "logit"))

summary(brm_mod4)
marginal_effects(brm_mod4)


## crch package - ML models ####
library(crch)

# In the paper for the package, they first fit a logistic censored model for "rain" with one variable as the regressor for the location and another variable as the regressor of the scale.  I am not sure what the scale and location are.  This is the example of the bionmial model or hurdle model.

# model 1
cens.mod1 <- crch(perc_change ~ propPrimLivFarm+Prop_Indigenous+Les1_R_Land, 
                  data = dat_working, left = 0, right = 100, dist = "logistic")

summary(cens.mod1)

# model 2
tot_pop10 <- (dat_working$tot_pop)*10
cens.mod2 <- crch(perc_change ~ tot_pop | tot_pop10, 
                  data = dat_working, left = 0, dist = "logistic")

summary(cens.mod2)
plot(perc_change ~ tot_pop, data=dat_working)
abline(coef(cens.mod2)[1:2], col="blue")
cens.mod2$coefficients

# predict from model 2
newx <- data.frame(tot_pop = seq(301,35300, length=100),
                   tot_pop10 = seq(3010,353000, length=100))

newy <- predict(cens.mod2, newdata = newx, int="c")
summary(newy)
plot(perc_change ~ tot_pop, data=dat_working)
matlines(newx,newy,lty=c(1,2,2),col="black")

# model 3
cens.mod3 <- crch(perc_change ~ tot_pop, 
                  data = dat_working, left = 0, dist = "logistic")
summary(cens.mod3)


## trying normal GLM ####
library(boot)

# first need to transform perc_change into 1's and 0's
dat_working <- dat_working %>% 
                mutate(perc_change_bin = ifelse(perc_change==0, 
                                                perc_change,
                                                1))
summary(dat_working$perc_change_bin)
plot(dat_working$perc_change_bin ~ dat_working$tot_pop)

# model
glm.mod1 <- glm(perc_change_bin ~ tot_pop, data = dat_working, family = "binomial")
summary(glm.mod1)
glm.diag.plots(glm.mod1)


## Dormann et al 2007 ####

# first need to create new variable for perc_change with binary response
dat_working <- dat_working %>% 
                mutate(perc_change_bin = ifelse(perc_change==0, 
                                                perc_change,
                                                1))

# run non-spatial model
summary(ns_mod1 <- glm(perc_change_bin ~ tot_pop + Prop_Indigenous, 
                       family=binomial,
                       data=dat_working))

# install / load pacakge
library('ncf')

model <- ns_mod1 
correlog1.1 <- correlog(dat_working$easting, dat_working$northing, 
                        residuals(model),na.rm=T, increment=1, resamp=0)

# now plot only the first 20 distance classes:
par(mar=c(5,5,0.1, 0.1))
plot(correlog1.1$correlation[1:2000], type="b", pch=16, cex=1.5, lwd=1.5,
xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)


# make a map of the residuals:
plot(dat_working$easting, dat_working$northing, col=c("blue",
"red")[sign(resid(model))/2+1.5], pch=19,
cex=abs(resid(model))/max(resid(model))*2, xlab="geographical xcoordinates",
ylab="geographical y-coordinates")

# calculate Moran's I values explicitly for a certain distance, and to test for its significance:
require(spdep)
comm.nb <- dnearneigh(as.matrix(dat_working[43:44]), 0, 100) #give lower and upper distance class here!

## This is not working
comm.listw <- nb2listw(comm.nb, zero.policy=TRUE ) # turns neighbourhood object into a weighted list

#this next step takes often several minutes to run:
GlobMT1.1<- moran.test(residuals(model), listw=snouter.listw)

library('nlme')
gls.mod1 <- gls(perc_change_bin ~ tot_pop + Prop_Indigenous, data=dat_working)
vario1 <- Variogram(gls.mod1, form = ~x + y, resType = "pearson") 
plot(vario1, smooth=TRUE)

## Spatial eigenvector mapping ####
library(spdep)
library(boot)
library(adespatial)
library(rgdal)
library(sf)
library(raster)

comm_sp <- SpatialPointsDataFrame(dat_working[c("easting","northing")], 
                                  proj4string = 
                                  CRS("+proj=utm +zone=48N +datum=WGS84"),
                                  dat_working)

# NOTE I need to use poly2nb function from adespatial, which creates neighbour surface using polygons.  I think I need to use this because my commune centroid points are not equally spaced, and so setting the upper distance bound for dnearneigh() is tricky

# DON'T DO THIS EVERY TIME. Export this layer to use to clip boundary_khum.shp to only the communes in dat_working
writeOGR(comm_sp, dsn = "H://PhD_Objective1", layer = "comm_sp", driver = "ESRI Shapefile")

# Import commune shapefile
communeSHP <- readOGR(dsn = "H://PhD_Objective1", layer = "boundary_khum_forest2010")
communeSHP <- readOGR(dsn = "C://Users/Matt&Kez/Documents/Matt PhD/PhD_Objective1", 
                      layer = "boundary_khum_forest2010")
plot(communeSHP)

# Ensuring the number of polygons matches the number of rows in dat_master otherwise when you try to get the eigenvectors during the modeling below it throws up an error about x and/or y not matching. For some reason there are 761 points in comm_sp but communeSHP has 768.  In GIS I have identified the communes which do not appear in com_sp.  I will remove them from communeSHP

# can't get it to work with multiple features so having to do them one by one...
communeSHP <- communeSHP[communeSHP@data$KHUM50_ID != 21206,]
communeSHP <- communeSHP[communeSHP@data$KHUM50_ID != 150602,]
communeSHP <- communeSHP[communeSHP@data$KHUM50_ID != 150604,]
communeSHP <- communeSHP[communeSHP@data$KHUM50_ID != 150603,]
communeSHP <- communeSHP[communeSHP@data$KHUM50_ID != 60104,]
communeSHP <- communeSHP[communeSHP@data$KHUM50_ID != 150605,]
communeSHP <- communeSHP[communeSHP@data$KHUM50_ID != 150601,]

str(communeSHP@data)

# Neighbour analysis
neighpoly <- poly2nb(communeSHP)
plot(neighpoly, coordinates(communeSHP), add = TRUE, pch = 20, col = "red")

#nneighb_5000 <- dnearneigh(comm_sp, 0, 20000)
nneighb_dists <- nbdists(neighpoly, coordinates(comm_sp))
#nneighb_sims <- lapply(nneighb_dists, function(x) (1-((x/4)^2)) )
ME.listw <- nb2listw(neighpoly, style="B", zero.policy = TRUE)

# I should test to see if adding weights (e.g. glist = nneighb_sims) actually does anything to the model outputs.  I doubt it now that I have used polygons for the neighbour analysis


### Demographics ####
library(faraway)
library(gridExtra)
library(grid)
library(scales)
library(export)

# plots 
plot(dat_working$perc_change_bin ~ dat_working$tot_pop)
plot(dat_working$perc_change_bin ~ dat_working$Prop_Indigenous)
pairs(demogs)

## Non-spatial model
glm.modDem1 <- glm(perc_change_bin ~ tot_pop * Prop_Indigenous, 
                data = dat_working, family = binomial)

glm.diag.plots(glm.modDem1)
summary(glm.modDem1)

# Get eigenvectors
me.modDem1 <- ME(perc_change_bin ~ tot_pop * Prop_Indigenous, 
              data=dat_working, family=binomial, listw = ME.listw, alpha=0.4)

## Spatial model 1
sp.modDem1 <- glm(perc_change_bin ~ tot_pop * Prop_Indigenous + fitted(me.modDem1), 
                data = dat_working, family = binomial)

glm.diag.plots(sp.modDem1) # There are potentially 3 points having a lot of influence
summary(sp.modDem1)
vif(sp.modDem1) # some variance inflation

# Compare models.  The spatial model has less residual deviance and is the better model
anova(sp.modDem1, glm.modDem1, test="Chisq")

## Spatial model 2. Simplify spatial model by removing interaction

# First get eigenvectors
me.modDem2 <- ME(perc_change_bin ~ tot_pop + Prop_Indigenous, 
              data=dat_working, family=binomial, listw = ME.listw)
me.modDem2.fit <- fitted(me.modDem2)

sp.modDem2 <- glm(perc_change_bin ~ tot_pop + Prop_Indigenous + as.vector(me.modDem2.fit), 
                data = dat_working, family = binomial)

summary(sp.modDem2)
glm.diag.plots(sp.modDem2)

# Compare models.  Looks like the model with interaction has lower resid. dev.
anova(sp.modDem2,sp.modDem1, test = "Chisq")

# I'm not sure whether to include the interaction.  The model appears to be better but that doesn't necessarily mean there is a true interaction.  With the interaction, Prop_indigneous is not significant, but when I remove the interaction the significance of tot_pop goes down and Prop_indigenous goes up.  The actual direction of the effect for Prop_indigenous changes when the mode gets simplified.  The standard error for Prop_indig is huge in the more complicated model, so I am not happy to trust it.


## Simplify spatial model further. Prop_indigenous has a stronger effect and higher significance, so remove tot_pop

# first get eigenvectors
me.modDem3 <- ME(perc_change_bin ~ Prop_Indigenous, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modDem3 <- glm(perc_change_bin ~ Prop_Indigenous + fitted(me.modDem3), 
                data = dat_working, family = binomial)

summary(sp.modDem3)

# compare models.  The model with tot_pop included is better
anova(sp.modDem2, sp.modDem3, test="Chi")
glm.diag.plots(sp.modDem3)

# added variable plots
avPlots(sp.modDem2)

## Predictions and plotting

# tot_pop
newtot_pop <- seq(301,35300,length=100)
mnprop_ind <- rep(mean(dat_working$Prop_Indigenous), length = 100)
mnspfitted <- rep(mean(fitted(me.modDem2)), length = 100)
newypop <- predict(sp.modDem2, 
                   newdata = list(tot_pop=newtot_pop, Prop_Indigenous=mnprop_ind, 
                        me.modDem2.fit = mnspfitted),type="link", se=TRUE)

plot(dat_working$tot_pop, dat_working$perc_change_bin, xlab="Total population", 
     ylab = "Probability of forest loss")
lines(newtot_pop,ilogit(newypop$fit),lwd=2)
lines(newtot_pop,ilogit(newypop$fit+1.96*newypop$se.fit),lty=3)
lines(newtot_pop,ilogit(newypop$fit-1.96*newypop$se.fit),lty=3)

# Plot with ggplot
pop_pred <- data.frame(newx = newtot_pop,
                       newy = ilogit(newypop$fit),
                       upr = ilogit(newypop$fit+1.96*newypop$se.fit),
                       lwr = ilogit(newypop$fit-1.96*newypop$se.fit))

p_tot_pop <- ggplot(pop_pred, aes(x=newx, y=newy))+
            geom_line(size=1, color="#000099")+
            geom_point(data=dat_working, aes(x=tot_pop, y=perc_change_bin), shape=1, size=1)+
            ylim(0,1)+
            geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
            labs(x="Total population", y="")+
            theme(plot.margin = unit(c(0,0,1.5,0), "cm"))

ggplot(pop_pred, aes(x=newx, y=newy))+
  geom_line(size=1)+
  ylim(0,1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), linetype=3, alpha=0, colour="black")+
  labs(x="Total population", y="Probability of forest loss")


library(visreg)
visreg(sp.modDem2, xvar="tot_pop", type = "conditional")

# Prop_Indigenous
newProp_ind <- seq(0,1,length=100)
mntot_pop <- rep(mean(dat_working$tot_pop), length=100)
newyind <- predict(sp.modDem2, newdata = list(Prop_Indigenous = newProp_ind,
                                              tot_pop = mntot_pop, 
                                              me.modDem2.fit = mnspfitted),
                   type = "link", se = TRUE)
plot(dat_working$Prop_Indigenous, dat_working$perc_change_bin, 
     xlab="Proportion of population indigenous", ylab = "Probability of forest loss")
lines(newProp_ind,ilogit(newyind$fit),lwd=2)
lines(newProp_ind,ilogit(newyind$fit+1.96*newyind$se.fit),lty=3)
lines(newProp_ind,ilogit(newyind$fit-1.96*newyind$se.fit),lty=3)

# Plot with ggplot
propInd_pred <- data.frame(newx = newProp_ind,
                       newy = ilogit(newyind$fit),
                       upr = ilogit(newyind$fit+1.96*newyind$se.fit),
                       lwr = ilogit(newyind$fit-1.96*newyind$se.fit))

p_prop_ind <- ggplot(propInd_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=Prop_Indigenous, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Proportion indigenous", y="")+
              theme(plot.margin = unit(c(1.5,0,0,0), "cm"))


demog_plots <- grid.arrange(
                arrangeGrob(p_tot_pop,p_prop_ind, ncol=1, 
                  left = textGrob("Probability of forest loss", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2ppt(file="demog_plots4", width=10, height=10)

#
### Education ####

# plots
par(mfrow=c(2,1))
plot(dat_working$perc_change_bin ~ dat_working$M6_24_sch)
plot(dat_working$perc_change_bin ~ dat_working$M18_60_ill)
pairs(education)

## Non-spatial model
glm.modEdu1 <- glm(perc_change_bin ~ M6_24_sch * M18_60_ill, 
                data = dat_working, family = binomial)

glm.diag.plots(glm.modEdu1)
summary(glm.modEdu1)

# Get eigenvectors
me.modEdu1 <- ME(perc_change_bin ~ M6_24_sch * M18_60_ill, 
              data=dat_working, family=binomial, listw = ME.listw)

## Spatial model 1
sp.modEdu1 <- glm(perc_change_bin ~ M6_24_sch * M18_60_ill + fitted(me.modEdu1), 
                data = dat_working, family = binomial)

glm.diag.plots(sp.modEdu1) # There are potentially 3 points having a lot of influence
summary(sp.modEdu1)
vif(sp.modEdu1) # some variance inflation, especially for M18_60_ill

# compare models. Spatial model is better
anova(glm.modEdu1,sp.modEdu1, test="Chi")

## Spatial model 2.  Simplify the model by removing interaction

# get eigenvectors
me.modEdu2 <- ME(perc_change_bin ~ M6_24_sch + M18_60_ill, 
              data=dat_working, family=binomial, listw = ME.listw, alpha = 0.05)
me.modEdu2.fit <- fitted(me.modEdu2)

sp.modEdu2 <- glm(perc_change_bin ~ M6_24_sch + M18_60_ill + as.vector(me.modEdu2.fit), 
                data = dat_working, family = binomial)

summary(sp.modEdu2)
vif(sp.modEdu2) # no variance inflation

# compare models.  Model with interaction is better, but I think this is because the variables are correlated, rather than there being an actual interaction. 
anova(sp.modEdu1,sp.modEdu2, test = "Chi")

# Try simplify model further

# get eigenvectors
me.modEdu3 <- ME(perc_change_bin ~ M18_60_ill, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modEdu3 <- glm(perc_change_bin ~ M18_60_ill + fitted(me.modEdu3),
                  data = dat_working, family = binomial)

summary(sp.modEdu3)
anova(sp.modEdu2,sp.modEdu3, test = "Chi")

# The more complicated model is better

## Predictions and plots

# M6_24_sch
newM6_24_sch <- seq(0,1,length=100)
mnM18_60_ill <- rep(mean(dat_working$M18_60_ill), length=100)
mnspfitted.ed <- rep(mean(me.modEdu2.fit), length=100)
newyM6_24_sch <- predict(sp.modEdu2, newdata = list(M6_24_sch = newM6_24_sch,
                                                    M18_60_ill = mnM18_60_ill,
                                                    me.modEdu2.fit = mnspfitted.ed),
                         type = "link", se = TRUE)

par(mfrow=c(1,1))
plot(dat_working$M6_24_sch, dat_working$perc_change_bin, xlab="Proportion of males in school", 
     ylab = "Probability of forest loss")
lines(newM6_24_sch,ilogit(newyM6_24_sch$fit),lwd=2)
lines(newM6_24_sch,ilogit(newyM6_24_sch$fit+1.96*newyM6_24_sch$se.fit),lty=3)
lines(newM6_24_sch,ilogit(newyM6_24_sch$fit-1.96*newyM6_24_sch$se.fit),lty=3)

# M18_60_ill
newM18_60_ill <- seq(0,0.9,length=100)
mnM6_24_sch <- rep(mean(dat_working$M6_24_sch), length=100)
mnspfitted.ed <- rep(mean(me.modEdu2.fit), length=100)
newyM18_60_ill <- predict(sp.modEdu2, newdata = list(M18_60_ill = newM18_60_ill,
                                                     M6_24_sch = mnM6_24_sch,
                                                    me.modEdu2.fit = mnspfitted.ed),
                          type = "link", se = TRUE)

par(mfrow=c(1,1))
plot(dat_working$M18_60_ill, dat_working$perc_change_bin, xlab="Proportion of illiterate males", 
     ylab = "Probability of forest loss")
lines(newM18_60_ill,ilogit(newyM18_60_ill$fit),lwd=2)
lines(newM18_60_ill,ilogit(newyM18_60_ill$fit+1.96*newyM18_60_ill$se.fit),lty=3)
lines(newM18_60_ill,ilogit(newyM18_60_ill$fit-1.96*newyM18_60_ill$se.fit),lty=3)

# Plot with ggplot
M6_24_sch_pred <- data.frame(newx = newM6_24_sch,
                       newy = ilogit(newyM6_24_sch$fit),
                       upr = ilogit(newyM6_24_sch$fit+1.96*newyM6_24_sch$se.fit),
                       lwr = ilogit(newyM6_24_sch$fit-1.96*newyM6_24_sch$se.fit))

p_M6_24_sch <- ggplot(M6_24_sch_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=M6_24_sch, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Proportion of young males in school", y="")+
              theme(plot.margin = unit(c(0,0,1.5,0), "cm"))

M18_60_ill_pred <- data.frame(newx = newM18_60_ill,
                       newy = ilogit(newyM18_60_ill$fit),
                       upr = ilogit(newyM18_60_ill$fit+1.96*newyM18_60_ill$se.fit),
                       lwr = ilogit(newyM18_60_ill$fit-1.96*newyM18_60_ill$se.fit))

p_M18_60_ill <- ggplot(M18_60_ill_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=M18_60_ill, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Proportion of illiterate adult males", y="")+
              theme(plot.margin = unit(c(1.5,0,0,0), "cm"))


demog_plots <- grid.arrange(
                arrangeGrob(p_M6_24_sch,p_M18_60_ill, ncol=1, 
                  left = textGrob("Probability of forest loss", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2ppt(file="education_plots", width=10, height=10)

## Employment ####

# Plots
pairs(employ)
par(mfrow=c(4,2))
plot(dat_working$numPrimLivFarm, dat_working$perc_change_bin)
plot(dat_working$propPrimLivFarm, dat_working$perc_change_bin)
plot(dat_working$Fish_man, dat_working$perc_change_bin)
plot(dat_working$ntfp_fam, dat_working$perc_change_bin)
plot(dat_working$fam_prod, dat_working$perc_change_bin)
plot(dat_working$Cloth_craft, dat_working$perc_change_bin)
plot(dat_working$Trader, dat_working$perc_change_bin)
plot(dat_working$serv_prov, dat_working$perc_change_bin)

# non-spatial model
glm.modEmp1 <- glm(perc_change_bin ~ numPrimLivFarm+propPrimLivFarm+Fish_man+ntfp_fam+fam_prod+
                     Cloth_craft+Trader+serv_prov, data = dat_working, family = binomial)

summary(glm.modEmp1)

# spatial model 1

# get eigenvectors
me.modEmp1 <- ME(perc_change_bin ~ numPrimLivFarm+propPrimLivFarm+Fish_man+ntfp_fam+fam_prod+
                     Cloth_craft+Trader+serv_prov, 
              data=dat_working, family=binomial, listw = ME.listw)
me.modEmp1.fit <- fitted(me.modEmp1)

sp.modEmp1 <- glm(perc_change_bin ~  numPrimLivFarm+propPrimLivFarm+Fish_man+ntfp_fam+fam_prod+
                     Cloth_craft+Trader+serv_prov + fitted(me.modEmp1),
                  data = dat_working, family = binomial)

summary(sp.modEmp1)
anova(sp.modEmp1, test="Chisq")

# spatial model 2 - simplified - remove PropPrimLiveFarm
me.modEmp2 <- ME(perc_change_bin ~ numPrimLivFarm+Fish_man+ntfp_fam+fam_prod+
                     Cloth_craft+Trader+serv_prov, 
              data=dat_working, family=binomial, listw = ME.listw)
me.modEmp1.fit <- fitted(me.modEmp1)

sp.modEmp2 <- glm(perc_change_bin ~  numPrimLivFarm+Fish_man+ntfp_fam+fam_prod+
                     Cloth_craft+Trader+serv_prov + fitted(me.modEmp2),
                  data = dat_working, family = binomial)

summary(sp.modEmp2)

# compare models
anova(sp.modEmp1,sp.modEmp2, test = "Chi")

# spatial model 3 - simplified - remove fam_prod
me.modEmp3 <- ME(perc_change_bin ~ numPrimLivFarm+propPrimLivFarm+Fish_man+ntfp_fam+
                     Cloth_craft+Trader+serv_prov, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modEmp3 <- glm(perc_change_bin ~  numPrimLivFarm+propPrimLivFarm+Fish_man+ntfp_fam+
                     Cloth_craft+Trader+serv_prov + fitted(me.modEmp3),
                  data = dat_working, family = binomial)

summary(sp.modEmp3)

# compare models
anova(sp.modEmp1,sp.modEmp3, test = "Chi")
anova(sp.modEmp2,sp.modEmp3, test = "Chi")

# spatial model 4 - simplified - remove Cloth_craft
me.modEmp4 <- ME(perc_change_bin ~ numPrimLivFarm+propPrimLivFarm+
                   Fish_man+ntfp_fam+fam_prod+Trader+serv_prov, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modEmp4 <- glm(perc_change_bin ~  numPrimLivFarm+propPrimLivFarm+
                   Fish_man+ntfp_fam+fam_prod+Trader+serv_prov + fitted(me.modEmp4),
                  data = dat_working, family = binomial)

summary(sp.modEmp4)

# compare models
anova(sp.modEmp1,sp.modEmp4, test = "Chi")

# spatial model 5 - simplified - remove propPrimLivFarm, fam_prod, Trader, serv_prod
me.modEmp5 <- ME(perc_change_bin ~numPrimLivFarm+Fish_man+ntfp_fam+
                     Cloth_craft, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modEmp5 <- glm(perc_change_bin ~  numPrimLivFarm+Fish_man+ntfp_fam+
                     Cloth_craft + fitted(me.modEmp5),
                  data = dat_working, family = binomial)

summary(sp.modEmp5)

# compare models.  Now the simpler model is better
anova(sp.modEmp1,sp.modEmp5, test = "Chi")

# spatial model 6 - simplified - as above but also exclude ntfp_fam
me.modEmp6 <- ME(perc_change_bin ~numPrimLivFarm+Fish_man+
                     Cloth_craft, 
              data=dat_working, family=binomial, listw = ME.listw)

me.modEmp6.fit <- fitted(me.modEmp6)

sp.modEmp6 <- glm(perc_change_bin ~  numPrimLivFarm+Fish_man+
                     Cloth_craft + me.modEmp6.fit,
                  data = dat_working, family = binomial)

summary(sp.modEmp6)

# compare models.  again the simpler model is better
anova(sp.modEmp5,sp.modEmp6, test = "Chi")

# spatial model 7 - simplified - as above but also exclude Cloth_craft
me.modEmp7 <- ME(perc_change_bin ~numPrimLivFarm+Fish_man, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modEmp7 <- glm(perc_change_bin ~  numPrimLivFarm+Fish_man + fitted(me.modEmp7),
                  data = dat_working, family = binomial)

summary(sp.modEmp7)

# compare models.  This suggests keeping Cloth_craft in
anova(sp.modEmp6,sp.modEmp7, test = "Chi")

## Predicting and plotting

# Pull out the eigenvectors 
me.modEmp6.vec <- me.modEmp6$vectors
me.modEmp6.vec <- as.data.frame(me.modEmp6.vec)

# numPrimLivFarm
newnumPrimLivFarm <- seq(0,6169,length=100)
mnFish_man <- rep(mean(dat_working$Fish_man), length=100)
mnCloth_craft <- rep(mean(dat_working$Cloth_craft), length=100)
mnvec28 <- rep(mean(me.modEmp6.vec$vec28), length=100)
mnvec16 <- rep(mean(me.modEmp6.vec$vec16), length=100)
mnvec17 <- rep(mean(me.modEmp6.vec$vec17), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
mnvec16.17.28 <- c(mnvec16, mnvec17, mnvec28)
mnme.modEmp6.fit <- matrix(mnvec16.17.28, ncol = 3, byrow = FALSE)
newynumPrimLivFarm <- predict(sp.modEmp6, 
                              newdata = list(numPrimLivFarm = newnumPrimLivFarm,
                                             Fish_man = mnFish_man,
                                             Cloth_craft = mnCloth_craft,
                                             me.modEmp6.fit = mnme.modEmp6.fit),
                              type = "link", se = TRUE)

par(mfrow=c(1,1))
plot(dat_working$numPrimLivFarm, dat_working$perc_change_bin, xlab="Families whose primary livelihood is farming", 
     ylab = "Probability of forest loss")
lines(newnumPrimLivFarm,ilogit(newynumPrimLivFarm$fit),lwd=2)
lines(newnumPrimLivFarm,ilogit(newynumPrimLivFarm$fit+1.96*newynumPrimLivFarm$se.fit),lty=3)
lines(newnumPrimLivFarm,ilogit(newynumPrimLivFarm$fit-1.96*newynumPrimLivFarm$se.fit),lty=3)


# Fish_man
newFish_man <- seq(0,1727, length=100) 
mnnumPrimLivFarm <- rep(mean(dat_working$numPrimLivFarm), length=100)
mnCloth_craft <- rep(mean(dat_working$Cloth_craft), length=100)
mnvec28 <- rep(mean(me.modEmp6.vec$vec28), length=100)
mnvec16 <- rep(mean(me.modEmp6.vec$vec16), length=100)
mnvec17 <- rep(mean(me.modEmp6.vec$vec17), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
mnvec16.17.28 <- c(mnvec16, mnvec17, mnvec28)
mnme.modEmp6.fit <- matrix(mnvec16.17.28, ncol = 3, byrow = FALSE)
newyFish_man <- predict(sp.modEmp6, 
                        newdata = list(Fish_man = newFish_man,
                                       numPrimLivFarm = mnnumPrimLivFarm,
                                       Cloth_craft = mnCloth_craft,
                                       me.modEmp6.fit = mnme.modEmp6.fit),
                        type = "link", se = TRUE)

par(mfrow=c(1,1))
plot(dat_working$Fish_man, dat_working$perc_change_bin, xlab="Families whose primary livelihood is fishing", 
     ylab = "Probability of forest loss")
lines(newFish_man,ilogit(newyFish_man$fit),lwd=2)
lines(newFish_man,ilogit(newyFish_man$fit+1.96*newyFish_man$se.fit),lty=3)
lines(newFish_man,ilogit(newyFish_man$fit-1.96*newyFish_man$se.fit),lty=3)


# Cloth_craft
newCloth_craft <- seq(0,0.9, length=100)
mnFish_man <- rep(mean(dat_working$Fish_man), length=100) 
mnnumPrimLivFarm <- rep(mean(dat_working$numPrimLivFarm), length=100)
mnvec28 <- rep(mean(me.modEmp6.vec$vec28), length=100)
mnvec16 <- rep(mean(me.modEmp6.vec$vec16), length=100)
mnvec17 <- rep(mean(me.modEmp6.vec$vec17), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
mnvec16.17.28 <- c(mnvec16, mnvec17, mnvec28)
mnme.modEmp6.fit <- matrix(mnvec16.17.28, ncol = 3, byrow = FALSE)
newyCloth_craft <- predict(sp.modEmp6, 
                        newdata = list(Cloth_craft = newCloth_craft,
                                       Fish_man = mnFish_man,
                                       numPrimLivFarm = mnnumPrimLivFarm,
                                       me.modEmp6.fit = mnme.modEmp6.fit),
                        type = "link", se = TRUE)

par(mfrow=c(1,1))
plot(dat_working$Cloth_craft, dat_working$perc_change_bin, xlab="Families whose primary livelihood is textiles", 
     ylab = "Probability of forest loss")
lines(newCloth_craft,ilogit(newyCloth_craft$fit),lwd=2)
lines(newCloth_craft,ilogit(newyCloth_craft$fit+1.96*newyCloth_craft$se.fit),lty=3)
lines(newCloth_craft,ilogit(newyCloth_craft$fit-1.96*newyCloth_craft$se.fit),lty=3)


# Plot with ggplot
numPrimLivFarm_pred <- data.frame(newx = newnumPrimLivFarm,
                       newy = ilogit(newynumPrimLivFarm$fit),
                       upr = ilogit(newynumPrimLivFarm$fit+1.96*newynumPrimLivFarm$se.fit),
                       lwr = ilogit(newynumPrimLivFarm$fit-1.96*newynumPrimLivFarm$se.fit))

p_numPrimLivFarm <- ggplot(numPrimLivFarm_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=numPrimLivFarm, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="No. families whose main livelihood is farming", y="")+
              theme(plot.margin = unit(c(0,0,1.5,0), "cm"))

fish_man_pred <- data.frame(newx = newFish_man,
                       newy = ilogit(newyFish_man$fit),
                       upr = ilogit(newyFish_man$fit+1.96*newyFish_man$se.fit),
                       lwr = ilogit(newyFish_man$fit-1.96*newyFish_man$se.fit))

p_fish_man <- ggplot(fish_man_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=Fish_man, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Proportion of illiterate adult males", y="")+
              theme(plot.margin = unit(c(1.5,0,0,0), "cm"))

Cloth_craft_pred <- data.frame(newx = newCloth_craft,
                       newy = ilogit(newyCloth_craft$fit),
                       upr = ilogit(newyCloth_craft$fit+1.96*newyCloth_craft$se.fit),
                       lwr = ilogit(newyCloth_craft$fit-1.96*newyCloth_craft$se.fit))

p_Cloth_craft <- ggplot(Cloth_craft_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=Cloth_craft, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Proportion of illiterate adult males", y="")+
              theme(plot.margin = unit(c(1.5,0,0,0), "cm"))


demog_plots <- grid.arrange(
                arrangeGrob(p_M6_24_sch,p_M18_60_ill, ncol=1, 
                  left = textGrob("Probability of forest loss", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2ppt(file="education_plots", width=10, height=10)

#
## Economic security ####

# Plots
pairs(econSec)
par(mfrow=c(2,3))
plot(dat_working$Les1_R_Land, dat_working$perc_change_bin)
plot(dat_working$No_R_Land, dat_working$perc_change_bin)
plot(dat_working$Les1_F_Land, dat_working$perc_change_bin)
plot(dat_working$No_F_Land, dat_working$perc_change_bin)
plot(dat_working$pig_fam, dat_working$perc_change_bin)

## non-spatial model
glm.modEcon1 <- glm(perc_change_bin ~ Les1_R_Land+No_R_Land+Les1_F_Land+No_F_Land+pig_fam,
                    data = dat_working, family = binomial)

summary(glm.modEcon1)

## Spatial model 1

# Get eigenvectors
me.modEcon1 <- ME(perc_change_bin ~ Les1_R_Land+No_R_Land+Les1_F_Land+No_F_Land+pig_fam, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modEcon1 <- glm(perc_change_bin ~ Les1_R_Land+No_R_Land+Les1_F_Land+No_F_Land+pig_fam + 
                     fitted(me.modEcon1), 
              data=dat_working, family=binomial)

summary(sp.modEcon1)
anova(sp.modEcon1, glm.modEcon1, test = "Chisq")

## Spatial model 2 - simplify - exclude No_F_Land 

# get eigenvectors 
me.modEcon2 <- ME(perc_change_bin ~ Les1_R_Land+No_R_Land+Les1_F_Land+pig_fam, 
              data=dat_working, family=binomial, listw = ME.listw)

sp.modEcon2 <- glm(perc_change_bin ~ Les1_R_Land+No_R_Land+Les1_F_Land+pig_fam + 
                     fitted(me.modEcon2), 
              data=dat_working, family=binomial)

summary(sp.modEcon2)
anova(sp.modEcon1,sp.modEcon2)
# Simplified model is better

# Spatial model 3 - simplify - exclude pig_fam
me.modEcon3 <- ME(perc_change_bin ~ Les1_R_Land+No_R_Land+Les1_F_Land, 
              data=dat_working, family=binomial, listw = ME.listw)
me.modEcon3.fit <- fitted(me.modEcon3)
me.modEcon3.vec <- me.modEcon3$vectors

# identifying outlier
dat_working %>% 
  filter(No_R_Land>0.6)

sp.modEcon3 <- glm(perc_change_bin ~ Les1_R_Land+No_R_Land+Les1_F_Land + me.modEcon3.fit, 
              data=dat_working, family=binomial)

summary(sp.modEcon3)
anova(sp.modEcon2,sp.modEcon3, test = "Chisq")
# Simplified model is better

## predictions and plots

# Les1_R_Land
newLes1_R_Land <- seq(0,0.932,length=100)
mnNo_R_Land <- rep(mean(dat_working$No_R_Land), length=100)
mnLes1_F_Land <- rep(mean(dat_working$Les1_F_Land), length=100)
#mnme.modEcon3 <- rep(mean(me.modEcon3.fit), length=100)
mnvec16 <- rep(mean(me.modEcon3.vec$vec16), length=100)
mnvec28 <- rep(mean(me.modEcon3.vec$vec28), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
mnvec16.28 <- c(mnvec16, mnvec28)
mnme.modEcon3 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newyLes1_R_Land <- predict(sp.modEcon3, newdata = list(Les1_R_Land = newLes1_R_Land,
                                                       No_R_Land = mnNo_R_Land,
                                                       Les1_F_Land = mnLes1_F_Land,
                                                       me.modEcon3.fit = mnme.modEcon3),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$Les1_R_Land, dat_working$perc_change_bin, xlab="Families with less than 1ha rice land", 
     ylab = "Probability of forest loss")
lines(newLes1_R_Land,ilogit(newyLes1_R_Land$fit),lwd=2)
lines(newLes1_R_Land,ilogit(newyLes1_R_Land$fit+1.96*newyLes1_R_Land$se.fit),lty=3)
lines(newLes1_R_Land,ilogit(newyLes1_R_Land$fit-1.96*newyLes1_R_Land$se.fit),lty=3)


# No_R_Land
newNo_R_Land <- seq(0,0.68,length=100)
mnLes1_R_Land <- rep(mean(dat_working$Les1_R_Land), length=100)
mnLes1_F_Land <- rep(mean(dat_working$Les1_F_Land), length=100)
#mnme.modEcon3 <- rep(mean(me.modEcon3.fit), length=100)
mnvec16 <- rep(mean(me.modEcon3.vec$vec16), length=100)
mnvec28 <- rep(mean(me.modEcon3.vec$vec28), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
mnvec16.28 <- c(mnvec16, mnvec28)
mnme.modEcon3 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newyNo_R_Land <- predict(sp.modEcon3, newdata = list(Les1_R_Land = mnLes1_R_Land,
                                                       No_R_Land = newNo_R_Land,
                                                       Les1_F_Land = mnLes1_F_Land,
                                                       me.modEcon3.fit = mnme.modEcon3),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$No_R_Land, dat_working$perc_change_bin, xlab="Families with no rice land", 
     ylab = "Probability of forest loss")
lines(newNo_R_Land,ilogit(newyNo_R_Land$fit),lwd=2)
lines(newNo_R_Land,ilogit(newyNo_R_Land$fit+1.96*newyNo_R_Land$se.fit),lty=3)
lines(newNo_R_Land,ilogit(newyNo_R_Land$fit-1.96*newyNo_R_Land$se.fit),lty=3)

## ggplot

Les1_R_land_pred <- data.frame(newx = newLes1_R_Land,
                       newy = ilogit(newyLes1_R_Land$fit),
                       upr = ilogit(newyLes1_R_Land$fit+1.96*newyLes1_R_Land$se.fit),
                       lwr = ilogit(newyLes1_R_Land$fit-1.96*newyLes1_R_Land$se.fit))

p_Les1_R_land <- ggplot(Les1_R_land_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=Les1_R_Land, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Proportion of families with less than 1ha rice land", y="")+
              theme(plot.margin = unit(c(0,0,1.5,0), "cm"))

No_R_land_pred <- data.frame(newx = newNo_R_Land,
                       newy = ilogit(newyNo_R_Land$fit),
                       upr = ilogit(newyNo_R_Land$fit+1.96*newyNo_R_Land$se.fit),
                       lwr = ilogit(newyNo_R_Land$fit-1.96*newyNo_R_Land$se.fit))

p_No_R_land <- ggplot(No_R_land_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=No_R_Land, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Proportion of families with no rice land", y="")+
              theme(plot.margin = unit(c(1.5,0,0,0), "cm"))

demog_plots <- grid.arrange(
                arrangeGrob(p_Les1_R_land,p_No_R_land, ncol=1, 
                  left = textGrob("Probability of forest loss", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2ppt(file="EconSec_plots", width=8, height=8)



# Les1_F_Land
newLes1_F_Land <- seq(0,0.74,length=100)
mnLes1_R_Land <- rep(mean(dat_working$Les1_R_Land), length=100)
mnNo_R_Land <- rep(mean(dat_working$No_R_Land), length=100)
#mnme.modEcon3 <- rep(mean(me.modEcon3.fit), length=100)
mnvec16 <- rep(mean(me.modEcon3.vec$vec16), length=100)
mnvec28 <- rep(mean(me.modEcon3.vec$vec28), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
mnvec16.28 <- c(mnvec16, mnvec28)
mnme.modEcon3 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newyLes1_F_Land <- predict(sp.modEcon3, newdata = list(Les1_R_Land = mnLes1_R_Land,
                                                       No_R_Land = mnNo_R_Land,
                                                       Les1_F_Land = newLes1_F_Land,
                                                       me.modEcon3.fit = mnme.modEcon3),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$Les1_F_Land, dat_working$perc_change_bin, xlab="Families with less than 1ha farming land", 
     ylab = "Probability of forest loss")
lines(newLes1_F_Land,ilogit(newyLes1_F_Land$fit),lwd=2)
lines(newLes1_F_Land,ilogit(newyLes1_F_Land$fit+1.96*newyLes1_F_Land$se.fit),lty=3)
lines(newLes1_F_Land,ilogit(newyLes1_F_Land$fit-1.96*newyLes1_F_Land$se.fit),lty=3)

## Access to services ####

# plots
pairs(service)
par(mfrow=c(2,3))
plot(dat_working$dist_sch, dat_working$perc_change_bin)
plot(dat_working$garbage, dat_working$perc_change_bin)
plot(dat_working$KM_Market, dat_working$perc_change_bin)
plot(dat_working$KM_Comm, dat_working$perc_change_bin)
plot(dat_working$wat_safe, dat_working$perc_change_bin)
plot(dat_working$wat_pipe, dat_working$perc_change_bin)

## non-spatial model
glm.modSer1 <- glm(perc_change_bin ~ dist_sch+garbage+KM_Market+KM_Comm+wat_safe+wat_pipe,
                   data = dat_working, family = binomial)
summary(glm.modSer1)

## Spatial model 1

# Get eigenvectors
me.modSer1 <- ME(perc_change_bin ~ dist_sch+garbage+KM_Market+KM_Comm+wat_safe+wat_pipe,
                 data = dat_working, family = binomial, listw = ME.listw, alpha = 0.1)

sp.modSer1 <- glm(perc_change_bin ~ dist_sch+garbage+KM_Market+KM_Comm+wat_safe+wat_pipe+
                    fitted(me.modSer1),
                   data = dat_working, family = binomial)
summary(sp.modSer1)
anova(glm.modSer1,sp.modSer1, test="Chisq")

# Spatial model 2 - simplify - exclude dist_sch

# Get eigenvectors
me.modSer2 <- ME(perc_change_bin ~ garbage+KM_Market+KM_Comm+wat_safe+wat_pipe,
                 data = dat_working, family = binomial, listw = ME.listw, alpha = 0.05)

sp.modSer2 <- glm(perc_change_bin ~ garbage+KM_Market+KM_Comm+wat_safe+wat_pipe+
                    fitted(me.modSer2),
                   data = dat_working, family = binomial)

summary(sp.modSer2)
anova(sp.modSer1, sp.modSer2, test="Chisq")

# Spatial model 3 - simplify - exclude dist_sch + garbage

# Get eigenvectors
me.modSer3 <- ME(perc_change_bin ~ KM_Market+KM_Comm+wat_safe+wat_pipe,
                 data = dat_working, family = binomial, listw = ME.listw)

sp.modSer3 <- glm(perc_change_bin ~ KM_Market+KM_Comm+wat_safe+wat_pipe+
                    fitted(me.modSer3),
                   data = dat_working, family = binomial)

summary(sp.modSer3)
anova(sp.modSer1, sp.modSer3, test="Chisq")
# Simpler model is better

# Spatial model 4 - simplify - exclude dist_sch + garbage + wat_safe + wat_pipe

# Get eigenvectors
me.modSer4 <- ME(perc_change_bin ~ KM_Market+KM_Comm,
                 data = dat_working, family = binomial, listw = ME.listw, alpha = 0.05)

sp.modSer4 <- glm(perc_change_bin ~ KM_Market+KM_Comm+fitted(me.modSer4),
                   data = dat_working, family = binomial)

summary(sp.modSer4)
anova(sp.modSer3, sp.modSer4, test="Chisq")
# model 3 is better - just

# Spatial model 5 - simplify - exclude dist_sch + garbage + wat_pipe

# Get eigenvectors
me.modSer5 <- ME(perc_change_bin ~ KM_Market+KM_Comm+wat_safe,
                 data = dat_working, family = binomial, listw = ME.listw)
me.modSer5.fit <- fitted(me.modSer5)

sp.modSer5 <- glm(perc_change_bin ~ KM_Market+KM_Comm+wat_safe+as.vector(me.modSer5.fit),
                   data = dat_working, family = binomial)

summary(sp.modSer5)
anova(sp.modSer3, sp.modSer5, test="Chisq")
# Spatial model 3 is apparently the best but I don't think I need wat_pipe and wat_safe. So for now I am going to use spatial model 5

## Predictions and plotting

# KM_Market
newKM_Market <- seq(0,154,length=100)
mnKM_Comm <- rep(mean(dat_working$KM_Comm), length=100)
mnwat_safe <- rep(mean(dat_working$wat_safe), length=100)
mnme.modSer5 <- rep(mean(me.modSer5.fit), length=100)
#mnvec16 <- rep(mean(me.modEcon3.vec$vec16), length=100)
#mnvec28 <- rep(mean(me.modEcon3.vec$vec28), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
#mnvec16.28 <- c(mnvec16, mnvec28)
#mnme.modEcon3 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newyKM_Market <- predict(sp.modSer5, newdata = list(KM_Market = newKM_Market,
                                                       KM_Comm = mnKM_Comm,
                                                       wat_safe = mnwat_safe,
                                                       me.modSer5.fit = mnme.modSer5),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$KM_Market, dat_working$perc_change_bin, xlab="Distance (KM) from nearest market", 
     ylab = "Probability of forest loss")
lines(newKM_Market,ilogit(newyKM_Market$fit),lwd=2)
lines(newKM_Market,ilogit(newyKM_Market$fit+1.96*newyKM_Market$se.fit),lty=3)
lines(newKM_Market,ilogit(newyKM_Market$fit-1.96*newyKM_Market$se.fit),lty=3)


# KM_Comm
newKM_Comm <- seq(0,28,length=100)
mnKM_Market <- rep(mean(dat_working$KM_Market), length=100)
mnwat_safe <- rep(mean(dat_working$wat_safe), length=100)
mnme.modSer5 <- rep(mean(me.modSer5.fit), length=100)
#mnvec16 <- rep(mean(me.modEcon3.vec$vec16), length=100)
#mnvec28 <- rep(mean(me.modEcon3.vec$vec28), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
#mnvec16.28 <- c(mnvec16, mnvec28)
#mnme.modEcon3 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newyKM_Comm <- predict(sp.modSer5, newdata = list(KM_Market = mnKM_Market,
                                                       KM_Comm = newKM_Comm,
                                                       wat_safe = mnwat_safe,
                                                       me.modSer5.fit = mnme.modSer5),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$KM_Comm, dat_working$perc_change_bin, xlab="Distance (KM) from Commune office", 
     ylab = "Probability of forest loss")
lines(newKM_Comm,ilogit(newyKM_Comm$fit),lwd=2)
lines(newKM_Comm,ilogit(newyKM_Comm$fit+1.96*newyKM_Comm$se.fit),lty=3)
lines(newKM_Comm,ilogit(newyKM_Comm$fit-1.96*newyKM_Comm$se.fit),lty=3)


# wat_safe
newwat_safe <- seq(0,1,length=100)
mnKM_Market <- rep(mean(dat_working$KM_Market), length=100)
mnKM_Comm <- rep(mean(dat_working$wat_safe), length=100)
mnme.modSer5 <- rep(mean(me.modSer5.fit), length=100)
#mnvec16 <- rep(mean(me.modEcon3.vec$vec16), length=100)
#mnvec28 <- rep(mean(me.modEcon3.vec$vec28), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
#mnvec16.28 <- c(mnvec16, mnvec28)
#mnme.modEcon3 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newywat_safe <- predict(sp.modSer5, newdata = list(KM_Market = mnKM_Market,
                                                       KM_Comm = mnKM_Comm,
                                                       wat_safe = newwat_safe,
                                                       me.modSer5.fit = mnme.modSer5),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$wat_safe, dat_working$perc_change_bin, xlab="Proportion families with access to safe water", 
     ylab = "Probability of forest loss")
lines(newwat_safe,ilogit(newywat_safe$fit),lwd=2)
lines(newwat_safe,ilogit(newywat_safe$fit+1.96*newywat_safe$se.fit),lty=3)
lines(newwat_safe,ilogit(newywat_safe$fit-1.96*newywat_safe$se.fit),lty=3)


## ggplot

KM_comm_pred <- data.frame(newx = newKM_Comm,
                       newy = ilogit(newyKM_Comm$fit),
                       upr = ilogit(newyKM_Comm$fit+1.96*newyKM_Comm$se.fit),
                       lwr = ilogit(newyKM_Comm$fit-1.96*newyKM_Comm$se.fit))

p_KM_comm <- ggplot(KM_comm_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=KM_Comm, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Distance to Commune capital", y="")+
              theme(plot.margin = unit(c(0,0,1.5,0), "cm"))

KM_Market_pred <- data.frame(newx = newKM_Market,
                       newy = ilogit(newyKM_Market$fit),
                       upr = ilogit(newyKM_Market$fit+1.96*newyKM_Market$se.fit),
                       lwr = ilogit(newyKM_Market$fit-1.96*newyKM_Market$se.fit))

p_KM_Market <- ggplot(KM_Market_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=KM_Market, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Distance to nearest market", y="")+
              theme(plot.margin = unit(c(1.5,0,0,0), "cm"))

demog_plots <- grid.arrange(
                arrangeGrob(p_KM_comm,p_KM_Market, ncol=1, 
                  left = textGrob("Probability of forest loss", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2ppt(file="serv_plots", width=8, height=8)

## Social justice ####

# Plots 
pairs(justice)
par(mfrow=c(1,2))
plot(dat_working$land_confl, dat_working$perc_change_bin)
plot(dat_working$crim_case, dat_working$perc_change_bin)

# non-spatial model
glm.modJus1 <- glm(perc_change_bin ~ crim_case+land_confl, data=dat_working, family = binomial)
summary(glm.modJus1)

## spatial model 1

# get eigenvectors
me.modJus1 <- ME(perc_change_bin ~ crim_case+land_confl, data=dat_working, family=binomial, 
                listw = ME.listw)

sp.modJus1 <- glm(perc_change_bin ~ crim_case+land_confl+fitted(me.modJus1), 
                  data=dat_working, family=binomial)

summary(sp.modJus1)
# Something strange going on with crim_Case

## spatial model 2 - remove crim_case

# get eigenvectors
me.modJus2 <- ME(perc_change_bin ~ land_confl, data=dat_working, family=binomial, 
                listw = ME.listw)
me.modJus2.fit <- fitted(me.modJus2)

sp.modJus2 <- glm(perc_change_bin ~ land_confl+as.vector(me.modJus2.fit), 
                  data=dat_working, family=binomial)
summary(sp.modJus2)

## predictions and plotting

# land_confl
newland_confl <- seq(0,179,length=100)
mnme.modJus2 <- rep(mean(me.modJus2.fit), length=100)
#mnvec16 <- rep(mean(me.modEcon3.vec$vec16), length=100)
#mnvec28 <- rep(mean(me.modEcon3.vec$vec28), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
#mnvec16.28 <- c(mnvec16, mnvec28)
#mnme.modEcon3 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newyland_confl <- predict(sp.modJus2, newdata = list(land_confl = newland_confl,
                                                       me.modJus2.fit = mnme.modJus2),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$land_confl, dat_working$perc_change_bin, xlab="Land conflic cases", 
     ylab = "Probability of forest loss")
lines(newland_confl,ilogit(newyland_confl$fit),lwd=2)
lines(newland_confl,ilogit(newyland_confl$fit+1.96*newyland_confl$se.fit),lty=3)
lines(newland_confl,ilogit(newyland_confl$fit-1.96*newyland_confl$se.fit),lty=3)


# ggplot

KM_Market_pred <- data.frame(newx = newKM_Market,
                       newy = ilogit(newyKM_Market$fit),
                       upr = ilogit(newyKM_Market$fit+1.96*newyKM_Market$se.fit),
                       lwr = ilogit(newyKM_Market$fit-1.96*newyKM_Market$se.fit))

p_KM_Market <- ggplot(KM_Market_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=KM_Market, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Distance to nearest market", y="")+
              theme(plot.margin = unit(c(1.5,0,0,0), "cm"))

demog_plots <- grid.arrange(
                arrangeGrob(p_KM_comm,p_KM_Market, ncol=1, 
                  left = textGrob("Probability of forest loss", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2ppt(file="serv_plots", width=8, height=8)

## Health ####

# plots
pairs(health)
par(mfrow=c(3,1))
plot(dat_working$KM_Heal_cent, dat_working$perc_change_bin)
plot(dat_working$inf_mort, dat_working$perc_change_bin)
plot(dat_working$U5_mort, dat_working$perc_change_bin)

# non-spatial model
glm.modHeal1 <- glm(perc_change_bin ~ KM_Heal_cent+inf_mort+U5_mort, 
                    data=dat_working, family=binomial)
summary(glm.modHeal1)

## spatial model 1

# eigenvectors
me.modHeal1 <- ME(perc_change_bin ~ KM_Heal_cent+inf_mort+U5_mort, 
                  data=dat_working, family=binomial, listw = ME.listw)

sp.modHeal1 <- glm(perc_change_bin ~ KM_Heal_cent+inf_mort+U5_mort+fitted(me.modHeal1), 
                    data=dat_working, family=binomial)
summary(sp.modHeal1)
# inf_mort and U5_mort are ridiculous. I will take them out

## spatial model 2 - remove inf_mort and U5_mort

# eigenvectors
me.modHeal2 <- ME(perc_change_bin ~ KM_Heal_cent, 
                  data=dat_working, family=binomial, listw = ME.listw)
me.modHeal2.fit <- fitted(me.modHeal2)
me.modHeal2.vec <- me.modHeal2$vectors
me.modHeal2.vec <- data.frame(me.modHeal2.vec)

sp.modHeal2 <- glm(perc_change_bin ~ KM_Heal_cent+me.modHeal2.fit, 
                    data=dat_working, family=binomial)
summary(sp.modHeal2)
# I will use this model

## predictions and plotting

# KM_Heal_cent
newKM_Heal_cent <- seq(0,90,length=100)
#mnme.modJus2 <- rep(mean(me.modJus2.fit), length=100)
mnvec16 <- rep(mean(me.modHeal2.vec$vec16), length=100)
mnvec28 <- rep(mean(me.modHeal2.vec$vec28), length=100)
# Need to put the eigenvector variables into a 2-column matrix as that's what went into the model
mnvec16.28 <- c(mnvec16, mnvec28)
mnme.modHeal2 <- matrix(mnvec16.28, ncol = 2, byrow = FALSE)
newyKM_Heal_cent <- predict(sp.modHeal2, newdata = list(KM_Heal_cent = newKM_Heal_cent,
                                                       me.modHeal2.fit = mnme.modHeal2),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$KM_Heal_cent, dat_working$perc_change_bin, xlab="Distance to health centre", 
     ylab = "Probability of forest loss")
lines(newKM_Heal_cent,ilogit(newyKM_Heal_cent$fit),lwd=2)
lines(newKM_Heal_cent,ilogit(newyKM_Heal_cent$fit+1.96*newyKM_Heal_cent$se.fit),lty=3)
lines(newKM_Heal_cent,ilogit(newyKM_Heal_cent$fit-1.96*newyKM_Heal_cent$se.fit),lty=3)

# ggplot 

KM_Heal_cent_pred <- data.frame(newx = newKM_Heal_cent,
                       newy = ilogit(newyKM_Heal_cent$fit),
                       upr = ilogit(newyKM_Heal_cent$fit+1.96*newyKM_Heal_cent$se.fit),
                       lwr = ilogit(newyKM_Heal_cent$fit-1.96*newyKM_Heal_cent$se.fit))

p_KM_Heal_cent <- ggplot(KM_Heal_cent_pred, aes(x=newx, y=newy))+
              geom_line(size=1, color="#000099")+
              geom_point(data=dat_working, 
                         aes(x=KM_Heal_cent, y=perc_change_bin), shape=1, size=1)+
              ylim(0,1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.25,fill="#000099")+
              labs(x="Distance to nearest health centre", y="Probability of forest loss")
              



graph2ppt(file="health_plots", width=8, height=8)

## Migration ####

# plots
pairs(migrate)
plot(dat_working$Pax_migt_in, dat_working$perc_change_bin)
plot(dat_working$Pax_migt_out, dat_working$perc_change_bin)

# non-spatial model
glm.modMig1 <- glm(perc_change_bin ~ Pax_migt_in+Pax_migt_out,
                   data=dat_working, family=binomial)
summary(glm.modMig1)

## spatial model 1

# eigenvectors
me.modMig1 <- ME(perc_change_bin ~ Pax_migt_in+Pax_migt_out, 
                  data=dat_working, family=binomial, listw = ME.listw)

sp.modMig1 <- glm(perc_change_bin ~ Pax_migt_in+Pax_migt_out+fitted(me.modMig1),
                   data=dat_working, family=binomial)
summary(sp.modMig1)

## spatial model 2 - remove Pax_migt_out - because in-migration is more likely to cause forest loss

me.modMig2 <- ME(perc_change_bin ~ Pax_migt_in, 
                  data=dat_working, family=binomial, listw = ME.listw)
me.modMig2.fit <- fitted(me.modMig2)
me.modMig2.vec <- data.frame(me.modMig2.fit)

sp.modMig2 <- glm(perc_change_bin ~ Pax_migt_in+me.modMig2.fit,
                   data=dat_working, family=binomial)
summary(sp.modMig2)
# Pax-migt-in is not significant

## predictions and plotting

# Pax_migt_in
newPax_migt_in <- seq(0,4018,length=100)
#mnme.modJus2 <- rep(mean(me.modJus2.fit), length=100)
mnvec2 <- rep(mean(me.modMig2.vec$vec2), length=100)
mnvec28 <- rep(mean(me.modMig2.vec$vec28), length=100)
mnvec39 <- rep(mean(me.modMig2.vec$vec39), length=100)
# Need to put the eigenvector variables into a 3-column matrix as that's what went into the model
mnvec2.28.39 <- c(mnvec2,mnvec28,mnvec39)
mnme.modMig2 <- matrix(mnvec2.28.39, ncol = 3, byrow = FALSE)
newyPax_migt_in <- predict(sp.modMig2, newdata = list(Pax_migt_in = newPax_migt_in,
                                                       me.modMig2.fit = mnme.modMig2),
                           type="link", se=TRUE)

par(mfrow=c(1,1))
plot(dat_working$Pax_migt_in, dat_working$perc_change_bin, xlab="Number of in-migrants", 
     ylab = "Probability of forest loss")
lines(newPax_migt_in,ilogit(newyPax_migt_in$fit),lwd=2)
lines(newPax_migt_in,ilogit(newyPax_migt_in$fit+1.96*newyPax_migt_in$se.fit),lty=3)
lines(newPax_migt_in,ilogit(newyPax_migt_in$fit-1.96*newyPax_migt_in$se.fit),lty=3)
