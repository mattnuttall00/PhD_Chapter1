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

# I have found the commune Trapeang Chour in GIS, but my village GIS layer doesnt have the same number of villages as in the data.  But based on the size of the commune, there are defnitely villages that have erroneous values for KM_Comm.  

## Dealing with KM_Market variable ####

# Identifying all potential problem values for KM_Market
dist_market <- dat2 %>%
                filter(KM_Market > 50) %>% 
                select(KM_Market, Province, Commune, Village)
str(dist_market)
hist(dist_market$KM_Market)

# Over 800 km are Khpob ateav and Preaek Ambel (Kandal).  
# 550km is Baray1 (Kampong Cham)
dist_market %>% filter(KM_Market > 500)
# 154km is Kampong Sralau Muoy (Preah Vihear)
# 161km is Kampong Sralau Pir (Preah Vihear)
# 159km is Kampong Sralau Pir (Preah Vihear) but diff village
dist_market %>% filter(KM_Market > 150)

dist_market %>% filter(KM_Market > 80) %>% filter(KM_Market < 150)
# The Communes which I will need to find means (minus the outliers) are: 
# Battambang > Hab
# Kampong Cham > Tonlung
# Kampong Speu > Ta Sal
# Kampong Thom > Sochet
# Koh Kong > Pralay
# Koh Kong > Chumnoab
# Koh Kong > Chi Phat
# Kracheh > 
dat2 %>% filter(Province=="Prey Veng", Commune=="Preaek Krabau")

# testing the replace function
dist_market %>% 
  mutate(KM_Market = replace(KM_Market, KM_Market > 800, 5.137493))


# Replace erroneous KM_Market values for Khpob ateav and Preaek Ambel communes.  See "KM_Market" section below for more details.  Mean KM_Market in Kandal Province (excl. incorrect values) = 5.137493
dat2 %>% 
  filter(Province == "Kandal") %>% 
  filter(KM_Market < 50) %>% 
  summarise(mean = mean(KM_Market))

dat2 <- dat2 %>% 
          filter(Province=="Kandal") %>% 
          mutate(KM_Market = ifelse(KM_Market > 50, 5.137493, KM_Market))

# Below was just to test the above code
#kandal.test <- test %>% filter(Province=="Kandal")  
#hist(kandal.test$KM_Market)
#kandal.test %>% 
  #select(KM_Market) %>% 
  #filter(KM_Market > 50)

# Replace final erroneous KM_Market value for K'am Samnar with the mean, as above
dat2 <- dat2 %>% 
          filter(Province=="Kandal") %>% 
          mutate(KM_Market = ifelse(KM_Market >=35, 5.137493, KM_Market))

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
qplot(dat_master$tot_pop, geom = "histogram")

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
qplot(dat_master$M6_24_sch, geom = "histogram")

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


#----- Below is the exploration before I removed the outliers #----

# Some big distances which I find hard to believe
dat_master %>% 
  group_by(CommCode) %>% 
  filter(KM_Market > 37) %>% 
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

#----- Below is after I removed the three outliers #----

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

## Need to check some more outliers
# Battambang 
dat_master %>% 
  filter((Province=="Battambang")) %>% 
  filter(KM_Comm > 20) %>% 
  print(width=Inf)
# There are two communes with much higher values than the rest of the Province. These have now been dealt with in the data cleaning section at the top

# Kampong Cham
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

# Kampong Chhnang
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

# Kampong Speu
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
