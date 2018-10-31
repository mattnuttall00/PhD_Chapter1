#### Load libraries and data ####

library('cowplot')
library('tidyverse')

dat <- read.csv("Socioeconomic_variables.csv")
commDat <- read.csv("commGIS.csv")


## Grouping and summarising indigenous group variables ####
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


## Adding unique Commune Codes ####
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



## Aggregating all variables up to the Commune level ####

# tidyverse
as.tibble(dat2)

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
tp1 <- qplot(battambang$tot_pop, geom = "histogram")
tp2 <- qplot(banteay_meanchey$tot_pop, geom = "histogram")
tp3 <- qplot(kampong_speu$tot_pop, geom = "histogram")
tp4 <- qplot(kep$tot_pop, geom = "histogram")
tp5 <- qplot(otdar_meanchey$tot_pop, geom = "histogram")
tp6 <- qplot(preah_vihear$tot_pop, geom = "histogram")
tp7 <- qplot(siem_reap$tot_pop, geom = "histogram")
tp8 <- qplot(kampong_thom$tot_pop, geom = "histogram")
tp9 <- qplot(koh_kong$tot_pop, geom = "histogram")
tp10 <- qplot(pailin$tot_pop, geom = "histogram")
tp11 <- qplot(prey_veng$tot_pop, geom = "histogram")
tp12 <- qplot(stung_treng$tot_pop, geom = "histogram")
tp13 <- qplot(kampong_cham$tot_pop, geom = "histogram")
tp14 <- qplot(kampot$tot_pop, geom = "histogram")
tp15 <- qplot(kratie$tot_pop, geom = "histogram")
tp16 <- qplot(phnom_penh$tot_pop, geom = "histogram")
tp17 <- qplot(pursat$tot_pop, geom = "histogram")
tp18 <- qplot(svay_rieng$tot_pop, geom = "histogram")
tp19 <- qplot(kampong_chhnang$tot_pop, geom = "histogram")
tp20 <- qplot(kandal$tot_pop, geom = "histogram")
tp21 <- qplot(mondulkiri$tot_pop, geom = "histogram")
tp22 <- qplot(preah_sihanouk$tot_pop, geom = "histogram")
tp23 <- qplot(ratanakiri$tot_pop, geom = "histogram")
tp24 <- qplot(takeo$tot_pop, geom = "histogram")
            
plot_grid(tp1,tp2,tp3,tp4,tp5,tp6,tp7,tp8,tp9,tp10,tp11,tp12,tp13,tp14,tp15,tp16,tp17,tp18,tp19,tp20,tp21,tp22,tp23,
          tp24)


## Family ####
qplot(dat_master$family, geom = "histogram")

f1 <- qplot(battambang$family, geom = "histogram")
f2 <- qplot(banteay_meanchey$family, geom = "histogram")
f3 <- qplot(kampong_speu$family, geom = "histogram")
f4 <- qplot(kep$family, geom = "histogram")
f5 <- qplot(otdar_meanchey$family, geom = "histogram")
f6 <- qplot(preah_vihear$family, geom = "histogram")
f7 <- qplot(siem_reap$family, geom = "histogram")
f8 <- qplot(kampong_thom$family, geom = "histogram")
f9 <- qplot(koh_kong$family, geom = "histogram")
f10 <- qplot(pailin$family, geom = "histogram")
f11 <- qplot(prey_veng$family, geom = "histogram")
f12 <- qplot(stung_treng$family, geom = "histogram")
f13 <- qplot(kampong_cham$family, geom = "histogram")
f14 <- qplot(kampot$family, geom = "histogram")
f15 <- qplot(kratie$family, geom = "histogram")
f16 <- qplot(phnom_penh$family, geom = "histogram")
f17 <- qplot(pursat$family, geom = "histogram")
f18 <- qplot(svay_rieng$family, geom = "histogram")
f19 <- qplot(kampong_chhnang$family, geom = "histogram")
f20 <- qplot(kandal$family, geom = "histogram")
f21 <- qplot(mondulkiri$family, geom = "histogram")
f22 <- qplot(preah_sihanouk$family, geom = "histogram")
f23 <- qplot(ratanakiri$family, geom = "histogram")
f24 <- qplot(takeo$family, geom = "histogram")

plot_grid(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,
          f24)

## male_18_60 ####

qplot(dat_master$male_18_60, geom = "histogram")

p1 <- qplot(battambang$male_18_60, geom = "histogram")
p2 <- qplot(banteay_meanchey$male_18_60, geom = "histogram")
p3 <- qplot(kampong_speu$male_18_60, geom = "histogram")
p4 <- qplot(kep$male_18_60, geom = "histogram")
p5 <- qplot(otdar_meanchey$male_18_60, geom = "histogram")
p6 <- qplot(preah_vihear$male_18_60, geom = "histogram")
p7 <- qplot(siem_reap$male_18_60, geom = "histogram")
p8 <- qplot(kampong_thom$male_18_60, geom = "histogram")
p9 <- qplot(koh_kong$male_18_60, geom = "histogram")
p10 <- qplot(pailin$male_18_60, geom = "histogram")
p11 <- qplot(prey_veng$male_18_60, geom = "histogram")
p12 <- qplot(stung_treng$male_18_60, geom = "histogram")
p13 <- qplot(kampong_cham$male_18_60, geom = "histogram")
p14 <- qplot(kampot$male_18_60, geom = "histogram")
p15 <- qplot(kratie$male_18_60, geom = "histogram")
p16 <- qplot(phnom_penh$male_18_60, geom = "histogram")
p17 <- qplot(pursat$male_18_60, geom = "histogram")
p18 <- qplot(svay_rieng$male_18_60, geom = "histogram")
p19 <- qplot(kampong_chhnang$male_18_60, geom = "histogram")
p20 <- qplot(kandal$male_18_60, geom = "histogram")
p21 <- qplot(mondulkiri$male_18_60, geom = "histogram")
p22 <- qplot(preah_sihanouk$male_18_60, geom = "histogram")
p23 <- qplot(ratanakiri$male_18_60, geom = "histogram")
p24 <- qplot(takeo$male_18_60, geom = "histogram")

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,
          p24)

# One value is very high - Phnom Penh. This makes sense when you look at tot_pop. Don't see any problems
qplot(phnom_penh$male_18_60, geom = "histogram")

## fem_18_60 ####
qplot(dat_master$fem_18_60, geom = "histogram")

# Looks like there is an outlier again
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., fem_18_60 > 40000)
# Phnom Penh and Paoy Pet again

p1 <- qplot(battambang$fem_18_60, geom = "histogram")
p2 <- qplot(banteay_meanchey$fem_18_60, geom = "histogram")
p3 <- qplot(kampong_speu$fem_18_60, geom = "histogram")
p4 <- qplot(kep$fem_18_60, geom = "histogram")
p5 <- qplot(otdar_meanchey$fem_18_60, geom = "histogram")
p6 <- qplot(preah_vihear$fem_18_60, geom = "histogram")
p7 <- qplot(siem_reap$fem_18_60, geom = "histogram")
p8 <- qplot(kampong_thom$fem_18_60, geom = "histogram")
p9 <- qplot(koh_kong$fem_18_60, geom = "histogram")
p10 <- qplot(pailin$fem_18_60, geom = "histogram")
p11 <- qplot(prey_veng$fem_18_60, geom = "histogram")
p12 <- qplot(stung_treng$fem_18_60, geom = "histogram")
p13 <- qplot(kampong_cham$fem_18_60, geom = "histogram")
p14 <- qplot(kampot$fem_18_60, geom = "histogram")
p15 <- qplot(kratie$fem_18_60, geom = "histogram")
p16 <- qplot(phnom_penh$fem_18_60, geom = "histogram")
p17 <- qplot(pursat$fem_18_60, geom = "histogram")
p18 <- qplot(svay_rieng$fem_18_60, geom = "histogram")
p19 <- qplot(kampong_chhnang$fem_18_60, geom = "histogram")
p20 <- qplot(kandal$fem_18_60, geom = "histogram")
p21 <- qplot(mondulkiri$fem_18_60, geom = "histogram")
p22 <- qplot(preah_sihanouk$fem_18_60, geom = "histogram")
p23 <- qplot(ratanakiri$fem_18_60, geom = "histogram")
p24 <- qplot(takeo$fem_18_60, geom = "histogram")

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,
          p24)

## pop_over61 ####
qplot(dat_master$pop_over61, geom = "histogram")

p1 <- qplot(battambang$pop_over61, geom = "histogram")
p2 <- qplot(banteay_meanchey$pop_over61, geom = "histogram")
p3 <- qplot(kampong_speu$pop_over61, geom = "histogram")
p4 <- qplot(kep$pop_over61, geom = "histogram")
p5 <- qplot(otdar_meanchey$pop_over61, geom = "histogram")
p6 <- qplot(preah_vihear$pop_over61, geom = "histogram")
p7 <- qplot(siem_reap$pop_over61, geom = "histogram")
p8 <- qplot(kampong_thom$pop_over61, geom = "histogram")
p9 <- qplot(koh_kong$pop_over61, geom = "histogram")
p10 <- qplot(pailin$pop_over61, geom = "histogram")
p11 <- qplot(prey_veng$pop_over61, geom = "histogram")
p12 <- qplot(stung_treng$pop_over61, geom = "histogram")
p13 <- qplot(kampong_cham$pop_over61, geom = "histogram")
p14 <- qplot(kampot$pop_over61, geom = "histogram")
p15 <- qplot(kratie$pop_over61, geom = "histogram")
p16 <- qplot(phnom_penh$pop_over61, geom = "histogram")
p17 <- qplot(pursat$pop_over61, geom = "histogram")
p18 <- qplot(svay_rieng$pop_over61, geom = "histogram")
p19 <- qplot(kampong_chhnang$pop_over61, geom = "histogram")
p20 <- qplot(kandal$pop_over61, geom = "histogram")
p21 <- qplot(mondulkiri$pop_over61, geom = "histogram")
p22 <- qplot(preah_sihanouk$pop_over61, geom = "histogram")
p23 <- qplot(ratanakiri$pop_over61, geom = "histogram")
p24 <- qplot(takeo$pop_over61, geom = "histogram")

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,
          p24)

## numPrimLivFarm ####
qplot(dat_master$numPrimLivFarm, geom = "histogram")

# There are a few outliers here, and I'm curious as to where in the country they are. I'm guessing the central lowlands, where rice farming is huge
dat_master %>% 
  group_by(CommCode) %>% 
  filter(., numPrimLivFarm > 4000)

p1 <- qplot(battambang$numPrimLivFarm, geom = "histogram")
p2 <- qplot(banteay_meanchey$numPrimLivFarm, geom = "histogram")
p3 <- qplot(kampong_speu$numPrimLivFarm, geom = "histogram")
p4 <- qplot(kep$numPrimLivFarm, geom = "histogram")
p5 <- qplot(otdar_meanchey$numPrimLivFarm, geom = "histogram")
p6 <- qplot(preah_vihear$numPrimLivFarm, geom = "histogram")
p7 <- qplot(siem_reap$numPrimLivFarm, geom = "histogram")
p8 <- qplot(kampong_thom$numPrimLivFarm, geom = "histogram")
p9 <- qplot(koh_kong$numPrimLivFarm, geom = "histogram")
p10 <- qplot(pailin$numPrimLivFarm, geom = "histogram")
p11 <- qplot(prey_veng$numPrimLivFarm, geom = "histogram")
p12 <- qplot(stung_treng$numPrimLivFarm, geom = "histogram")
p13 <- qplot(kampong_cham$numPrimLivFarm, geom = "histogram")
p14 <- qplot(kampot$numPrimLivFarm, geom = "histogram")
p15 <- qplot(kratie$numPrimLivFarm, geom = "histogram")
p16 <- qplot(phnom_penh$numPrimLivFarm, geom = "histogram")
p17 <- qplot(pursat$numPrimLivFarm, geom = "histogram")
p18 <- qplot(svay_rieng$numPrimLivFarm, geom = "histogram")
p19 <- qplot(kampong_chhnang$numPrimLivFarm, geom = "histogram")
p20 <- qplot(kandal$numPrimLivFarm, geom = "histogram")
p21 <- qplot(mondulkiri$numPrimLivFarm, geom = "histogram")
p22 <- qplot(preah_sihanouk$numPrimLivFarm, geom = "histogram")
p23 <- qplot(ratanakiri$numPrimLivFarm, geom = "histogram")
p24 <- qplot(takeo$numPrimLivFarm, geom = "histogram")

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,
          p24)



histplot <- function(variable) {
  p1 <- qplot(battambang$variable, geom = "histogram")
  p2 <- qplot(banteay_meanchey$variable, geom = "histogram")
  p3 <- qplot(kampong_speu$variable, geom = "histogram")
  p4 <- qplot(kep$variable, geom = "histogram")
  p5 <- qplot(otdar_meanchey$variable, geom = "histogram")
  p6 <- qplot(preah_vihear$variable, geom = "histogram")
  p7 <- qplot(siem_reap$variable, geom = "histogram")
  p8 <- qplot(kampong_thom$variable, geom = "histogram")
  p9 <- qplot(koh_kong$variable, geom = "histogram")
  p10 <- qplot(pailin$variable, geom = "histogram")
 q <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
 print(q)
}

histplot(variable = dat_master$numPrimLivFarm)

plotfunc <- function(x){
  x <- qplot(battambang$numPrimLivFarm, geom = "histogram")
  print(x)
}
 
plotfunc(x) 
