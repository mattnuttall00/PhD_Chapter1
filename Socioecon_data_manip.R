
library('robustbase')
library('xlsx')
library('xlsReadWrite')
library('rJava')
library('openxlsx')
library('tidyverse')

dat <- read.csv("Socioeconomic_variables.csv")
commDat <- read.csv("commGIS.csv")

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


dt <- data.frame(PV=dat2[which(lgth>1),'Province'],CM=dat2[which(lgth>1),'Commune'])
dt



## Edit below code to select by communeGIS not commune
# To aggregate at the commune level, we can do another loop

commGIS <- unique(dat2$CommCode)    # Unique commune IDs
commGIS <- commGIS[-which(is.na(commGIS)==T)]
dat3 <- dat2[1,]        # Create template for final data frame columns
for (c in 1:length(commGIS)){      # For each commune ID
  sub <- dat2[dat2$CommCode==commGIS[c],]   # Subset the data frame
  for (cc in 1:ncol(sub)){                                            # For each column of sub
    if (cc %in% c(5:9,14,16,17,36,41,42)){   # If column number is in this list of numbers...
      dat3[c,cc] <- sum(sub[cc],na.rm=T)           # Do the column sum
    }
    if (cc %in% c(10:13,15,18:28,30:35,37:40,43)){  # If the column number is in this list of numbers...
      dat3[c,cc] <- mean(sub[cc],na.rm=T)                   # Do the column mean
    }
    if (cc %in% 29) {
        dat3[c,cc] <- median(sub[cc] [sub[cc]>0],na.rm=T)     # Do the median
    } 
  }
}

dat3 <- dat3[,-4]    # Remove column 4 as it should be empty (no villages)
write.xlsx(dat3, "C://Users/mnn1/Box Sync/Objective 1/Analysis/Data/dat3.xlsx", sheetName="Sheet 1", 
           col.names = T)


# tidyverse
as.tibble(dat2)

dat3 <- dat2 %>% 
   
  select(CommCode,tot_pop,family,male_18_60,fem_18_60,pop_over61,numPrimLivFarm,Fish_man,ntfp_fam,
         land_confl,Pax_migt_in,Pax_migt_out) %>%  
  group_by(CommCode) %>%
  summarise_all(funs(sum)) %>% 

dat4 <- dat2 %>% 
  select(CommCode,F6_24_sch,M6_24_sch,F18_60_ill,M18_60_ill,propPrimLivFarm,fam_prod,Cloth_craft
         ,Trader,serv_prov,T18_60_uncjob,Les1_R_Land,No_R_Land,Les1_F_Land,No_F_Land,cow_fam,
         pig_fam, garbage,KM_Market,KM_Comm,YR_Pp_well,wat_safe,wat_pipe,crim_case,
         KM_Heal_cent,inf_mort,U5_mort,Prop_Indigenous) %>% 
  group_by(CommCode) %>%
  summarise_all(funs(mean)) %>% 

dat5 <- dat2 %>% 
  select(CommCode, dist_sch) %>% 
  group_by(CommCode) %>% 
  summarise_all(funs(median))
                
dat6 <- left_join(dat3,dat4,by = "CommCode")
dat7 <- left_join(dat6, dat5, by = "CommCode")

admindat <- dat2 %>% 
  select(CommCode,Province, Commune) %>% 
  group_by(CommCode)

dat_master <- left_join(admindat, dat7, by = "CommCode")

### master
dat3 <- dat2 %>% 
  
  group_by(CommCode) %>% 
  select(tot_pop,family,male_18_60,fem_18_60,pop_over61,numPrimLivFarm,Fish_man,ntfp_fam,
         land_confl,Pax_migt_in,Pax_migt_out) %>%  
 
  summarise_all(funs(sum)) %>% 
  
  select(CommCode,F6_24_sch,M6_24_sch,F18_60_ill,M18_60_ill,propPrimLivFarm,fam_prod,Cloth_craft
         ,Trader,serv_prov,T18_60_uncjob,Les1_R_Land,No_R_Land,Les1_F_Land,No_F_Land,cow_fam,
         pig_fam, garbage,KM_Market,KM_Comm,YR_Pp_well,wat_safe,wat_pipe,crim_case,
         KM_Heal_cent,inf_mort,U5_mort,Prop_Indigenous) %>% 
  group_by(CommCode) %>%
  summarise_all(funs(mean)) %>% 

  select(CommCode, dist_sch) %>% 
  group_by(CommCode) %>% 
  summarise_all(funs(median))



