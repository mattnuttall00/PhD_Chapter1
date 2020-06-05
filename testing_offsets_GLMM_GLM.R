### testing offsets


# load data
dat <- read.csv("Data/commune/dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

# scale vars (ok ok, I'll use base...)
dat1 <- dat
vrs <- c("year","tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec","propSecSec","Les1_R_Land",
        "pig_fam","dist_sch","garbage","KM_Comm","land_confl","crim_case","Pax_migt_in",
        "Pax_migt_out","mean_elev","dist_border","dist_provCap")
dat1[,vrs] = apply(dat1[,vrs],2,scale)

# create unique commune name
dat1$Provcomm = paste(dat1$Province, dat1$Commune, sep = "_")

# get mean ForPix value for each commune over the years (I'm sorry, I don't know how to do this is base...! Well, I could probably work it out but it would take me longer ;) )
meanForPix <- dat1 %>% group_by(Provcomm) %>% summarise(meanForPix = mean(ForPix))
dat1 <- left_join(dat1, meanForPix, by="Provcomm")

# model 1. GLMM with offset as areaKM, set as an argument 
m1 <- glmer(ForPix ~ tot_pop + pop_den + prop_ind + (year|Province/Provcomm),
                   offset = log(areaKM), data=dat1, family="poisson")

# model 2. 