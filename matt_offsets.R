### Matt data - comparing offsets

library(lme4)

dat <- read.csv("dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

### tidyverse - just say no, kids!
# why reinvent the wheel? why install a whole army of packages, most of which you'll never need? also, IT'S SO UGLY!
# (also, sorry - joking aside, I don't have tidyverse installed and I can't be bothered - sorry)

# scale all vars
# dat1 <- dat %>%
#   mutate_at(c("year","tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec","propSecSec","Les1_R_Land",
#               "pig_fam","dist_sch","garbage","KM_Comm","land_confl","crim_case","Pax_migt_in",
#               "Pax_migt_out","mean_elev","dist_border","dist_provCap"), ~(scale(.) %>% as.vector))

# This does the exact same thing as above but just using base R:
dat1 = dat
vrs = c("year","tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec","propSecSec","Les1_R_Land",
        "pig_fam","dist_sch","garbage","KM_Comm","land_confl","crim_case","Pax_migt_in",
        "Pax_migt_out","mean_elev","dist_border","dist_provCap"); dat1[,vrs] = apply(dat1[,vrs],2,scale)

# merge Province and Commune into a new unique variable (to remove issue of communes with the same name)
#dat1 <- dat1 %>% mutate(Provcomm = paste(dat1$Province, dat1$Commune, sep = "_"))
dat1$Provcomm = paste(dat1$Province, dat1$Commune, sep = "_") # Oh look base-R is even shorter in syntax!


# model 1, offset as argument
popdem.m1 <- glmer(ForPix ~ tot_pop + pop_den + prop_ind + (year|Province/Provcomm),
                   offset = log(areaKM), data=dat1, family="poisson")

# model 2, offset as term
popdem.m2 <- glmer(ForPix ~ tot_pop + pop_den + prop_ind + offset(log(areaKM)) +
                     (year|Province/Provcomm),
                   data=dat1, family="poisson")

# compare predictions from observed data only
pred_popdem.m1 <- predict(popdem.m1, type = "response")
pred_popdem.m2 <- predict(popdem.m2, type = "response")
plot(pred_popdem.m1,pred_popdem.m2)
head(pred_popdem.m1)
head(pred_popdem.m2)
### Spot on so yes these are (and should be!) the same - because we are dealing with fully conditional predictions here,
### I think.

# create new data for model with offset as argument (i.e. no offset in dataframe)
tot_pop_newdat <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                             pop_den = mean(dat1$pop_den),
                             prop_ind = mean(dat1$prop_ind))

# create new data for model with offset as term (offset required in dataframe)
tot_pop_newdat_off <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                                 pop_den = mean(dat1$pop_den),
                                 prop_ind = mean(dat1$prop_ind),
                                 areaKM = mean(dat1$areaKM))

pred_popdem.m1 <- predict(popdem.m1, newdata=tot_pop_newdat, type="response", re.form=NA)
pred_popdem.m2 <- predict(popdem.m2, newdata=tot_pop_newdat_off, type="response", re.form=NA)

# Although the two models predict relatively the right values, 
plot(pred_popdem.m1,pred_popdem.m2)
# They are each on a massively different scale:
head(pred_popdem.m1)
head(pred_popdem.m2)
# This is because m2 predicts values for a "mean" offset, i.e. an "average area", m1 by defaults predict ForPix per unit area.
# You can ensure they are exactly the same by setting the areaKM in the second prediction frame to 1 (unit area):
tot_pop_newdat_off <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                                 pop_den = mean(dat1$pop_den),
                                 prop_ind = mean(dat1$prop_ind),
                                 areaKM = 1)  ### <------
pred_popdem.m2 <- predict(popdem.m2, newdata=tot_pop_newdat_off, type="response", re.form=NA)
head(pred_popdem.m1)
head(pred_popdem.m2)
# Now what you are predicting is the relationship between ForPix PER KM^2 and tot_pop, for an "average" commune, given
# mean values for pop_den and prop_ind.
# Previously, m2 was predicting the relationship between ForPix per AVERAGE KM^2, and the predictors.


