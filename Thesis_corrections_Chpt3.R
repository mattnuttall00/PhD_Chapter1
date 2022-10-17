### This script is to conduct the corrections from my viva. The corrections relate to Chapter 3. Marcus Rowcliffe said that I should not have used the count of forest pixels as the response because pixels are an arbitrary unit. Therefore, I could have chosen to change the size of the pixels and that would change the response, and therefore the results. Marcus says I need to use proportion of forest cover with a beta distribution. 


#### Libraries and Data ####
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(cAIC4)
library(sjPlot)
library(ggeffects)

# I can use Data/commune/dat1.csv for this analyis. This is the master dataset that I used for the mixed models. This data set has the area of each commune, plus the total number of forested pixels. Therefore, I can easily calculate the proportion of forest per commune. 

# load in the data (already centered and scaled)
dat <- read.csv("Data/commune/dat1.csv", header=TRUE, stringsAsFactors = TRUE)

# At the moment, the forested area is in pixels, where 1 pixel = 0.09 km2. The area is in km2. Therefore, I need to create a new variable that is the forested area in km2, and then create a proportion variable.
dat$forest_area <- dat$ForPix * 0.09
dat$forest_prop <- dat$forest_area / dat$areaKM

# look at histogram of the new variable
hist(dat$forest_prop)

# As expected, there are mostly zeros. The glmmTMB package supports zero-inflated beta models, which is what I think I need. 

# There are some rows (68) that have a proportion greater than 1. I will change them to 1

dat$forest_prop <- ifelse(dat$forest_prop > 1, 1, dat$forest_prop)

# Turns out that having 1's causes errors. Following advice (from here: https://github.com/glmmTMB/glmmTMB/issues/770) I will change all 1's to 0.999's
dat$forest_prop <- ifelse(dat$forest_prop==1, 0.999, dat$forest_prop)

# Change elc and PA to factor
dat$elc <- as.factor(dat$elc)
dat$PA  <- as.factor(dat$PA)



#### COMMUNE LEVEL MODELS ####

# I will need to follow my modeling approach that I used previously. I ran maximal within group models, and terms with no effect were dropped. If there is only one variable in a group then it is automatically taken forward. 

    ## Population demographics ####

popdem.m1 <- glmmTMB(forest_prop ~ pop_den + prop_ind + offset(log(areaKM)) + (year|Province/Provcomm),
                     data=dat, ziformula = ~1, family = beta_family())




popdem.m2 <- glmmTMB(forest_prop ~ pop_den + offset(log(areaKM)) + (year|Province/Provcomm),
                     data=dat, ziformula = ~1, family = beta_family())
# can ignore this warning


# anova
anova(popdem.m2, popdem.m1, test="Chisq")
# simpler model is better

## SAME AS BEFORE


    ## Education ####

# Only one education variable so it gets taken forward

    ## Employment ####

emp.m1 <- glmmTMB(forest_prop ~ propPrimSec + propSecSec + offset(log(areaKM)) + (year|Province/Provcomm),
                               data=dat, ziformula = ~1, family = beta_family())

emp.m2 <- glmmTMB(forest_prop ~ propPrimSec + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

anova(emp.m1, emp.m2)

# simpler model is better.

### SAME AS BEFORE


    ## Economic security ####

econ.m1 <- glmmTMB(forest_prop ~ Les1_R_Land + pig_fam + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

econ.m2 <- glmmTMB(forest_prop ~ pig_fam + offset(log(areaKM)) + (year|Province/Provcomm),
                   data=dat, ziformula = ~1, family = beta_family())


anova(econ.m1, econ.m2)
# simpler model is better


### SAME AS BEFORE


    ## Access to services ####

acc.m1 <- glmmTMB(forest_prop ~ dist_sch + garbage + KM_Comm + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

acc.m2 <- glmmTMB(forest_prop ~ dist_sch + garbage + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

acc.m3 <- glmmTMB(forest_prop ~ garbage + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

anova(acc.m1,acc.m2)
# simpler model is best


anova(acc.m2,acc.m3)
# simpler model is best

## TAKE dist_sch 7 Garbage FORWARD - AS BEFORE


    ## Social justice ####

soc.m1 <- glmmTMB(forest_prop ~  crim_case + land_confl + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

soc.m2 <- glmmTMB(forest_prop ~  crim_case + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())


anova(soc.m1,soc.m2)
# simpler model is better

### SAME AS BEFORE


    ## Migration ####

mig.m1 <- glmmTMB(forest_prop ~  Pax_migt_in*Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

mig.m2 <- glmmTMB(forest_prop ~  Pax_migt_in + Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

mig.m3 <- glmmTMB(forest_prop ~  Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

mig.m4 <- glmmTMB(forest_prop ~  Pax_migt_in + offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

anova(mig.m1,mig.m2)
# simpler model is better


anova(mig.m2,mig.m3)
# simpler model is better

AICc(mig.m3)
AICc(mig.m4)

# pax_migt_out taken forward SAME AS BEFORE


    ## Environment vars ####

env.m1 <- glmmTMB(forest_prop ~ mean_elev + offset(log(areaKM)) + (year|Province/Provcomm),
                data=dat, ziformula = ~1, family = beta_family())

### only one var so taken forward SAME AS BEFORE

    ## Human additional vars ####


hum.m1 <- glmmTMB(forest_prop ~ dist_border+dist_provCap+elc+PA+PA_cat+offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())


hum.m2 <- glmmTMB(forest_prop ~ dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
                  data=dat, ziformula = ~1, family = beta_family())

anova(hum.m1,hum.m2)
# simpler model is better

# will take dist_provCap, dist_border, PA, and elc forward, SAME AS BEFORE



  #### FINAL COMMUNE MODELS ####


# Global mode for diagnostics
m0 <- glmmTMB(forest_prop ~ pop_den + M6_24_sch + propPrimSec + pig_fam + dist_sch + garbage + crim_case + Pax_migt_out +  
                mean_elev + dist_border + dist_provCap + elc + PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())


### DHARMa diagnostics

# USing DHARMa package for diagnostics - first calculate residuals using all RE levels
simulationOutput <- simulateResiduals(fittedModel = m0, plot = T, 
                                      re.form = ~(year|Province/Provcomm))

plot(simulationOutput)
# These both look good. QQ plot says maybe the wrong distribution, but whatever

# QQ plot
plotQQunif(simulationOutput)
# as above

# Another test for dispersion
testDispersion(simulationOutput)
# this looks pretty good to me

# residual vs predcted plot
plotResiduals(simulationOutput)
# this to me shows fairly major heteroskedasicity - much more residual variance for small values of predicted ForPix.  I was hoping that the removal of prop_ind was going to make this look better!
hist(simulationOutput)

# plot residuals from individual predictors
par(mfrow=c(3,5))
plotResiduals(simulationOutput, dat$propPrimSec)
plotResiduals(simulationOutput, dat$mean_elev)
plotResiduals(simulationOutput, dat$dist_border)
plotResiduals(simulationOutput, dat$dist_provCap)
plotResiduals(simulationOutput, dat$elc)
plotResiduals(simulationOutput, dat$PA)
plotResiduals(simulationOutput, dat$Pax_migt_out)
plotResiduals(simulationOutput, dat$pop_den)
plotResiduals(simulationOutput, dat$garbage)
plotResiduals(simulationOutput, dat$M6_24_sch)
plotResiduals(simulationOutput, dat$pig_fam)
plotResiduals(simulationOutput, dat$crim_case)
plotResiduals(simulationOutput, dat$dist_sch)
## I think that most of these are good, except garbage, which looks a bit odd. 


m1 <- glmmTMB(forest_prop ~ pop_den+mean_elev+dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
        data=dat, ziformula = ~1, family = beta_family())

m2 <- glmmTMB(forest_prop ~ M6_24_sch + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m3 <- glmmTMB(forest_prop ~ propPrimSec + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m4 <- glmmTMB(forest_prop ~  pig_fam + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m5 <- glmmTMB(forest_prop ~  dist_sch + garbage + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m6 <- glmmTMB(forest_prop ~  crim_case + garbage + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m7 <- glmmTMB(forest_prop ~ Pax_migt_out + garbage + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m8 <- glmmTMB(forest_prop ~ M6_24_sch + dist_sch + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m9 <- glmmTMB(forest_prop ~ propPrimSec + Pax_migt_out + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

m10 <- glmmTMB(forest_prop ~ pop_den + M6_24_sch + propPrimSec + pig_fam + dist_sch + garbage + crim_case + 
                 Pax_migt_out + mean_elev + dist_border+dist_provCap+elc+PA+offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat, ziformula = ~1, family = beta_family())

modSel.commune <- model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
# Models 1:7, and 9, all have some support, so I will need to model average.


## Model averaging
modSel.commune.sub <- modSel.commune[modSel.commune$delta<4]

m.avg.commune <- model.avg(modSel.commune.sub, fit = TRUE)




    ## Predictions ####


# variables that I need to include in the newdata for predictions are:
# pop_den, M6_24_sch, propPrimSec, pig_fam, dist_sch, garbage, crim_case, Pax_migt_out, mean_elev, dist_border, dist_provCap, elc, PA, offset(log(areaKM))


## Create new data

# pop_den
pop_den_newdat <- data.frame(pop_den = seq(min(dat$pop_den), max(dat$pop_den), length=300),
                              M6_24_sch = mean(dat$M6_24_sch),
                              propPrimSec = mean(dat$propPrimSec),
                              pig_fam = mean(dat$pig_fam),
                              dist_sch = mean(dat$dist_sch),
                              garbage = mean(dat$garbage),
                              crim_case = mean(dat$crim_case),
                              Pax_migt_out = mean(dat$Pax_migt_out),
                              mean_elev = mean(dat$mean_elev),
                              dist_border = mean(dat$dist_border),
                              dist_provCap = mean(dat$dist_provCap),
                              elc = as.factor(0),
                              PA = as.factor(0),
                              areaKM = median(dat$areaKM),
                              year=NA,
                              Province=NA,
                              Provcomm=NA)

# M6_24_sch
M6_24_sch_newdat <- data.frame(M6_24_sch = seq(min(dat$M6_24_sch), max(dat$M6_24_sch), length=100),
                              pop_den = mean(dat$pop_den),
                              propPrimSec = mean(dat$propPrimSec),
                              pig_fam = mean(dat$pig_fam),
                              dist_sch = mean(dat$dist_sch),
                              garbage = mean(dat$garbage),
                              crim_case = mean(dat$crim_case),
                              Pax_migt_out = mean(dat$Pax_migt_out),
                              mean_elev = mean(dat$mean_elev),
                              dist_border = mean(dat$dist_border),
                              dist_provCap = mean(dat$dist_provCap),
                              elc = as.factor(0),
                              PA = as.factor(0),
                              areaKM = median(dat$areaKM),
                              year=NA,
                              Province=NA,
                              Provcomm=NA)


# propPrimSec
propPrimSec_newdat <- expand.grid(propPrimSec = seq(min(dat$propPrimSec), max(dat$propPrimSec), length=100),
                                pop_den = mean(dat$pop_den),
                                M6_24_sch = mean(dat$M6_24_sch),
                                pig_fam = mean(dat$pig_fam),
                                dist_sch = mean(dat$dist_sch),
                                garbage = mean(dat$garbage),
                                crim_case = mean(dat$crim_case),
                                Pax_migt_out = mean(dat$Pax_migt_out),
                                mean_elev = mean(dat$mean_elev),
                                dist_border = mean(dat$dist_border),
                                dist_provCap = mean(dat$dist_provCap),
                                elc = as.factor(0),
                                PA = as.factor(0),
                                areaKM = median(dat$areaKM),
                                year=NA,
                                Province=NA,
                                Provcomm=NA)

# pig_fam
pig_fam_newdat <- expand.grid(pig_fam = seq(min(dat$pig_fam), max(dat$pig_fam), length=100),
                                  pop_den = mean(dat$pop_den),
                                  M6_24_sch = mean(dat$M6_24_sch),
                                  propPrimSec = mean(dat$propPrimSec),
                                  dist_sch = mean(dat$dist_sch),
                                  garbage = mean(dat$garbage),
                                  crim_case = mean(dat$crim_case),
                                  Pax_migt_out = mean(dat$Pax_migt_out),
                                  mean_elev = mean(dat$mean_elev),
                                  dist_border = mean(dat$dist_border),
                                  dist_provCap = mean(dat$dist_provCap),
                                  elc = as.factor(0),
                                  PA = as.factor(0),
                                  areaKM = median(dat$areaKM),
                                  year=NA,
                                  Province=NA,
                                  Provcomm=NA)

# dist_sch
dist_sch_newdat <- expand.grid(dist_sch = seq(min(dat$dist_sch), max(dat$dist_sch), length=100),
                              pop_den = mean(dat$pop_den),
                              M6_24_sch = mean(dat$M6_24_sch),
                              propPrimSec = mean(dat$propPrimSec),
                              pig_fam = mean(dat$pig_fam),
                              garbage = mean(dat$garbage),
                              crim_case = mean(dat$crim_case),
                              Pax_migt_out = mean(dat$Pax_migt_out),
                              mean_elev = mean(dat$mean_elev),
                              dist_border = mean(dat$dist_border),
                              dist_provCap = mean(dat$dist_provCap),
                              elc = as.factor(0),
                              PA = as.factor(0),
                              areaKM = median(dat$areaKM),
                              year=NA,
                              Province=NA,
                              Provcomm=NA)

# garbage
garbage_newdat <- expand.grid(garbage = seq(min(dat$garbage), max(dat$garbage), length=100),
                               pop_den = mean(dat$pop_den),
                               M6_24_sch = mean(dat$M6_24_sch),
                               propPrimSec = mean(dat$propPrimSec),
                               pig_fam = mean(dat$pig_fam),
                               dist_sch = mean(dat$dist_sch),
                               crim_case = mean(dat$crim_case),
                               Pax_migt_out = mean(dat$Pax_migt_out),
                               mean_elev = mean(dat$mean_elev),
                               dist_border = mean(dat$dist_border),
                               dist_provCap = mean(dat$dist_provCap),
                               elc = as.factor(0),
                               PA = as.factor(0),
                               areaKM = median(dat$areaKM),
                               year=NA,
                               Province=NA,
                               Provcomm=NA)

# crim_case
crim_case_newdat <- expand.grid(crim_case = seq(min(dat$crim_case), max(dat$crim_case), length=100),
                              pop_den = mean(dat$pop_den),
                              M6_24_sch = mean(dat$M6_24_sch),
                              propPrimSec = mean(dat$propPrimSec),
                              pig_fam = mean(dat$pig_fam),
                              dist_sch = mean(dat$dist_sch),
                              garbage = mean(dat$garbage),
                              Pax_migt_out = mean(dat$Pax_migt_out),
                              mean_elev = mean(dat$mean_elev),
                              dist_border = mean(dat$dist_border),
                              dist_provCap = mean(dat$dist_provCap),
                              elc = as.factor(0),
                              PA = as.factor(0),
                              areaKM = median(dat$areaKM),
                              year=NA,
                              Province=NA,
                              Provcomm=NA)

# Pax_migt_out
Pax_migt_out_newdat <- expand.grid(Pax_migt_out = seq(min(dat$Pax_migt_out), max(dat$Pax_migt_out), length=100),
                                pop_den = mean(dat$pop_den),
                                M6_24_sch = mean(dat$M6_24_sch),
                                propPrimSec = mean(dat$propPrimSec),
                                pig_fam = mean(dat$pig_fam),
                                dist_sch = mean(dat$dist_sch),
                                garbage = mean(dat$garbage),
                                crim_case = mean(dat$crim_case),
                                mean_elev = mean(dat$mean_elev),
                                dist_border = mean(dat$dist_border),
                                dist_provCap = mean(dat$dist_provCap),
                                elc = as.factor(0),
                                PA = as.factor(0),
                                areaKM = median(dat$areaKM),
                                year=NA,
                                Province=NA,
                                Provcomm=NA)

# mean_elev
mean_elev_newdat <- expand.grid(mean_elev = seq(min(dat$mean_elev), max(dat$mean_elev), length=100),
                                   pop_den = mean(dat$pop_den),
                                   M6_24_sch = mean(dat$M6_24_sch),
                                   propPrimSec = mean(dat$propPrimSec),
                                   pig_fam = mean(dat$pig_fam),
                                   dist_sch = mean(dat$dist_sch),
                                   garbage = mean(dat$garbage),
                                   crim_case = mean(dat$crim_case),
                                   Pax_migt_out = mean(dat$Pax_migt_out),
                                   dist_border = mean(dat$dist_border),
                                   dist_provCap = mean(dat$dist_provCap),
                                   elc = as.factor(0),
                                   PA = as.factor(0),
                                   areaKM = median(dat$areaKM),
                                   year=NA,
                                   Province=NA,
                                   Provcomm=NA)

# dist_border
dist_border_newdat <- expand.grid(dist_border = seq(min(dat$dist_border), max(dat$dist_border), length=100),
                                pop_den = mean(dat$pop_den),
                                M6_24_sch = mean(dat$M6_24_sch),
                                propPrimSec = mean(dat$propPrimSec),
                                pig_fam = mean(dat$pig_fam),
                                dist_sch = mean(dat$dist_sch),
                                garbage = mean(dat$garbage),
                                crim_case = mean(dat$crim_case),
                                Pax_migt_out = mean(dat$Pax_migt_out),
                                mean_elev = mean(dat$mean_elev),
                                dist_provCap = mean(dat$dist_provCap),
                                elc = as.factor(0),
                                PA = as.factor(0),
                                areaKM = median(dat$areaKM),
                                year=NA,
                                Province=NA,
                                Provcomm=NA)

# dist_provCap
dist_provCap_newdat <- expand.grid(dist_provCap = seq(min(dat$dist_provCap), max(dat$dist_provCap), length=100),
                                  pop_den = mean(dat$pop_den),
                                  M6_24_sch = mean(dat$M6_24_sch),
                                  propPrimSec = mean(dat$propPrimSec),
                                  pig_fam = mean(dat$pig_fam),
                                  dist_sch = mean(dat$dist_sch),
                                  garbage = mean(dat$garbage),
                                  crim_case = mean(dat$crim_case),
                                  Pax_migt_out = mean(dat$Pax_migt_out),
                                  mean_elev = mean(dat$mean_elev),
                                  dist_border = mean(dat$dist_border),
                                  elc = as.factor(0),
                                  PA = as.factor(0),
                                  areaKM = median(dat$areaKM),
                                  year=NA,
                                  Province=NA,
                                  Provcomm=NA)

# elc
elc_newdat <- expand.grid(elc = as.factor(c(0, 1)),
                                   pop_den = mean(dat$pop_den),
                                   M6_24_sch = mean(dat$M6_24_sch),
                                   propPrimSec = mean(dat$propPrimSec),
                                   pig_fam = mean(dat$pig_fam),
                                   dist_sch = mean(dat$dist_sch),
                                   garbage = mean(dat$garbage),
                                   crim_case = mean(dat$crim_case),
                                   Pax_migt_out = mean(dat$Pax_migt_out),
                                   mean_elev = mean(dat$mean_elev),
                                   dist_border = mean(dat$dist_border),
                                   dist_provCap = mean(dat$dist_provCap),
                                   PA = as.factor(0),
                                   areaKM = median(dat$areaKM),
                                   year=NA,
                                   Province=NA,
                                   Provcomm=NA)

# PA
PA_newdat <- expand.grid(PA = as.factor(c(0, 1)),
                          pop_den = mean(dat$pop_den),
                          M6_24_sch = mean(dat$M6_24_sch),
                          propPrimSec = mean(dat$propPrimSec),
                          pig_fam = mean(dat$pig_fam),
                          dist_sch = mean(dat$dist_sch),
                          garbage = mean(dat$garbage),
                          crim_case = mean(dat$crim_case),
                          Pax_migt_out = mean(dat$Pax_migt_out),
                          mean_elev = mean(dat$mean_elev),
                          dist_border = mean(dat$dist_border),
                          dist_provCap = mean(dat$dist_provCap),
                          elc = as.factor(0),
                          areaKM = median(dat$areaKM),
                          year=NA,
                          Province=NA,
                          Provcomm=NA)




### Predictions

pop_den_pred <- as.data.frame(predict(m.avg.commune, newdata = pop_den_newdat, type = "response", se.fit = TRUE, re.form=NA, full=TRUE))
M6_24_sch_pred <- as.data.frame(predict(m.avg.commune, newdata = M6_24_sch_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
propPrimSec_pred <- as.data.frame(predict(m.avg.commune, newdata = propPrimSec_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
pig_fam_pred <- as.data.frame(predict(m.avg.commune, newdata = pig_fam_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
dist_sch_pred <- as.data.frame(predict(m.avg.commune, newdata = dist_sch_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
garbage_pred <- as.data.frame(predict(m.avg.commune, newdata = garbage_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
crim_case_pred <- as.data.frame(predict(m.avg.commune, newdata = crim_case_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
Pax_migt_out_pred <- as.data.frame(predict(m.avg.commune, newdata=Pax_migt_out_newdat, type="response", se.fit = TRUE, re.form=NA, full = TRUE))
mean_elev_pred <- as.data.frame(predict(m.avg.commune, newdata = mean_elev_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
dist_border_pred <- as.data.frame(predict(m.avg.commune, newdata = dist_border_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
dist_provCap_pred <- as.data.frame(predict(m.avg.commune, newdata=dist_provCap_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
elc_pred <- as.data.frame(predict(m.avg.commune, newdata =elc_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))
PA_pred <- as.data.frame(predict(m.avg.commune, newdata = PA_newdat, type = "response", se.fit = TRUE, re.form=NA, full = TRUE))

# attach newdat and pred together

pop_den_results <- cbind(pop_den_newdat, pop_den_pred)
M6_24_sch_results <- cbind(M6_24_sch_newdat, M6_24_sch_pred)
propPrimSec_results <- cbind(propPrimSec_newdat, propPrimSec_pred)
pig_fam_results <- cbind(pig_fam_newdat, pig_fam_pred)
dist_sch_results <- cbind(dist_sch_newdat, dist_sch_pred)
garbage_results <- cbind(garbage_newdat, garbage_pred)
crim_case_results <- cbind(crim_case_newdat, crim_case_pred)
Pax_migt_out_results <- cbind(Pax_migt_out_newdat, Pax_migt_out_pred)
mean_elev_results <- cbind(mean_elev_newdat, mean_elev_pred)
dist_border_results <- cbind(dist_border_newdat, dist_border_pred)
dist_provCap_results <- cbind(dist_provCap_newdat, dist_provCap_pred)
elc_results <- cbind(elc_newdat, elc_pred)
PA_results <- cbind(PA_newdat, PA_pred)


# Add 2 x SE as confidence intervals

pop_den_results$U_SE <- pop_den_results$fit + (2*pop_den_results$se.fit)
pop_den_results$L_SE <- pop_den_results$fit - (2*pop_den_results$se.fit)


M6_24_sch_results$U_SE <- M6_24_sch_results$fit + (2*M6_24_sch_results$se.fit)
M6_24_sch_results$L_SE <- M6_24_sch_results$fit - (2*M6_24_sch_results$se.fit)

propPrimSec_results$U_SE <- propPrimSec_results$fit + (2*propPrimSec_results$se.fit)
propPrimSec_results$L_SE <- propPrimSec_results$fit - (2*propPrimSec_results$se.fit)

pig_fam_results$U_SE <- pig_fam_results$fit + (2*pig_fam_results$se.fit)
pig_fam_results$L_SE <- pig_fam_results$fit - (2*pig_fam_results$se.fit)

dist_sch_results$U_SE <- dist_sch_results$fit + (2*dist_sch_results$se.fit)
dist_sch_results$L_SE <- dist_sch_results$fit - (2*dist_sch_results$se.fit)

garbage_results$U_SE <- garbage_results$fit + (2*garbage_results$se.fit)
garbage_results$L_SE <- garbage_results$fit - (2*garbage_results$se.fit)

crim_case_results$U_SE <- crim_case_results$fit + (2*crim_case_results$se.fit)
crim_case_results$L_SE <- crim_case_results$fit - (2*crim_case_results$se.fit)

Pax_migt_out_results$U_SE <- Pax_migt_out_results$fit + (2*Pax_migt_out_results$se.fit)
Pax_migt_out_results$L_SE <- Pax_migt_out_results$fit - (2*Pax_migt_out_results$se.fit)

mean_elev_results$U_SE <- mean_elev_results$fit + (2*mean_elev_results$se.fit)
mean_elev_results$L_SE <- mean_elev_results$fit - (2*mean_elev_results$se.fit)

dist_border_results$U_SE <- dist_border_results$fit + (2*dist_border_results$se.fit)
dist_border_results$L_SE <- dist_border_results$fit - (2*dist_border_results$se.fit)

dist_provCap_results$U_SE <- dist_provCap_results$fit + (2*dist_provCap_results$se.fit)
dist_provCap_results$L_SE <- dist_provCap_results$fit - (2*dist_provCap_results$se.fit)

elc_results$U_SE <- elc_results$fit + (2*elc_results$se.fit)
elc_results$L_SE <- elc_results$fit - (2*elc_results$se.fit)

PA_results$U_SE <- PA_results$fit + (2*PA_results$se.fit)
PA_results$L_SE <- PA_results$fit - (2*PA_results$se.fit)



## Save outputs
write.csv(pop_den_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/pop_den_results.csv")
write.csv(M6_24_sch_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/M6_24_sch_results.csv")
write.csv(propPrimSec_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/propPrimSec_results.csv")
write.csv(pig_fam_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/pig_fam_results.csv")
write.csv(dist_sch_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/dist_sch_results.csv")
write.csv(garbage_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/garbage_results.csv")
write.csv(crim_case_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/crim_case_results.csv")
write.csv(Pax_migt_out_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/Pax_migt_out_results.csv")
write.csv(mean_elev_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/mean_elev_results.csv")
write.csv(dist_border_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/dist_border_results.csv")
write.csv(dist_provCap_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/dist_provCap_results.csv")
write.csv(elc_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/elc_results.csv")
write.csv(PA_results, file="C:/Users/Matt Nuttall/Documents/PhD/PhD_Chapter1/CORRECTIONS/Results/PA_results.csv")



    ## Plotting ####


pop_den_results <- read.csv("CORRECTIONS/Results/pop_den_results.csv", header = TRUE, stringsAsFactors = TRUE)
M6_24_sch_results <- read.csv("CORRECTIONS/Results/M6_24_sch_results.csv", header = TRUE, stringsAsFactors = TRUE)
propPrimSec_results <- read.csv("CORRECTIONS/Results/propPrimSec_results.csv", header = TRUE, stringsAsFactors = TRUE)
pig_fam_results <- read.csv("CORRECTIONS/Results/pig_fam_results.csv", header = TRUE, stringsAsFactors = TRUE)
dist_sch_results <- read.csv("CORRECTIONS/Results/dist_sch_results.csv", header = TRUE, stringsAsFactors = TRUE)
garbage_results <- read.csv("CORRECTIONS/Results/garbage_results.csv", header = TRUE, stringsAsFactors = TRUE)
crim_case_results <- read.csv("CORRECTIONS/Results/crim_case_results.csv", header = TRUE, stringsAsFactors = TRUE)
Pax_migt_out_results <- read.csv("CORRECTIONS/Results/Pax_migt_out_results.csv", header = TRUE, stringsAsFactors = TRUE)
mean_elev_results <- read.csv("CORRECTIONS/Results/mean_elev_results.csv", header = TRUE, stringsAsFactors = TRUE)
dist_border_results <- read.csv("CORRECTIONS/Results/dist_border_results.csv", header = TRUE, stringsAsFactors = TRUE)
dist_provCap_results <- read.csv("CORRECTIONS/Results/dist_provCap_results.csv", header = TRUE, stringsAsFactors = TRUE)
elc_results <- read.csv("CORRECTIONS/Results/elc_results.csv", header = TRUE, stringsAsFactors = TRUE)
PA_results <- read.csv("CORRECTIONS/Results/PA_results.csv", header = TRUE, stringsAsFactors = TRUE)




## pop_den
pop_den_plot <- ggplot(pop_den_results, aes(x=pop_den, y=fit))+
                geom_point(data=dat, aes(x=pop_den, y=forest_prop))+
                geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                geom_line(size=2, color="red")+
                theme(panel.background = element_blank(),
                      axis.line = element_line(colour = "grey20"),
                      axis.title = element_text(size=17),
                      axis.text = element_text(size=15))+
                ylab("Predicted proportion of forest cover")+
                xlab("Population density")

## M6_24_sch_results
M6_24_sch_plot <- ggplot(M6_24_sch_results, aes(x=M6_24_sch, y=fit))+
                  geom_point(data=dat, aes(x=M6_24_sch, y=forest_prop))+
                  geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                  geom_line(size=2, color="red")+
                  theme(panel.background = element_blank(),
                        axis.line = element_line(colour = "grey20"),
                        axis.title = element_text(size=17),
                        axis.text = element_text(size=15))+
                  ylab("Predicted proportion of forest cover")+
                  xlab("Proportion males in school")
 
## propPrimSec
propPrimSec_plot <- ggplot(propPrimSec_results, aes(x=propPrimSec, y=fit))+
                    geom_point(data=dat, aes(x=propPrimSec, y=forest_prop))+
                    geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                    geom_line(size=2, color="red")+
                    theme(panel.background = element_blank(),
                          axis.line = element_line(colour = "grey20"),
                          axis.title = element_text(size=17),
                          axis.text = element_text(size=15))+
                    ylab("Predicted proportion of forest cover")+
                    xlab("Proportion adults in the primary sector")


## pig_fam
pig_fam_plot <- ggplot(pig_fam_results, aes(x=pig_fam, y=fit))+
                geom_point(data=dat, aes(x=pig_fam, y=forest_prop))+
                geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                geom_line(size=2, color="red")+
                theme(panel.background = element_blank(),
                      axis.line = element_line(colour = "grey20"),
                      axis.title = element_text(size=17),
                      axis.text = element_text(size=15))+
                ylab("Predicted proportion of forest cover")+
                xlab("Proportion families with pigs")


## dist_sch
dist_sch_plot <- ggplot(dist_sch_results, aes(x=dist_sch, y=fit))+
                geom_point(data=dat, aes(x=dist_sch, y=forest_prop))+
                geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                geom_line(size=2, color="red")+
                theme(panel.background = element_blank(),
                      axis.line = element_line(colour = "grey20"),
                      axis.title = element_text(size=17),
                      axis.text = element_text(size=15))+
                ylab("Predicted proportion of forest cover")+
                xlab("Median distance to nearest school (km)")


## garbage
garbage_plot <- ggplot(garbage_results, aes(x=garbage, y=fit))+
                geom_point(data=dat, aes(x=garbage, y=forest_prop))+
                geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                geom_line(size=2, color="red")+
                theme(panel.background = element_blank(),
                      axis.line = element_line(colour = "grey20"),
                      axis.title = element_text(size=17),
                      axis.text = element_text(size=15))+
                ylab("Predicted proportion of forest cover")+
                xlab("Proportion families with access to waste removal")



## crim_case
crim_case_plot <- ggplot(crim_case_results, aes(x=crim_case, y=fit))+
                  geom_point(data=dat, aes(x=crim_case, y=forest_prop))+
                  geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                  geom_line(size=2, color="red")+
                  theme(panel.background = element_blank(),
                        axis.line = element_line(colour = "grey20"),
                        axis.title = element_text(size=17),
                        axis.text = element_text(size=15))+
                  ylab("Predicted proportion of forest cover")+
                  xlab("Number of criminal cases")



## Pax_migt_out
Pax_migt_out_plot <- ggplot(Pax_migt_out_results, aes(x=Pax_migt_out, y=fit))+
                      geom_point(data=dat, aes(x=Pax_migt_out, y=forest_prop))+
                      geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                      geom_line(size=2, color="red")+
                      theme(panel.background = element_blank(),
                            axis.line = element_line(colour = "grey20"),
                            axis.title = element_text(size=17),
                            axis.text = element_text(size=15))+
                      ylab("Predicted proportion of forest cover")+
                      xlab("Number of out-migrants")


## mean_elev
mean_elev_plot <- ggplot(mean_elev_results, aes(x=mean_elev, y=fit))+
                  geom_point(data=dat, aes(x=mean_elev, y=forest_prop))+
                  geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                  geom_line(size=2, color="red")+
                  theme(panel.background = element_blank(),
                        axis.line = element_line(colour = "grey20"),
                        axis.title = element_text(size=17),
                        axis.text = element_text(size=15))+
                  ylab("Predicted proportion of forest cover")+
                  xlab("Mean elevation (masl)")



## dist_border
dist_border_plot <- ggplot(dist_border_results, aes(x=dist_border, y=fit))+
                    geom_point(data=dat, aes(x=dist_border, y=forest_prop))+
                    geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                    geom_line(size=2, color="red")+
                    theme(panel.background = element_blank(),
                          axis.line = element_line(colour = "grey20"),
                          axis.title = element_text(size=17),
                          axis.text = element_text(size=15))+
                    ylab("Predicted proportion of forest cover")+
                    xlab("Median distance to nearest international border (km)")



## dist_provCap
dist_provCap_plot <- ggplot(dist_provCap_results, aes(x=dist_provCap, y=fit))+
                      geom_point(data=dat, aes(x=dist_provCap, y=forest_prop))+
                      geom_ribbon(aes(ymin=L_SE, ymax=U_SE), alpha=0.3)+
                      geom_line(size=2, color="red")+
                      theme(panel.background = element_blank(),
                            axis.line = element_line(colour = "grey20"),
                            axis.title = element_text(size=17),
                            axis.text = element_text(size=15))+
                      ylab("Predicted proportion of forest cover")+
                      xlab("Median distance to nearest provincial capital (km)")



## elc
elc_results$elc <- as.factor(elc_results$elc) 

elc_plot <- ggplot(elc_results, aes(x=elc, y=fit))+
            geom_bar(stat = "identity", fill="deepskyblue4")+
            geom_errorbar(aes(ymin=L_SE, ymax=U_SE), width=0.5)+
            theme(panel.background = element_blank(),
                  axis.line = element_line(colour = "grey20"),
                  axis.title = element_text(size=17),
                  axis.text = element_text(size=15))+
            ylim(0,0.085)+
            ylab("Predicted proportion of forest cover")+
            xlab("Presence of Economic Land Concessions")


## PA
PA_results$PA <- as.factor(PA_results$PA) 

PA_plot <- ggplot(PA_results, aes(x=PA, y=fit))+
            geom_bar(stat = "identity", fill="deepskyblue4")+
            geom_errorbar(aes(ymin=L_SE, ymax=U_SE), width=0.5)+
            theme(panel.background = element_blank(),
                  axis.line = element_line(colour = "grey20"),
                  axis.title = element_text(size=17),
                  axis.text = element_text(size=15))+
            ylim(0,0.07)+
            ylab("Predicted proportion of forest cover")+
            xlab("Presence of Protected Areas")







#### PROVINCE LEVEL MODELS ####
  ### Load data ####

# load dat_cat - aggregated to the province level, scaled, and categorised
dat_cat <- read.csv("Data/commune/dat_cat.csv", header=T, stringsAsFactors = T)
str(dat_cat)
dat_cat <- dat_cat[ ,-1]


## Do the same steps as the top of the script to create the new variable.

# At the moment, the forested area is in pixels, where 1 pixel = 0.09 km2. The area is in km2. Therefore, I need to create a new variable that is the forested area in km2, and then create a proportion variable.
dat_cat$forest_area <- dat_cat$ForPix * 0.09
dat_cat$forest_prop <- dat_cat$forest_area / dat_cat$areaKM

# look at histogram of the new variable
hist(dat_cat$forest_prop)

# This looks quite different to the commune level histogram. This is because forest cover is all aggregated, and so there are no provinces with actually zero forest cover. But there are still quite a few with very low.

# There are no rows with proportions greater than 1 or less than 0.

  ### Models ####


## first I will run all of the same models as I ran above in the commune-level section. I will also run a few more models that are simpler


# re-name vars (to make the facet titles further down look better)
dat_cat <- dat_cat %>% 
  rename(Land_conflict = land_confl.cat,Crime = crim_case.cat, 
         In_migration = Pax_migt_in.cat,
         Out_migration = Pax_migt_out.cat, Primary_sec = propPrimSec.cat,
         Secondary_sec = propSecSec.cat, no_farmland = Les1_R_Land.cat,
         Owns_pigs = pig_fam.cat, Distance_border = dist_border.cat,
         Distance_capital = dist_provCap.cat, Economic_concession = elc,
         PAs = PA, School_attendance=M6_24_sch.cat, Distance_school=dist_sch.cat,
         Elevation=mean_elev.cat) 

dat_cat$Economic_concession <- as.factor(dat_cat$Economic_concession)
dat_cat$PAs <- as.factor(dat_cat$PAs) 



# Global mode for diagnostics
m0 <- glmmTMB(forest_prop ~  pop_den.cat + Land_conflict + Crime + In_migration + Out_migration +
                School_attendance + Primary_sec + Secondary_sec + no_farmland + Owns_pigs +
                Distance_school + Elevation +Distance_border + Distance_capital + 
                Economic_concession + PAs +
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())
summary(m0)



## Models run above in commune-level section
m1 <- glmmTMB(forest_prop ~ pop_den.cat + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m2 <- glmmTMB(forest_prop ~ School_attendance + Elevation + Distance_border+Distance_capital+Economic_concession+PAs +
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m3 <- glmmTMB(forest_prop ~ Primary_sec + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m4 <- glmmTMB(forest_prop ~ Owns_pigs + Elevation + Distance_border+Distance_capital+Economic_concession+PAs +  
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m5 <- glmmTMB(forest_prop ~ Distance_school + Elevation + Distance_border+Distance_capital+Economic_concession+PAs +  
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m6 <- glmmTMB(forest_prop ~ Crime + Elevation + Distance_border+Distance_capital+Economic_concession+PAs +  
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m7 <- glmmTMB(forest_prop ~ Out_migration + Elevation + Distance_border+Distance_capital+Economic_concession+PAs +  
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m8 <- glmmTMB(forest_prop ~ School_attendance + Distance_school + Elevation + Distance_border+Distance_capital
              +Economic_concession + PAs +   
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())


m9 <- glmmTMB(forest_prop ~ Primary_sec + Out_migration + Elevation + Distance_border+Distance_capital+
                Economic_concession+PAs +    
                offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())



## simpler models

# pop_den
m10 <- glmmTMB(forest_prop ~ pop_den.cat +  offset(log(areaKM)) + (year|Province),
              data=dat_cat, ziformula = ~1, family = beta_family())




# social justice (land_confl, crim_case)
m11 <- glmmTMB(forest_prop ~ Land_conflict + Crime +  offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())



# migration
m12 <- glmmTMB(forest_prop ~ In_migration + Out_migration +  offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())


# education
m13 <- glmmTMB(forest_prop ~ School_attendance +  offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())


# employment
m14 <- glmmTMB(forest_prop ~ Primary_sec + Secondary_sec +  offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())



# economic security
m15 <- glmmTMB(forest_prop ~ no_farmland + Owns_pigs + offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())



# access to services
m16 <- glmmTMB(forest_prop ~ Distance_school + offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())



# environmental 
m17 <- glmmTMB(forest_prop ~ Elevation + offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())



# other human vars
m18 <- glmmTMB(forest_prop ~  Distance_border + Distance_capital + offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())



# elcs and PAs
m19 <- glmmTMB(forest_prop ~ Economic_concession + PAs + offset(log(areaKM)) + (year|Province),
               data=dat_cat, ziformula = ~1, family = beta_family())






# compare models
modSel.prov <- model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19)

# m8 has the most support (which is the same as before), but m2 and m3 have some support here too. But m2 is just a simpler version of m8, and so can be discarded. So I will model average across m8 and m3.


## Model averaging
modSel.prov.sub <- modSel.prov[modSel.prov$delta<4]

m.avg.prov <- model.avg(modSel.prov.sub, fit = TRUE)

summary(m.avg.prov)

