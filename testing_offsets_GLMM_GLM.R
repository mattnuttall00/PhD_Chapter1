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
dat1$Provcomm <- paste(dat1$Province, dat1$Commune, sep = "_")

# get mean ForPix value for each commune over the years (I'm sorry, I don't know how to do this in base...! Well, I could probably work it out but it would take me longer ;) )
meanForPix <- dat1 %>% group_by(Provcomm) %>% summarise(meanForPix = mean(ForPix))
dat1 <- left_join(dat1, meanForPix, by="Provcomm")

# model 1. GLMM with offset as areaKM, set as an argument 
m1 <- glmer(ForPix ~ tot_pop + pop_den + prop_ind + (year|Province/Provcomm),
                   offset = log(areaKM), data=dat1, family="poisson")

# model 2. GLMM with offset as mean ForPix over the time period, set as argument
m2 <- glmer(ForPix ~ tot_pop + pop_den + prop_ind + (year|Province/Provcomm),
            offset = log(meanForPix), data=dat1, family="poisson")

# remove various RE terms from the RE (singular warning in above model)
m2a <- glmer(ForPix ~ tot_pop + pop_den + (year|Province/Provcomm),
            offset = log(meanForPix), data=dat1, family="poisson")


# model 3. GLM with offset as mean ForPix over the time period
m3 <- glm(ForPix ~ tot_pop + pop_den + prop_ind + year, offset = log(meanForPix), data=dat1, family = "poisson")

# model 4. GLM with offset as areaKM, set as argument
m4 <- glm(ForPix ~ tot_pop + pop_den + prop_ind + year, offset = log(areaKM), data=dat1, family = "poisson")


summary(m1)
summary(m2)
summary(m2a)
summary(m3)
summary(m4)


### manually predict

## model 1

m1.dat <- select(dat1, ForPix, tot_pop, pop_den, prop_ind, year, Province, Provcomm)

# add global intercept
m1.dat$Iglobal <- fixef(m1)[["(Intercept)"]]

# add RE intercepts for each Province 
m1.dat$Iprovince <- ranef(m1)$Province[,"(Intercept)"][
  match(m1.dat$Province, row.names(ranef(m1)$Province))]

# add Provcomm:Province name column to match the RE output
m1.dat$Provcomm_P = paste(m1.dat$Provcomm,m1.dat$Province,sep=":")

# add RE intercepts for each commune
m1.dat$Icommune <- ranef(m1)[[1]][, "(Intercept)"][
  match(m1.dat$Provcomm_P, row.names(ranef(m1)[[1]]))]

# add RE slope for year for province
m1.dat$b_year_prov <- ranef(m1)$Province[,"year"][
  match(m1.dat$Province, row.names(ranef(m1)$Province))]

# add random slope for year for commune
m1.dat$b_year_com <- ranef(m1)$'Provcomm:Province'[,"year"][
  match(m1.dat$Provcomm_P, row.names(ranef(m1)$'Provcomm:Province'))]

# now add the fixed effects of tot_pop, pop_den, prop_ind
m1.dat$b_tot_pop <- fixef(m1)['tot_pop']
m1.dat$b_pop_den <- fixef(m1)['pop_den']
m1.dat$b_prop_ind <- fixef(m1)['prop_ind']

# add offset
m1.dat$offset <- log(dat1$areaKM)[match(m1.dat$Provcomm, dat1$Provcomm)]

head(m1.dat)

# predict manually
manual_m1_pred <- with(m1.dat, {
  Iglobal +
    tot_pop*b_tot_pop +
    pop_den*b_pop_den +
    prop_ind*b_prop_ind +
    Iprovince +
    Icommune +
    year*(b_year_prov+b_year_com)+
    offset
})



## model 2

m2.dat <- select(dat1, ForPix, tot_pop, pop_den, prop_ind, year, Province, Provcomm)

# add global intercept
m2.dat$Iglobal <- fixef(m2)[["(Intercept)"]]

# add RE intercepts for each Province 
m2.dat$Iprovince <- ranef(m2)$Province[,"(Intercept)"][
  match(m2.dat$Province, row.names(ranef(m2)$Province))]

# add Provcomm:Province name column to match the RE output
m2.dat$Provcomm_P = paste(m2.dat$Provcomm,m2.dat$Province,sep=":")

# add RE intercepts for each commune
m2.dat$Icommune <- ranef(m2)[[1]][, "(Intercept)"][
  match(m2.dat$Provcomm_P, row.names(ranef(m2)[[1]]))]

# add RE slope for year for province
m2.dat$b_year_prov <- ranef(m2)$Province[,"year"][
  match(m2.dat$Province, row.names(ranef(m2)$Province))]

# add random slope for year for commune
m2.dat$b_year_com <- ranef(m2)$'Provcomm:Province'[,"year"][
  match(m2.dat$Provcomm_P, row.names(ranef(m2)$'Provcomm:Province'))]

# now add the fixed effects of tot_pop, pop_den, prop_ind
m2.dat$b_tot_pop <- fixef(m2)['tot_pop']
m2.dat$b_pop_den <- fixef(m2)['pop_den']
m2.dat$b_prop_ind <- fixef(m2)['prop_ind']

# add offset
m2.dat$offset <- log(dat1$meanForPix)[match(m2.dat$Provcomm, dat1$Provcomm)]

head(m2.dat)

# predict manually
manual_m2_pred <- with(m2.dat, {
  Iglobal +
    tot_pop*b_tot_pop +
    pop_den*b_pop_den +
    prop_ind*b_prop_ind +
    Iprovince +
    Icommune +
    year*(b_year_prov+b_year_com)+
    offset
})



## model 3

m3.dat <- select(dat1, ForPix, tot_pop, pop_den, prop_ind, year, Provcomm)

# add intercept
m3.dat$Int <- m3$coefficients["(Intercept)"]

# add the fixed effects of tot_pop, pop_den, prop_ind
m3.dat$b_tot_pop <- m3$coefficients["tot_pop"]
m3.dat$b_pop_den <- m3$coefficients["pop_den"]
m3.dat$b_prop_ind <- m3$coefficients["prop_ind"]
m3.dat$b_year <- m3$coefficients["year"]

# add offset
m3.dat$offset <- log(dat1$meanForPix)[match(m3.dat$Provcomm, dat1$Provcomm)]

head(m3.dat)

# predict manually
manual_m3_pred <- with(m3.dat, {
  Int +
    tot_pop*b_tot_pop +
    pop_den*b_pop_den +
    prop_ind*b_prop_ind +
    year*b_year+
    offset
})



## model 4

m4.dat <- select(dat1, ForPix, tot_pop, pop_den, prop_ind, year, Provcomm)

# add intercept
m4.dat$Int <- m4$coefficients["(Intercept)"]

# add the fixed effects of tot_pop, pop_den, prop_ind
m4.dat$b_tot_pop <- m4$coefficients["tot_pop"]
m4.dat$b_pop_den <- m4$coefficients["pop_den"]
m4.dat$b_prop_ind <- m4$coefficients["prop_ind"]
m4.dat$b_year <- m4$coefficients["year"]

# add offset
m4.dat$offset <- log(dat1$areaKM)[match(m4.dat$Provcomm, dat1$Provcomm)]

head(m4.dat)

# predict manually
manual_m4_pred <- with(m4.dat, {
  Int +
    tot_pop*b_tot_pop +
    pop_den*b_pop_den +
    prop_ind*b_prop_ind +
    year*b_year+
    offset
})


man_pred_obs <- data.frame(m1=exp(manual_m1_pred),m2=exp(manual_m2_pred),m3=exp(manual_m3_pred),
                           m4=exp(manual_m4_pred))

# plot all manual predictions against each other
pairs(man_pred_obs)

par(mfrow=c(2,2))
hist(exp(manual_m1_pred))
hist(exp(manual_m2_pred))
hist(exp(manual_m3_pred))
hist(exp(manual_m4_pred))

# plot residuals
hist(residuals(m1))
hist(residuals(m2))
hist(residuals(m3))
hist(residuals(m4))

### predicting from the 3 models using new data

## m1
m1_newdat <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                             pop_den = mean(dat1$pop_den),
                             prop_ind = mean(dat1$prop_ind))

m1_pred <- as.vector(predict(m1, newdata=m1_newdat, type="response", re.form=NA))


## m2
m2_newdat <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                        pop_den = mean(dat1$pop_den),
                        prop_ind = mean(dat1$prop_ind))

m2_pred <- as.vector(predict(m2, newdata=m2_newdat, type="response", re.form=NA))


## m3
m3_newdat <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                        pop_den = mean(dat1$pop_den),
                        prop_ind = mean(dat1$prop_ind),
                        year = mean(dat1$year),
                        meanForPix = 1) # I'm assuming here we want per unit (pixel) as that is what m1+m2 are doing?

m3_pred <- as.vector(predict(m3, newdata=m3_newdat, type="response"))


## m4
m4_newdat <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                        pop_den = mean(dat1$pop_den),
                        prop_ind = mean(dat1$prop_ind),
                        year = mean(dat1$year),
                        areaKM = 1) # I'm assuming here we want per unit (area) as that is what m1+m2 are doing?

m4_pred <- as.vector(predict(m4, newdata=m4_newdat, type="response"))

# bind all predictions
preds <- data.frame(m1_pred = m1_pred, m2_pred = m2_pred, m3_pred = m3_pred, m4_pred = m4_pred,
                    tot_pop = m1_newdat$tot_pop)

# plot all together
ggplot(dat1, aes(x=tot_pop, y=ForPix))+
  geom_line(data=preds, aes(y=m1_pred), colour="red", size=1)+
  geom_line(data=preds, aes(y=m2_pred), colour="blue", size=1)+
  geom_line(data=preds, aes(y=m3_pred), colour="green", size=1)+
  geom_line(data=preds, aes(y=m4_pred), colour="orange", size=1)

# so for m1 (red line), these predictions are the predicted count of forest pixels per UNIT AREA for an "average" commune, given mean pop_den and prop_ind, and varying values of tot_pop

# for m2 (blue line), these are the predicted counts of forest pixels....per what?....per pixel of forest?  Can't quite get my head around what this is predicting.  

# neither for m3...

# the models are defintely not agreeing in terms of predictions though!