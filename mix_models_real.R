### This script is the analysis of the socioeconomic data from the Commune Database, as predictors of forest loss between 2007 and 2012. This will be chapter 1 in my Phd. All data have been cleaned and variables checked for correlation, and relevant variables selected. For data creation, data cleaning, and variable selection, see the "Socioecon_dataClean.R" script in H:PhD_Chapter1

#### Load libraries and data ####

library(tidyverse)
library(arm)
library(sjPlot)
library(DHARMa)
library(lme4)
library(nlme)
library(Matrix)
#library(rcpp)
library(psych)
library(cowplot)
library(visreg)
library(car)
library(ggeffects)

# load data
dat <- read.csv("Data/commune/dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.factor(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

str(dat)


#### data exploration ####

# for the moment, and based on discussions with Nils, Jeroen, and Luc, I will use raw forest pixels as my response. This eliminates all the zeros associated with difference in pixels (the response I originally considered), and so means I don't have to mess around with zero-inflated models, and I can use a Poisson distribution

  ## population demographics ####

    # tot_pop ####

# tot_pop. All years
ggplot(dat, aes(x=tot_pop, y=ForPix))+
  geom_point()
# difficult to see as there are so many communes with very small number of forested pixels. Nevertheless, it does look as though broadly speaking, as total population increases, forest pixels decrease

# as above, but split by year
ggplot(dat, aes(x=tot_pop, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# no obvious changes over time

# all years but coloured by year
ggplot(dat, aes(x=tot_pop, y=ForPix, group=year, colour=year))+
  geom_point()

# simple linear model
lm.tot_pop <- lm(ForPix ~ tot_pop, data=dat)
summary(lm.tot_pop)
# tot_pop is significant in predicting the amount of forest in a commune

# lets see if year makes a difference
lm.tot_pop_year <- lm(ForPix ~ tot_pop + year, data=dat)
summary(lm.tot_pop_year)
# year doesn't appear to make any difference. This could be because the total population of communes don't change dramatically over time

# lets plot it
visreg(lm.tot_pop_year)
visreg(lm.tot_pop_year, xvar = "tot_pop", by = "year")
# definite downward trend, although we can see it is predicting negative ForPix values which is not correct

# try glm with poisson structure
glm.tot_pop_year <- glm(ForPix ~ tot_pop + year, data=dat, family = poisson)
summary(glm.tot_pop_year)
# All intercepts for the different factor levels are different from level 1 (2007), but by very small amounts. 2010 has a very small difference and relatively larger SE.

# predict
newpopdat <- expand.grid(tot_pop = seq(279,39117,length.out = 100),
                         year = c("2007","2008","2009","2010","2011","2012"))  
glm.tot_pop_year.predict <- predict(glm.tot_pop_year, newdata = newpopdat, type="response", se=T)
newpopdat <- cbind(newpopdat,glm.tot_pop_year.predict)
newpopdat <- newpopdat %>% rename(ForPix = fit)

# plot just fit lines
ggplot(newpopdat, aes(x=tot_pop, y=ForPix, group=year, colour=year))+
  geom_line()

# plot results with original points
ggplot(NULL, aes(x=tot_pop, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newpopdat, aes(group=year, colour=year), size=1)


# check model with interaction
glm.tot_pop_year_int <- glm(ForPix ~ tot_pop * year, data=dat, family=poisson)
summary(glm.tot_pop_year_int)

anova(glm.tot_pop_year_int, glm.tot_pop_year, test="Chisq")
# model with interaction is better

# plot marginal effects
plot_model(glm.tot_pop_year_int, type="pred", terms = c("tot_pop","year"))
# Interesting. The shape is the same for all years - the lower the population of the communes, the more forest there is. However, in earlier years communes with lower populations have more forest than communes with an equivalent population in later years (i.e forest is being lost overall, over time).  But in earlier years (07,09,10), the amount of forest drops more quickly with increasing population size. When the population of a commune reaches >4000, the commune is more likely to have more forest in 2011 and 2012 than in earlier years (ie. the lines cross). 


## take home message here is that communes with low total populations tend to have more forest. The decrease in forest with increasing population is steep in communes with low populations, but once you get to population sizes of around 10,000, the slope gets less steep. Once you get to a population size of 20,000 you have very little forest, and the slope flattens right out. Interestingly year does seem to make a differnce, although not a huge difference. Could this interaction be showing reforestation / plantation expansion in later years?

# Kez had a good idea - because commune populations change over time, perhaps the difference in the slopes for different years is reflecting an increase in population of communes that retain their forest cover over time.  So those communes are being shifted right on the plot (i.e. increasing populations) but are not being shifted down (i.e. not losing forest). This would explain the "flatter" slope in later years.

    # prop_ind ####

# plot prop_ind against ForPix across all years
ggplot(dat, aes(x=prop_ind, y=ForPix))+
  geom_point()
# this doesn't suggest any obvious trend.  I was expecting communes with more indigneous people to have more forest, but this isn't immediately obvious

# split by year
ggplot(dat, aes(x=prop_ind, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# the only thing I can spot that changes over time (which may be interesting), is that the number of communes with prop_ind == 1 decreases year on year(far right of each plot). This is showing the ethinc make-up of communes changing, as more in-migrants move in to once remote communities. It's not obvious from these plots whether or not the amount of forest in those communes is also decreasing

# fit simple model
glm.prop_ind <- glm(ForPix ~ prop_ind, data=dat, family=poisson)
summary(glm.prop_ind)
# ok so there is a positive relationship between prop_ind and forest cover, which is what I suspected it would be. 

# create new data for plotting
newinddat <- data.frame(prop_ind = seq(1,0,length.out = 100))

# predict
glm.prop_ind.pred <- predict(glm.prop_ind, newdata = newinddat, type="response", se=T)
newinddat <- cbind(newinddat, glm.prop_ind.pred)
newinddat <- newinddat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newinddat, aes(x=prop_ind, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=prop_ind, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newinddat, size=1)


# now check model with year as an interaction term
glm.prop_ind_year <- glm(ForPix ~ prop_ind * year, data=dat, family=poisson)
summary(glm.prop_ind_year)
# some variation in the direction of effects of prop_ind between years

# compare models
anova(glm.prop_ind_year, glm.prop_ind, test="Chisq")
# model with interaction term is better

# plot marginal effects
plot_model(glm.prop_ind_year, type="pred", terms = c("prop_ind","year"))
# The trend is the same shape as the model without year.  And in a similar vein to tot_pop, the later years (2011, 2012) are slightly separated from the earlier years. In this case they have more forest in communes with higher prop_ind values

    # pop_den ####


# plot population density against forest cover across all years
ggplot(dat, aes(x=pop_den, y=ForPix))+
  geom_point()
# so you are very unlikely to have any forest cover at all above a population density of around 250 people/km2

# plot split by year
ggplot(dat, aes(x=pop_den, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# no obvious difference but quite a difficult plot to see anything!

# simple model (no year)
glm.pop_den <- glm(ForPix ~ pop_den, data=dat, family=poisson)
summary(glm.pop_den)
# as expected, there is a negative relatinonship between population density and forest cover

## getting warning messsage in all pop_den models - "fitted rates numerically 0 occurred".  I think this is because of the shape of the data, and so some of my predicted probabilites are indistinguishable from 0

# create new data for plotting
newpop_dendat <- data.frame(pop_den = seq(0.31,2201,length.out = 100))

# predict
glm.pop_den.pred <- predict(glm.pop_den, newdata = newpop_dendat, type="response", se=T)
newpop_dendat <- cbind(newpop_dendat, glm.pop_den.pred)
newpop_dendat <- newpop_dendat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newpop_dendat, aes(x=pop_den, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=pop_den, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newpop_dendat, size=1)
# I mean, there is not much more the model could do!


# test model with year (no interaction)
glm.pop_den_year <- glm(ForPix ~ pop_den + year, data=dat, family=poisson)
summary(glm.pop_den_year)
# subsequent years have small positive effects on forest cover relative to 2007, assuming pop_den==0

# model with year as interaction
glm.pop_den_year.int <- glm(ForPix ~ pop_den * year, data=dat, family=poisson)
summary(glm.pop_den_year.int)
# the effect of population density on forest cover is higher in subsequent years compared to 2007

# compare models
anova(glm.pop_den_year,glm.pop_den_year.int, test="Chisq")
# interaction model is better

# plot interaction model
plot_model(glm.pop_den_year.int, type="pred", terms = c("pop_den","year"))
# can't see any difference

    # All demographic vars ####


# fit simple glm model with all demographic variables
glm.demog <- glm(ForPix ~ tot_pop + prop_ind + pop_den, data=dat, family=poisson)
summary(glm.demog)
# including all vars appears to have reversed the sign of the tot_pop effect (from negative to positive)  

# add year
glm.demog_year <- glm(ForPix ~ tot_pop + prop_ind + pop_den + year, data=dat, family=poisson)
summary(glm.demog_year)
# assuming all vars set to 0, subsequent years have small positive effect

# allow interactions between all vars and year
glm.demog_yaer_int <- glm(ForPix ~ tot_pop*year + prop_ind*year + pop_den*year, data=dat, family=poisson)
summary(glm.demog_yaer_int)

# plot
plot_model(glm.demog_yaer_int, type="int")
# the direction of the effect of tot_pop has been completely reversed which is confusing

# remove pop_den to see if direction of effect of tot_pop changes
glm.demog_nopopden <- glm(ForPix ~ tot_pop*year + prop_ind*year, data=dat, family=poisson)
summary(glm.demog_nopopden)
# yes - so tot_pop is a negative slope again

# plot
plot_model(glm.demog_nopopden, type="int")


# compare models with and without pop_den
anova(glm.demog_yaer_int,glm.demog_nopopden, test="Chisq")
# more complex model is better

# test interaction between tot_pop and pop_den
glm.demog_totDen_int <- glm(ForPix ~ tot_pop*pop_den, data=dat, family = poisson)
summary(glm.demog_totDen_int)

# plot 
plot_model(glm.demog_totDen_int, type="int")

# mmm ok.  So I think what is happening here is that when population density is very high, then it is irrelevant what the total population is - there is no forest (see the pop_den plots). This reflects generally smaller, urban communes, cities etc.  But when population density is very low, then total population seems to have a positive effect. I think this may be because communes that have larger absolute populations but very low densities, these are the large, remote communes which probably have quite a lot of forest. The total populations are relatively "large" just because they are geographically large communes (many villages), but their density is actually very low.  These are probably the communes in Mondulkiri, Rattanikiri, Stung Treng, Koh Kong etc. where there is a lot of forest. 
# The pop_den data isn't very nice, so I wonder whether later on if I include commune size as an offset, then this will acount for the issue above without the need to incluide pop_den.