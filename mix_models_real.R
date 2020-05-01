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
  ## Education ####
    # M6_24_sch ####

# only one variables in this set as they were all highly correlated. See data clean script for more details

# plot the variable against ForPix
ggplot(dat, aes(x=M6_24_sch, y=ForPix))+
  geom_point()
# Communes with very low proportion of boys in school and very high proportions of boys in school tend to have less forest. The communes with between 50-75% of boys in school tend to have the most forest.

# lets see what a simple model says (no year)
glm.edu <- glm(ForPix ~ M6_24_sch, data=dat, family=poisson)
summary(glm.edu)
# negative effect

# create new data for plotting
newEdudat <- data.frame(M6_24_sch = seq(0,1,length.out = 100))

# predict
glm.edu.pred <- predict(glm.edu, newdata = newEdudat, type="response", se=T)
newEdudat <- cbind(newEdudat, glm.edu.pred)
newEdudat <- newEdudat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newEdudat, aes(x=M6_24_sch, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=M6_24_sch, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newEdudat, size=1)
# pretty shitty fit

# add year
glm.edu_year <- glm(ForPix ~ M6_24_sch + year, data=dat, family=poisson)
summary(glm.edu_year)
# asuming M6_24_sch == 0, 2008, 2009, and 2010 have a positive effect on forest relative to 2007, but 2011 and 2012 have negative effects relative to 2007

# interaction
glm.edu_year_int <- glm(ForPix ~ M6_24_sch*year, data=dat, family=poisson)
summary(glm.edu_year_int)
# the effect of year on the effect of M6_24_sch varies depending on the year

# plot it
plot_model(glm.edu_year_int, type="int")
# again 2011 and 2012 have more forest relative to earlier years at lower values of education.  If I'm honest, I am not really sure what this means.

# take home message for now is that communes with more children in school have less forest. This is probably reflecting that fact that school attendance is going to be much lower in remote, forested communes compared with higher attendance in urbanised communes, that don't have much forest
  ## Employment ####
    # propPrimSec ####

# plot the var
ggplot(dat, aes(x=propPrimSec, y=ForPix))+
  geom_point()
# communes with a higher proprtion of people employed in the primary sector tend to have more forest. This makes sense as the more rural, remote communes tend to have more forest, and these are the communes where more people are going to be farmers, fisherman etc.

# split by year
ggplot(dat, aes(x=propPrimSec, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)

# simple model, no year
glm.primsec <- glm(ForPix ~ propPrimSec, data=dat, family=poisson)
summary(glm.primsec)
# as expected, positive effect

# create new data for plotting
newPrimSecdat <- data.frame(propPrimSec = seq(0.01,1,length.out = 100))

# predict
glm.PrimSec.pred <- predict(glm.primsec, newdata = newPrimSecdat, type="response", se=T)
newPrimSecdat <- cbind(newPrimSecdat, glm.PrimSec.pred)
newPrimSecdat <- newPrimSecdat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newPrimSecdat, aes(x=propPrimSec, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=propPrimSec, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newPrimSecdat, size=1)
# not a massive effect

# include year
glm.PrimSec_year <- glm(ForPix ~ propPrimSec + year, data=dat, family=poisson)
summary(glm.PrimSec_year)
# assuming propPrimSec == 0, subsequent years have a negative effect on forest relative to 2007, until 2011 and 2012 where it becomes positive

# interaction
glm.PrimSec_year_int <- glm(ForPix ~ propPrimSec * year, data=dat, family=poisson)
summary(glm.PrimSec_year_int)
# now the effect of year (when propPrimSec ==  0) relative to 2007 are all negative. In each subsequent year, the effect of propPrimSec gets larger

# compare models
anova(glm.PrimSec_year,glm.PrimSec_year_int, test="Chisq")
# interaction model is better

# plot interaction
plot_model(glm.PrimSec_year_int, type="int")
# don't have an explanation for this yet

    # propSecSec ####

# plot var
ggplot(dat, aes(x=propSecSec, y=ForPix))+
  geom_point()
# opposite of PrimSec. This makes sense I suppose - in communes with more people employed in the production sector, you are probably talking about more urbanised communes that will have less forest. Ugly data though - most communes have very small values for propSecSec

# plot split by year
ggplot(dat, aes(x=propSecSec, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# different shape of data in 2011 and 2012. This could be an artefact of the changing questions in the commune database...

# simple model
glm.SecSec <- glm(ForPix ~ propSecSec, data=dat, family = poisson)
summary(glm.SecSec)
# as expected - negative effect

# create new data for plotting
newSecSecdat <- data.frame(propSecSec = seq(0,0.85,length.out = 100))

# predict
glm.SecSec.pred <- predict(glm.SecSec, newdata = newSecSecdat, type="response", se=T)
newSecSecdat <- cbind(newSecSecdat, glm.SecSec.pred)
newSecSecdat <- newSecSecdat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newSecSecdat, aes(x=propSecSec, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=propSecSec, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newSecSecdat, size=1)


# model with year
glm.SecSec_year <- glm(ForPix ~ propSecSec + year, data=dat, family=poisson)
summary(glm.SecSec_year)
# assuming propSecSec==0, then subsequent years have negative effect on forest, relative to 2007

# interacion with year
glm.SecSec_year_int <- glm(ForPix ~ propSecSec * year, data=dat, family=poisson)
summary(glm.SecSec_year_int)
# the effect of propSecSec gets larger (i.e. more negative) in subsequent years

# compare models
anova(glm.SecSec_year, glm.SecSec_year_int, test="Chisq")
# interaction model is better

# plot
plot_model(glm.SecSec_year_int, type="int")
# 2007:2010 all have virtually identical effects. Big difference in effect of 2011 and 2012 on the effect of propSecSec.  In these later two years the effect of propSecSec becomes much larger. In the eariler years, propSecSec actually has very little effect on forest cover. I am concerned this is because of the changes in the questions in the commune database in 2011

    # All employment vars ####

# fit simple model with both vars
glm.Emp <- glm(ForPix ~ propPrimSec + propSecSec, data=dat, family=poisson)
summary(glm.Emp)
# the effect of propSecSec has reversed direction (from negative to positive)

# include year
glm.Emp_year <- glm(ForPix ~ propPrimSec + propSecSec + year, data=dat, family=poisson)
summary(glm.Emp_year)
# including year has increased the effect size of PrimSec, but reducd the effect size of SecSec

# create new data for plotting
newprimdata <- expand.grid(year = c("2007","2008", "2009", "2010", "2011", "2012"),
                          propPrimSec = seq(0.01,1, length.out = 100),
                          propSecSec = 0.008)

newsecdata <- expand.grid(year = c("2007","2008", "2009", "2010", "2011", "2012"),
                          propPrimSec = 0.87,
                          propSecSec = seq(0,0.85,length.out = 100))

# predict
glm.Prim_year_pred <- predict(glm.Emp_year, newdata=newprimdata, type="response", se=T)
newprimdata <- cbind(newprimdata,glm.Prim_year_pred)

glm.Sec_year_pred <- predict(glm.Emp_year, newdata=newsecdata, type="response", se=T)
newsecdata <- cbind(newsecdata,glm.Sec_year_pred)

# plot propPrimSec
ggplot(newprimdata, aes(x=propPrimSec, y=fit, group=year, colour=year))+
  geom_line()

# plot propSecSec
ggplot(newsecdata, aes(x=propSecSec, y=fit, group=year, colour=year))+
  geom_line()

# the partial effect of propSecSec is completely opposite to the effect of the variable when modelled alone. Let's check what the interaction between the two vars looks like

# model with interaction between propPrimSec and propSecSec
glm.prim_sec_int <- glm(ForPix ~ propPrimSec*propSecSec, data=dat, family=poisson)
summary(glm.prim_sec_int)
# the effect size of PrimSec on forest increases with unit increase of SecSec

# plot interaction
plot_model(glm.prim_sec_int, type="int")
# hmm, this looks dodgy

# test 3-way interaciton with year
glm.prim_sec_year_in <- glm(ForPix ~ propPrimSec*propSecSec*year, data=dat, family=poisson)
summary(glm.prim_sec_year_in)

# plot 3-way interaction
plot_model(glm.prim_sec_year_in, type="int")

# Not sure what is really happening here. There's clearly an odd relatinonship between the two variables. There is only really an interaction in 2008 and 2010. I would probably be inclinde to just drop propSecSec, as I'm not really sure what it contributes to my hypotheses. 

  ## Economic security ####
    # Les_1_R_Land ####

# plot var
ggplot(dat, aes(x=Les1_R_Land, y=ForPix))+
  geom_point()
# looks like as proportion of people with less than 1 ha of rice land increases, the amount of forest in the commune decreases

# split by year
ggplot(dat, aes(x=Les1_R_Land, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)

# simple model
glm.rice <- glm(ForPix ~ Les1_R_Land, data=dat, family=poisson)
summary(glm.rice)
# negative effect


# create new data for plotting
newricedat <- data.frame(Les1_R_Land = seq(0,1,length.out = 100))

# predict
glm.rice.pred <- predict(glm.rice, newdata = newricedat, type="response", se=T)
newricedat <- cbind(newricedat, glm.rice.pred)
newricedat <- newricedat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newricedat, aes(x=Les1_R_Land, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=Les1_R_Land, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newricedat, size=1)


# add year
glm.rice_year <- glm(ForPix ~ Les1_R_Land + year, data=dat, family=poisson)
summary(glm.rice_year)
# varying effects of year

# create new data
newriceYeardat <- expand.grid(year = c("2007","2008","2009","2010","2011","2012"),
                              Les1_R_Land = seq(0,1,length.out = 100))

# predict
riceYearpred <- predict(glm.rice_year, newdata=newriceYeardat, type = "response", se=T)
newriceYeardat <- cbind(newriceYeardat,riceYearpred)

# plot
ggplot(newriceYeardat, aes(x=Les1_R_Land, y=fit, group=year, colour=year))+
  geom_line()
# 2011 and 2012 look different to the earlier years. 

# interaction with year
glm.rice_year_int <- glm(ForPix ~ Les1_R_Land*year, data=dat, family=poisson)
summary(glm.rice_year_int)
# the effect of Les1_R_Land gets larger with each subsequent year

# compare models
anova(glm.rice_year,glm.rice_year_int, test="Chisq")
# interaction model is better

# plot
plot_model(glm.rice_year_int, type="int")
# So for example, in 2007 if you have no people with no rice land then forest cover is high, and as you increase the proportion of people with no rice land, the amount of forest cover drops more steeply. Then in 2011, the forest cover starts lower with no people with no rice land, but doesn't drop nearly as quickly as you increase the proportion of people with no rice land. In 2012, forest cover starts high when you have no people with no rice land, and has a slope somewhere in the middle of 2007 and 2011.  

# main point here is that as you increase the proportion of people with less than 1ha of rice land, you will have less forest. This could reflect the fact that in more urbanised areas, that have less forest, you have fewer people with agricultural land.

    # pig_fam ####

# plot var
ggplot(dat, aes(x=pig_fam, y=ForPix))+
  geom_point()
# not much of a pattern here that I can see

# split by year
ggplot(dat, aes(x=pig_fam, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# no discernable difference

# simple model
glm.pig <- glm(ForPix ~ pig_fam, data=dat, family=poisson)
summary(glm.pig)
# positive effect


# create new data for plotting
newpigdat <- data.frame(pig_fam = seq(0,1,length.out = 100))

# predict
glm.pig.pred <- predict(glm.pig, newdata = newpigdat, type="response", se=T)
newpigdat <- cbind(newpigdat, glm.pig.pred)
newpigdat <- newpigdat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newpigdat, aes(x=pig_fam, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=pig_fam, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newpigdat, size=1)
# not an overwhelming relationship. But as the proportion of families with pigs increase, so does forest cover


# include year
glm.pig_year <- glm(ForPix ~ pig_fam + year, data=dat, family=poisson)
summary(glm.pig_year)
# The intercept for the fit for each year would be slightly higher in each year relative to 2007

# create new data
newpigYeardat <- expand.grid(year = c("2007","2008","2009","2010","2011","2012"),
                              pig_fam = seq(0,1,length.out = 100))

# predict
pigYearpred <- predict(glm.pig_year, newdata=newpigYeardat, type = "response", se=T)
newpigYeardat <- cbind(newpigYeardat,pigYearpred)

# plot
ggplot(newpigYeardat, aes(x=pig_fam, y=fit, group=year, colour=year))+
  geom_line()


# year as interaction
glm.pig_year_int <- glm(ForPix ~ pig_fam * year, data=dat, family = poisson)
summary(glm.pig_year_int)
# this changes the direction the intercepts move relative to 2007. Year has a positive effect on the effect of pig_fam i.e. with each year, the effect size of pig_fam gets larger

# plot
plot_model(glm.pig_year_int, type="int")
# the stand out year here is 2012 - with low values of pig_fam, you have less forest, but as pig_fam increases, the slope is much steeper than the other years
    # All economic security vars ####

# simple model with both vars
glm.pigrice <- glm(ForPix ~ pig_fam + Les1_R_Land, data=dat, family=poisson)
summary(glm.pigrice)
# both directions of effects are the same as they were in individual models, but the effect size of rice has decreased

# plot partial effects

# create new data
pigricedat <- expand.grid(pig_fam =  seq(0,1,length.out = 100),
                          Les1_R_Land = mean(dat$Les1_R_Land))

ricepigdat <- expand.grid(Les1_R_Land = seq(0,1,length.out = 100),
                          pig_fam = mean(dat$pig_fam))

# predict
pigrice_pred <- predict(glm.pigrice, newdata = pigricedat, type="response", se=T)
pigricedat <- cbind(pigricedat,pigrice_pred)
ricepig_pred <- predict(glm.pigrice, newdata = ricepigdat, type="response", se=T)
ricepigdat <- cbind(ricepigdat,ricepig_pred)

# plot
ggplot(pigricedat, aes(x=pig_fam, y=fit))+
  geom_line()

ggplot(ricepigdat, aes(x=Les1_R_Land, y=fit))+
  geom_line()


# test interaction between vars
glm.pigrice_int <- glm(ForPix ~ pig_fam * Les1_R_Land, data=dat, family=poisson)
summary(glm.pigrice_int)
# significant interaction - with every unit increase of pig_fam, the effect size of rice decreases

# plot
plot_model(glm.pigrice_int, type="int")
# so when rice land is low, pig_fam has a much larger effect on forest. When rice is high, the effect of pig_fam is much less

# plot other way around
plot_model(glm.pigrice_int, type="pred", terms = c("Les1_R_Land","pig_fam"))
# when pig_fam is low, the effect size of rice land is lower, and vice versa. 

# add year
glm.pigrice_year <- glm(ForPix ~ pig_fam + Les1_R_Land + year, data=dat, family=poisson)
summary(glm.pigrice_year)
# not very disimilar to the previous individual models with year

# plot
plot_model(glm.pigrice_year, type="pred", terms = c("Les1_R_Land","pig_fam","year"))
# looks like 2011 and 2012 are different

# test interactions
glm.pigriceyear_int <- glm(ForPix ~ pig_fam * Les1_R_Land * year, data=dat, family=poisson)
summary(glm.pigriceyear_int)

# plot
plot_model(glm.pigriceyear_int, type="int")
plot_model(glm.pigriceyear_int, type="pred", terms = c("Les1_R_Land","pig_fam","year"))

# compare models
anova(glm.pigrice_year,glm.pigriceyear_int, test="Chisq")
# model with interactions is better

# summary here is that when lots of people have less than 1ha of rice land, then the proportion of families who keep pigs has a much larger effect on predicting forest cover. The size of the effects and teh differences in the effects of the interaction changes between years.
# When the proportion of families with pigs is very high, in 2007 and 2009 this causes a stonger effect of rice land on predicting forest cover. This interaction doesn't seem to have much of an effect in the other years.  

  ## Access to services ####

# dist_sch, garbage, KM_Comm

    # dist_sch ####

# plot var
ggplot(dat, aes(x=dist_sch, y=ForPix))+
  geom_point()
# this looks like a positive trend - as median distance to school increases, so does forest cover

# split by year
ggplot(dat, aes(x=dist_sch, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# no obvious difference

# simple model
glm.school <- glm(ForPix ~ dist_sch, data=dat, family=poisson)
summary(glm.school)
# sig positive relationship

# plot
plot_model(glm.school, type="pred")


# add year
glm.sch_year <- glm.school <- glm(ForPix ~ dist_sch + year, data=dat, family=poisson)
summary(glm.sch_year)
# intercepts are sig differnet from 2007, and move up

# plot 
plot_model(glm.sch_year, type="pred", terms = c("dist_sch","year"))
# in each subsequent year, amount of forest increases more with each extra km to a school. This is interesting becausse I doubt the distances to school within a given commune change that much over time (some will change as schools get built or moved etc). So I'm not really sure why this would be

# add interaction
glm.sch_year_int <- glm(ForPix ~ dist_sch * year, data=dat, family=poisson)
summary(glm.sch_year_int)
# in each subsequent year the effect of distance to school increases relative to 2007, except for 2009, when it is lower

# plot
plot_model(glm.sch_year_int, type="int")


    # garbage ####

# plot var
ggplot(dat, aes(x=garbage, y=ForPix))+
  geom_point()
# not pretty

# simple model
glm.garbage <- glm(ForPix ~ garbage, data=dat, family=poisson)
summary(glm.garbage)
# negative effect

# plot
plot_model(glm.garbage, type="pred")


# add year
glm.garbage_year <- glm(ForPix ~ garbage + year, data=dat, family=poisson)
summary(glm.garbage_year)
# year has a negative effect

# plot
plot_model(glm.garbage_year, type="pred", terms = c("garbage","year"))
# virtually no difference

# test interaction
glm.garbage_year_int <- glm(ForPix ~ garbage * year, data=dat, family=poisson)
summary(glm.garbage_year_int)
# in 2008 the effect of garbage on forest cover goes down (relative to 2007), in all other years it goes up

# plot
plot_model(glm.garbage_year_int, type="int")
# so in 2007 and 2008, the effect of garbage on forest cover is larger - the slopes are much steeper. All the other years have much flatter curves

# commues with more people who have access to garbage have less forest cover - again this is reflecting urbanised areas I think

    # KM_Comm ####

# plot var
ggplot(dat, aes(x=KM_Comm, y=ForPix))+
  geom_point()

# hang on - is this variable appropriate