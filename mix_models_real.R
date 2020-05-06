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
dat$year <- as.numeric(dat$year)
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

# simple model
glm.tot_pop <- glm(ForPix ~ tot_pop, data=dat, family=poisson)
summary(glm.tot_pop)
# negative effect

# plot
totpop_plot <- plot_model(glm.tot_pop, type="pred")

# extract above data
newtotpopdat <- data.frame(ForPix = totpop_plot$tot_pop$data$predicted,
                           tot_pop = totpop_plot$tot_pop$data$x)

# plot with original points
ggplot(NULL, aes(x=tot_pop, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newtotpopdat, size=1)

# add year
glm.tot_pop_year <- glm(ForPix ~ tot_pop + year, data=dat, family = poisson)
summary(glm.tot_pop_year)
# tot_pop effect not changed much. Year has a positive effect

# plot partial effects
plot_model(glm.tot_pop_year, type="pred", terms=c("tot_pop","year"))
plot_model(glm.tot_pop_year, type="pred", terms=c("year","tot_pop[279,7762,39117]"))
# not much difference in tot_pop effect across years.  Partial year effect is positive, so forest cover increases over time when population is accounted for. For communes with low populations, forest cover increases more quickly over time, whereas in communes with large populations, the slope is basically flat. The slopes are diffferent and so there is likely an interaction 


# check model with interaction
glm.tot_pop_year_int <- glm(ForPix ~ tot_pop * year, data=dat, family=poisson)
summary(glm.tot_pop_year_int)


# plot marginal effects with interaction
plot_model(glm.tot_pop_year_int, type="pred", terms = c("tot_pop","year"))
plot_model(glm.tot_pop_year_int, type="pred", terms = c("year","tot_pop[279,7762,39117]"))
# Interesting. The interactions has reversed the slope for communes with low populations. So if a communes has a low population, then over time forest cover has gone down. Communes with high population, forest cover basically hasn't changed. For communes with mean population size, forest cover has gone up. 

## The relationship between forest cover and population size depends on year and the population of the communes

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

# add year
glm.propind_year <- glm(ForPix ~ prop_ind + year, data=dat, family=poisson)
summary(glm.propind_year)

# plot
plot_model(glm.propind_year, type="pred", terms=c("prop_ind", "year"))
plot_model(glm.propind_year, type="pred", terms=c("year","prop_ind[0,0.09,1]"))
# maybe a small interaction.


# now check model with year as an interaction term
glm.prop_ind_year <- glm(ForPix ~ prop_ind * year, data=dat, family=poisson)
summary(glm.prop_ind_year)


# plot marginal effects of interaction
plot_model(glm.prop_ind_year, type="pred", terms = c("prop_ind","year"))
plot_model(glm.prop_ind_year, type="pred", terms = c("year","prop_ind[0,0.09,1]"))
# so prop_ind has an important relationship to forest cover over time - 

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

# convert year to continuous
dat$year <- as.numeric(as.character(dat$year))
str(dat)

# add year
glm.garbage_year <- glm(ForPix ~ garbage + year, data=dat, family=poisson)
summary(glm.garbage_year)
# year has a negative effect

# plot
plot_model(glm.garbage_year, type="pred", terms = c("garbage","year"))
plot_model(glm.garbage_year, type="pred", terms = c("year","garbage"))
# virtually no difference

# test interaction
glm.garbage_year_int <- glm(ForPix ~ garbage * year, data=dat, family=poisson)
summary(glm.garbage_year_int)
# in 2008 the effect of garbage on forest cover goes down (relative to 2007), in all other years it goes up

# plot
plot_model(glm.garbage_year_int, type="pred", terms=c("year","garbage"))
# so in 2007 and 2008, the effect of garbage on forest cover is larger - the slopes are much steeper. All the other years have much flatter curves

# commues with more people who have access to garbage have less forest cover - again this is reflecting urbanised areas I think

    # KM_Comm ####

# plot var
ggplot(dat, aes(x=KM_Comm, y=ForPix))+
  geom_point()
# looks like a positive relationship here

# split by year
ggplot(dat, aes(x=KM_Comm, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# doesn't look differnet, which is what I was expecting. The vast majority of villages won't get a different distance to the commmune office, as neither village nor office changes over time. Of course in a few cases they will (ie if the office moves), but this should be rare

# simple model
glm.Comm <- glm(ForPix ~ KM_Comm, data=dat, family=poisson)
summary(glm.Comm)
# positive effect

# plot
plot_model(glm.Comm, type="pred")

# add year
glm.Comm_year <- glm(ForPix ~ KM_Comm + year, data=dat, family=poisson)
summary(glm.Comm_year)
# effect of year is negative for each year relative to 2007

# plot 
plot_model(glm.Comm_year, type="pred", terms = c("KM_Comm","year"))
# very little difference - 2012 (and maybe 2011?) are slightly different from the rest

# add interaction
glm.Comm_year_int <- glm(ForPix ~ KM_Comm * year, data=dat, family=poisson)
summary(glm.Comm_year_int)
# variation in effects - some years make the effect of KM_Comm larger, some smaller

# plot
plot_model(glm.Comm_year_int, type="int")
# not a huge interaction, but the slopes do change in the different years

## take home message - the communes where the median distance from a given village to the commune office is large, have more forest.  This probably, to some degree, reflects the fact that the more remote communes that are larger and in larger provinces, do tend to have more forest.

    # All access to services vars ####

# simple model with all vars
glm.AccServ <- glm(ForPix ~ dist_sch + garbage + KM_Comm, data=dat, family=poisson)
summary(glm.AccServ)
# similar effect for dist_sch (small, positive), similar effect for garbage but reduced effect size (negative), similar effect for KM_Comm (small, positive)

# plot partial effects
plot_model(glm.AccServ, type="pred")

# add year
glm.AccServ_year <- glm(ForPix ~ dist_sch + garbage + KM_Comm + year, data=dat, family=poisson)
summary(glm.AccServ_year)
# years have small positive effects relative to 2007

# plot
plot_model(glm.AccServ_year, type="pred", terms=c("dist_sch","year"))
plot_model(glm.AccServ_year, type="pred", terms=c("garbage","year"))
plot_model(glm.AccServ_year, type="pred", terms=c("KM_Comm","year"))

# test interactions between vars (no year)
glm.AccServ_int <- glm(ForPix ~  dist_sch * garbage * KM_Comm, data=dat, family=poisson)
summary(glm.AccServ_int)

# plot
plot_model(glm.AccServ_int, type="int")

# simplify
glm.AccServ_int2 <- glm(ForPix ~  dist_sch * garbage + KM_Comm, data=dat, family=poisson)
summary(glm.AccServ_int2)

# compare
anova(glm.AccServ_int,glm.AccServ_int2, test="Chisq")
# complex model better

# simplify
glm.AccServ_int3 <- glm(ForPix ~  dist_sch + garbage * KM_Comm, data=dat, family=poisson)
summary(glm.AccServ_int3)

# compare
anova(glm.AccServ_int,glm.AccServ_int3, test="Chisq")
# complex model better

# interactions with year
glm.AccServ_int_year <- glm(ForPix ~  dist_sch*year + KM_Comm*year + garbage*year, 
                            data=dat, family=poisson)
summary(glm.AccServ_int_year)

# plot
plot_model(glm.AccServ_int_year, type="int")
# Partial effects with interaction with year shows that as distance to school increases, so does forest cover, with variation in slope between years (particularly 2009 and 2007). As distance to commune office increases, so does forest cover (2007, 2008, 2010 effects are similar, and 2009, 2011, 2012 are similar). As the proportion of families with access to garbage collection increases, forest cover decreases. Some year effects - 2007, 2008 and 2009 have much steeper negative slopes than 2010:2012.


# test 3-way interactions
glm.AccServ_int_year2 <- glm(ForPix ~  dist_sch * KM_Comm * year, 
                            data=dat, family=poisson)
summary(glm.AccServ_int_year2)

plot_model(glm.AccServ_int_year2, type="int")
#  when distances to KM_Comm are high, distance to school has a small, negative effect on forest cover. When distances to KM_Comm are low, distance to school has a larger, positive effect on forest cover. year doesn't have a huge impact on the shape of the relationship, except for 2009

glm.AccServ_int_year3 <- glm(ForPix ~  dist_sch * garbage * year, 
                            data=dat, family=poisson)
summary(glm.AccServ_int_year3)

plot_model(glm.AccServ_int_year3, type="int")
# distance to school and garbage don't really have a relationship

glm.AccServ_int_year4 <- glm(ForPix ~ KM_Comm * garbage * year, 
                            data=dat, family=poisson)
summary(glm.AccServ_int_year4)

plot_model(glm.AccServ_int_year4, type="int")
# this model is throwing an error I don't understand when I try to plot it
  ## Social justice ####
    # crim_case ####

# plot var
ggplot(dat, aes(x=crim_case, y=ForPix))+
  geom_point()
# Looks like a negative slope

# split by year
ggplot(dat, aes(x=crim_case, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# no obvious differences between years. 2011 maybe slightly different

# simple model
glm.crim <- glm(ForPix ~ crim_case, data=dat, family=poisson)
summary(glm.crim)
# hmm positive effect. I think this might be the outliers

visreg(glm.crim)

# create new data for plotting
newcrimdat <- data.frame(crim_case = seq(0,0.054,length.out = 100))

# predict
glm.crim.pred <- predict(glm.crim, newdata = newcrimdat, type="response", se=T)
newcrimdat <- cbind(newcrimdat, glm.crim.pred)
newcrimdat <- newcrimdat %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newcrimdat, aes(x=crim_case, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=crim_case, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newcrimdat, size=1)
# I think there is one outlier that is skewing the fit

# ID the outlier
dat %>% filter(crim_case > 0.03 & ForPix > 10000) %>% select(year,Province,Commune,crim_case,ForPix)
# 2009 -> Kracheh -> Boeng Char

# remove the outlier
test_crim <- dat[!(dat$year == "2009" & dat$Commune == "Boeng Char"), ]
test_crim %>% filter(year=="2009" & Commune=="Boeng Char")

# fit new model
glm.crim_sub <- glm(ForPix ~ crim_case, data=test_crim, family=poisson)
summary(glm.crim_sub)
# still positive

# new data
newcrimdat2 <- data.frame(crim_case = seq(0,0.054,length.out = 100))

# predict
glm.crim.pred2 <- predict(glm.crim_sub, newdata = newcrimdat2, type="response", se=T)
newcrimdat2 <- cbind(newcrimdat2, glm.crim.pred2)
newcrimdat2 <- newcrimdat2 %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newcrimdat2, aes(x=crim_case, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=crim_case, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newcrimdat2, size=1)
# same fit!  ok, remove the other outliers


# remove other outliers
test_crim2 <- test_crim %>% filter(!crim_case > 0.035)

# fit new model
glm.crim_sub2 <- glm(ForPix ~ crim_case, data=test_crim2, family=poisson)
summary(glm.crim_sub2)
# even larger positive effect...

# new data
newcrimdat3 <- data.frame(crim_case = seq(0,0.054,length.out = 100))

# predict
glm.crim.pred3 <- predict(glm.crim_sub2, newdata = newcrimdat3, type="response", se=T)
newcrimdat3 <- cbind(newcrimdat3, glm.crim.pred3)
newcrimdat3 <- newcrimdat3 %>% rename(ForPix = fit)

# plot the fit by iteslf
ggplot(newcrimdat3, aes(x=crim_case, y=ForPix))+
  geom_line()

# plot the fit with the points
ggplot(NULL, aes(x=crim_case, y=ForPix))+
  geom_point(data=test_crim2)+
  geom_line(data=newcrimdat3, size=1)
# ok, so it wasn't the outliers!


# test year (back to original data)
glm.crim_year <- glm(ForPix ~ crim_case + year, data=dat, family=poisson)
summary(glm.crim_year)
# woah. When year is accounted for, effect of crim_case is massive. each year has a negative effect relative to 2007

plot_model(glm.crim_year, type="pred", terms=c("crim_case","year"))
# very little difference between years

# test interaction
glm.crim_year_int <- glm(ForPix ~ crim_case * year, data=dat, family = poisson)
summary(glm.crim_year_int)
# so 2008:2010 reduce the effect size of crim_case, but 2011 ans 2012 massively increase the effect size

# plot
plot_model(glm.crim_year_int, type="int")
# 2012 having a very large influence. Doesn't seem to match with what I was expecting based on the scatter plots....2012 data didn't look massively different

## main message at the moment is that as the criminal case per captia increases, so does forest cover. This is the opposite of what I was expecting. I was expecting the more densely populated, urban areas with less forest to have higher per capita crime.

# plot crim_case split by province to check my hypithesis
ggplot(dat, aes(crim_case))+
  geom_histogram()+
  facet_wrap(dat$Province, nrow=4)
# ok so provinces like Koh Kong, Rattanikiri, Stung Treng do have crim-case values that look higher than say, Phmom Penh. 


    # land_confl ####

# plot
ggplot(dat, aes(x=land_confl, y=ForPix))+
  geom_point()
# looks negative

# split by year
ggplot(dat, aes(x=land_confl, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year)
# 2011 looks slightly different - fewer communes with  high number of conflict cases

# simple model
glm.confl <- glm(ForPix ~ land_confl, data=dat, family=poisson)
summary(glm.confl)
# negative effect

# plot
confl_plot <- plot_model(glm.confl, type="pred")

# extract predicted values from plot_model above
newconfldat <- data.frame(ForPix = confl_plot$land_confl$data$predicted,
                          land_confl = confl_plot$land_confl$data$x)
                          

# plot the fit with the points
ggplot(NULL, aes(x=land_confl, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newconfldat, size=1)
# pretty weak trend

# add year
glm.confl_year <- glm(ForPix ~ land_confl + year, data=dat, family=poisson)
summary(glm.confl_year)
# adding year increases the effect size of land_confl. Each year has a negative effect relative to 2007

# plot
plot_model(glm.confl_year, type="pred", terms=c("land_confl","year"))
# 2011 and 2012 slightly separate from other years

# add interaction
glm.confl_year_int <- glm(ForPix ~ land_confl * year, data=dat, family=poisson)
summary(glm.confl_year_int)
# 2008 and 2010 make the negative slope steeper. All the other years flatten it

# plot
plot_model(glm.confl_year_int, type="int")
# what the actual fuck 2012.  

## so generally, as the number of land conflicts increase, the amount of forest decreases.  Unless of course it is 2012, in which case the opposite is true. There are a couple of outlier points in 2012 which I think are causing this effect in 2012


    # All social justice vars ####

# simple model
glm.SocJus <- glm(ForPix ~ crim_case + land_confl, data=dat, family=poisson)
summary(glm.SocJus)
# large positive effect of crim_case, small negative effect of land_confl

# plot partial effects
plot_model(glm.SocJus, type="pred")

# add year
glm.SocJus_year <- glm(ForPix ~ crim_case + land_confl + year, data=dat, family=poisson)
summary(glm.SocJus_year)
# same trends

# plot partial effects with year
plot_model(glm.SocJus_year, type="pred", terms=c("crim_case","year"))
plot_model(glm.SocJus_year, type="pred", terms=c("land_confl","year"))


# add interactions with year
glm.SocJus_year_int <- glm(ForPix ~ crim_case*year + land_confl*year, data=dat, family=poisson)
summary(glm.SocJus_year_int)
# So 2008:2010 the positive effect of crim_case is reduced by year, but in 2011 and 2012 the effect is increased (by a huge amount in 2012). in 2008 and 2010 the negative effect of land_confl is made steeper by year, but in 2009, 2011, and 2012 the negative slope is flattened (by a huge amount in 2012).

# plot
plot_model(glm.SocJus_year_int, type="int")
# 2012 is very different to all other years.
  ## Migration ####
    # Pax_migt_in ####

# plot
ggplot(dat, aes(x=Pax_migt_in, y=ForPix))+
  geom_point()

# split by year
ggplot(dat, aes(x=Pax_migt_in, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# 2011 and 2012 look slightly different - more communes with higher migration but lower forest

# simple model
glm.migin <- glm(ForPix ~ Pax_migt_in, data=dat, family=poisson)
summary(glm.migin)
# negative effect - as in-migraion increases, forest cover decreases

# plot
migin_plot <- plot_model(glm.migin, type="pred")

# extract data from above
newmigindat <- data.frame(ForPix = migin_plot$Pax_migt_in$data$predicted,
                          Pax_migt_in = migin_plot$Pax_migt_in$data$x)
                          

# plot the fit with the points
ggplot(NULL, aes(x=Pax_migt_in, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newmigindat, size=1)


# add year
glm.migin_year <- glm(ForPix ~ Pax_migt_in + year, data=dat, family=poisson)
summary(glm.migin_year)
# 2008, 2011, and 2012 have higher intercept relative to 2007, but 2009 and 2010 are lower

# plot
plot_model(glm.migin_year, type="pred", terms=c("Pax_migt_in","year"))

# add interaction
glm.migin_year_int <- glm(ForPix ~ Pax_migt_in * year, data=dat, family=poisson)
summary(glm.migin_year_int)
# direciton of effect changes to positive, and each year from 2008:2010 adds to the effect, but 2011 and 2012 reverse it

# plot
plot_model(glm.migin_year_int, type="int")
# Interesting. So pre-2011, as the number of in-migrants to a commune increases, so does the forest cover. But in 2011 and 2012, this completely changes - as the number of in-migrants increase in a commune, the forest cover decreases so far that once you get beyond ~1000 migrants, forest cover = 0. I wonder if this reflects a surge in migration to urban areas that have no forest cover in 2011 onwards


    # Pax_migt_out ####

# plot
ggplot(dat, aes(x=Pax_migt_out, y=ForPix))+
  geom_point()

# split by year
ggplot(dat, aes(x=Pax_migt_out, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# 2011 and 2012 show a different pattern - more communes with high levels of out-migration and very low forest cover

# simple model
glm.migout <- glm(ForPix ~ Pax_migt_out, data=dat, family=poisson)
summary(glm.migout)
# negative effect

# plot
migout_plot <- plot_model(glm.migout, type="pred")

# extract data from above
newmigoutdat <- data.frame(ForPix = migout_plot$Pax_migt_out$data$predicted,
                          Pax_migt_out = migout_plot$Pax_migt_out$data$x)
                          

# plot the fit with the points
ggplot(NULL, aes(x=Pax_migt_out, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newmigoutdat, size=1)
# very sharp decline in forest cover as out-migration increases. As soon as you get more than 1000 ou-migrants, forest cover is essentially 0

# add year
glm.migout_year <- glm(ForPix ~ Pax_migt_out + year, data=dat, family=poisson)
summary(glm.migout_year)
# variable effects of year - 2008, 2011, 2012 larger intercepts than 2007, whereas 2009 and 2010 are lower

# plot
plot_model(glm.migout_year, type="pred", terms=c("Pax_migt_out","year"))
# not huge differences

# add interaction
glm.migout_year_int <- glm(ForPix ~ Pax_migt_out * year, data=dat, family=poisson)
summary(glm.migout_year_int)
# year reduces the steepness of the slope to varying degrees

# plot
plot_model(glm.migout_year_int, type="int")
# 2007 and 2009 very steep slopes, 2011 in the middle, 2008, 2010, 2012 are a bit flatter

    # All migration vars ####

# simple model
glm.migration <- glm(ForPix ~ Pax_migt_in + Pax_migt_out, data=dat, family = poisson)
summary(glm.migration)
# in-migration direction reversed to small positive. out-mig effect very similar

# plot partial effects
plot_model(glm.migration, type="pred")

# add year
glm.migration_year <- glm(ForPix ~ Pax_migt_in + Pax_migt_out + year, data=dat, family = poisson)
summary(glm.migration_year)
# main effects are similar. variable effect of year on intercept

# plot partial effects with year
plot_model(glm.migration_year, type="pred", terms=c("Pax_migt_in","year"))
# larger effect of 2011 and 2012 than other years
plot_model(glm.migration_year, type="pred", terms=c("Pax_migt_out","year"))
# still not much apparent difference between years

# interaction with year
glm.migration_year_int <- glm(ForPix ~ Pax_migt_in*year + Pax_migt_out*year, data=dat, family=poisson)
summary(glm.migration_year_int)
# so for in-migration, each subsequent year makes the positive slope a bit steeper until 2011 and 2012 after which the direction of the effect changes. With each year the effect of out-migration gets less (flatter curve)

# plot
plot_model(glm.migration_year_int, type="int")


## so accounting for year is really important for in-migration - the direction of the effect changes in 2011.  Year also has an important impact on the effect size of out-migration.  In earlier years, communes with high in-migration were the communes with lots of forest. After 2011, the communes with higher levels of in-migration had low forest cover. I wonder if this reflects a shift in migration patterns from people migrating out to the provinces for work (perhaps to work on plantations?), to people migrating to urban areas to work (textiles? industry?). Across all years, communes with more people migrating out tend to have more forest cover. This what I expected - people leaving the remote, rural communes to move into urban areas in search of work. 

  ## Environmental variables ####
    # mean_elev ####

# plot
ggplot(dat, aes(x=mean_elev, y=ForPix))+
  geom_point()
# looks like a positive relationship

# shouldn't change over time but plot anyway
ggplot(dat, aes(x=mean_elev, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)

# simple model
glm.elev <- glm(ForPix ~ mean_elev, data=dat, family=poisson)
summary(glm.elev)
# positive relationship

# plot
elev_plot <- plot_model(glm.elev, type="pred")

# extract data from above
newelevdat <- data.frame(ForPix = elev_plot$mean_elev$data$predicted,
                          mean_elev = elev_plot$mean_elev$data$x)
                          

# plot the fit with the points
ggplot(NULL, aes(x=mean_elev, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newelevdat, size=1)
# not a great model fit but quite a large effect

# add year
glm.elev_year <- glm(ForPix ~ mean_elev + year, data=dat, family=poisson)
summary(glm.elev_year)
# intercept goes down each year

# plot
plot_model(glm.elev_year, type="pred", terms=c("mean_elev","year"))
# not much change

# interaction
glm.elev_year_int <- glm(ForPix ~ mean_elev * year, data=dat, family=poisson)
summary(glm.elev_year_int)
# years from 2008:2010 increase the effect of elevation, 2011 and 2012 decrease it

# plot
plot_model(glm.elev_year_int, type="int")
# hmm, not really much of an interaction there

# basically, the higher elevation communes have more forest cover. This is what I was expecting - Koh Kong, Mondulkiri, Rattanikiri etc. 

    # habitat ####

# I am unsure about habitat. It made sense when I was using forest loss as a response (ie difference in pixels between years), because habitat could predict where forest was lost. But now I am using just raw forest cover, I am not really sure what habitat would be telling me? Surely it will just be telling me what kind of forest is dominant in forested communes, and what level of mosaic is in less forested communes?

# plot
ggplot(dat, aes(x=habitat, y=ForPix))+
  geom_point()

# split by year
ggplot(dat, aes(x=habitat, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# doesn't look like much change

# simple model
glm.hab <- glm(ForPix ~ habitat, data=dat, family=poisson)
summary(glm.hab)
# all habitats have larger intercept than cropland (unsurprisingly), except grassland. But GL only has one point I think

# plot
plot_model(glm.hab, type="pred")
# Not sure this is telling me much

# add year
glm.hab_year <- glm(ForPix ~ habitat + year, data=dat, family=poisson)
summary(glm.hab_year)
# intercepts for 2008:2011 are larger than 2007. 2012 is lower

# plot
plot_model(glm.hab_year, type="pred", terms=c("habitat","year"))
# virtually no difference between years

# interaction
glm.hab_year_int <- glm(ForPix ~ habitat * year, data=dat, family=poisson)
summary(glm.hab_year_int)

# plot
plot_model(glm.hab_year_int, type="int")
# the only potentially interesting thing here is that the number of forest pixels in a commune that is predominanlty deciduous broadleaf forest goes down in 2011 and 2012. 


    # All environmental vars ####

# simple model
glm.env <- glm(ForPix ~ mean_elev + habitat, data=dat, family=poisson)
summary(glm.env)
# slightly reduced effect size for elevation. Habitat looks similar 

# plot
plot_model(glm.env, type="pred", terms=c("mean_elev","habitat"))
# elevation has no effect unless the commune is broadleaved, or mosaic of natural cover.  Looks like a farily strong interaction

# interaction
glm.env_int <- glm(ForPix ~ mean_elev * habitat, data=dat[!(dat$habitat=="nd" | dat$habitat=="NF"),], 
                   family=poisson)
summary(glm.env_int)
# had to remove nd (no data) and NF (flooded forest) as they were sending the plot haywire and I coudln't see what was going on with the rest

# plot
plot_model(glm.env_int, type="int", terms = c("mean_elev","habitat"))
# so elevation is actually only useful for predicting forest cover when the communes is predominantly crop land, water, or broadleaved deciduous. In communes with other habitats/land covers, elevation makes no difference. This is potentially interesting/useful

# interaction with year too
glm.env_year_int <- glm(ForPix ~ mean_elev * habitat * year, 
                        data=dat[!(dat$habitat=="nd" | dat$habitat=="NF"| dat$habitat=="W"),], 
                        family=poisson)
summary(glm.env_year_int)
# That's a silly number of coefficients

# plot
plot_model(glm.env_year_int, type="int")
# Interesting. So as above, elevation is a good predictor of forest cover only when the commune is predominantly cropland or BLD, until 2011 and 2012. In 2011 and 2012 this effect dramaticlly reduces for BLD communes 

# interaction between vars and year
glm.env_year_int2 <- glm(ForPix ~ mean_elev*year + habitat*year, data=dat, family=poisson)
summary(glm.env_year_int2)

# plot
plot_model(glm.env_year_int2, type="int")
# not much difference from the plots in the individual variable sections
  ## Human additional variables ####
    # dist_border ####

# plot
ggplot(dat, aes(x=dist_border, y=ForPix))+
  geom_point()
# doesn't look like much of a pattern here

# split by year
ggplot(dat, aes(x=dist_border, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)
# some minor changes perhaps

# simple model
glm_border <- glm(ForPix ~ dist_border, data=dat, family=poisson)
summary(glm_border)
# small negative effect

# plot
border_plot <- plot_model(glm_border, type="pred")
# this suggests that communes closer to an international border tend to have more forest cover. This is what I was expecting (e.g. mondulkiri, rattankiri, koh kong, preah vihear, stung treng etc)

# extract data from above
newborderdat <- data.frame(ForPix = border_plot$dist_border$data$predicted,
                          dist_border = border_plot$dist_border$data$x)
                          

# plot the fit with the points
ggplot(NULL, aes(x=dist_border, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newborderdat, size=1)


# add year
glm_border_year <- glm(ForPix ~ dist_border + year, data=dat, family=poisson)
summary(glm_border_year)
# intercept value goes down in each subsequent year from 2007

# plot
plot_model(glm_border_year, type="pred", terms=c("dist_border","year"))
# This suggests the amount of forest cover is decreasing over time (as distance to border doesn't change)

# interaction
glm_border_year_int <- glm(ForPix ~ dist_border * year, data=dat, family=poisson)
summary(glm_border_year_int)

# plot
plot_model(glm_border_year_int, type="int")
# not really much of an interaction

    # dist_provCap ####

# plot
ggplot(dat, aes(x=dist_provCap, y=ForPix))+
  geom_point()
# looks like a positive effect

# split by year
ggplot(dat, aes(x=dist_provCap, y=ForPix))+
  geom_point()+
  facet_wrap(dat$year, nrow=2)

# simple model
glm.provCap <- glm(ForPix ~ dist_provCap, data=dat, family=poisson)
summary(glm.provCap)
# positive effect - i.e. communes further away from the provincial capital have more forest cover

# plot
provCap_plot <- plot_model(glm.provCap, type="pred")

# extract data from above
newprovCapdat <- data.frame(ForPix = provCap_plot$dist_provCap$data$predicted,
                          dist_provCap = provCap_plot$dist_provCap$data$x)
                          

# plot the fit with the points
ggplot(NULL, aes(x=dist_provCap, y=ForPix))+
  geom_point(data=dat)+
  geom_line(data=newprovCapdat, size=1)
# remote communes (far away from provincial capital) have way more forest 

# add year
glm.provCap_year <- glm(ForPix ~ dist_provCap + year, data=dat, family=poisson)
summary(glm.provCap_year)
# year has negative effect on intercept relative to 2007

# plot 
plot_model(glm.provCap_year, type="pred", terms=c("dist_provCap","year"))
# very little difference

# interaction
glm.provCap_year_int <- glm(ForPix ~ dist_provCap * year, data=dat, family=poisson)
summary(glm.provCap_year_int)
# each year increases the effect size of dist_provCap by a very small amount

# plot
plot_model(glm.provCap_year_int, type="int")
# 2011 and 2012 have larger effect on slope than other years
    # elc ####

# plot
ggplot(dat, aes(x=elc, y=ForPix))+
  geom_boxplot()
# unsurprisingly, communes with lots of forest are more likely to have ELCs

# split by year
ggplot(dat, aes(x=elc, y=ForPix))+
  geom_boxplot()+
  facet_wrap(dat$year, nrow=2)
# I think you can see an increase in communes with ELCs over time, and that those communes tend to be more forested

# simple model
glm.elc <- glm(ForPix ~ elc, data=dat, family=poisson)
summary(glm.elc)
# positive relationship between elc==1 and forest cover

# plot
plot_model(glm.elc, type="pred")

# add year
glm.elc_year <- glm(ForPix ~ elc + year, data=dat, family=poisson)
summary(glm.elc_year)
# intercept goes down in subsequent years

# plot
plot_model(glm.elc_year, type="pred", terms=c("elc","year"))
# this is interesting. I think this shows that generally forest cover is decreasing over time, but that it is decreasing faster in communes with ELCs. This is not a surprise, but nice to see it in the data

# interaction
glm.elc_year_int <- glm(ForPix ~ elc * year, data=dat, family=poisson)
summary(glm.elc_year_int)
# 2008 and 2009 reduce the effect size of ELC, but 2010:2012 increase it

# plot
plot_model(glm.elc_year_int, type="int")
# changes the interpretation a bit. 
    # PA ####

# plot 
ggplot(dat, aes(x=PA, y=ForPix))+
  geom_boxplot()
# this is what I expected. Although does show that there are a lot of forested communes outside of PAs

# split by year
ggplot(dat, aes(x=PA, y=ForPix))+
  geom_boxplot()+
  facet_wrap(dat$year, nrow=2)
# can't see any big changes

# simple model
glm.pa <- glm(ForPix ~ PA, data=dat, family=poisson)
summary(glm.pa)
# PA ==1 has positive effect on forest cover

# plot
plot_model(glm.pa, type="pred")

# add year
glm.pa_year <- glm(ForPix ~ PA + year, data=dat, family=poisson)
summary(glm.pa_year)
# year has negative impact on intercept

# plot
plot_model(glm.pa_year, type="pred", terms=c("PA","year"))
# this also seems to show forest loss over time, but interestingly suggests that forest loss is greatest in communes with PAs

# interaction
glm.pa_year_int <- glm(ForPix ~ PA * year, data=dat, family=poisson)
summary(glm.pa_year_int)
# 2008:2010 year has positive impact on effect of PA, 2011 and 2012 have negative

# plot
plot_model(glm.pa_year_int, type="int")
# in communes with no PA, year has no real effect. In communes with PAs, this suggests that 2010, 2011 and 2012 had increased forest loss that in previous years

    # PA_cat ####

# plot
ggplot(dat, aes(x=PA_cat, y=ForPix))+
  geom_boxplot()
# this shows that communes with multiple PA categories have the most forest, followed by WIldlife Sanctuaries and national parks. Protected landscapes and multiple-use areas don't look that different to None, but I think this is beacause the are used to protect areas like the Tonle Sap, where there isn't much forest cover. 

# split by year
ggplot(dat, aes(x=PA_cat, y=ForPix))+
  geom_boxplot()+
  facet_wrap(dat$year, nrow=2)
# we can see the number of communes with MULTI categories increases in 2010 (reducing median forest cover). Ramsar sites change in 2011, pulling median forest cover up. national parks and wildlife sanctuaries don't change much

# simple model
glm.pacat <- glm(ForPix ~ PA_cat, data=dat, family=poisson)
summary(glm.pacat)

# plot
plot_model(glm.pacat, type="pred")
# MULTI predicts most forest, then WS, NP, RMS, None, MUA, PL

# add year
glm.pacat_year <- glm(ForPix ~ PA_cat + year, data=dat, family=poisson)
summary(glm.pacat_year)

# plot
plot_model(glm.pacat_year, type="pred", terms=c("PA_cat", "year"))
# the categories that predict the most forest (MULTI,WS,NP,RMS) predict lower forest cover in subsequent years, suggesting forest loss in these communes

# interaction
glm.pacat_year_int <- glm(ForPix ~ PA_cat * year, data=dat, family=poisson)
summary(glm.pacat_year_int)

# plot
plot_model(glm.pacat_year_int, type="int")
