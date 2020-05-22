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
library(plotly)
library(patchwork)
library(lattice)
library(reshape2)

# load data
dat <- read.csv("Data/commune/dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

str(dat)


#### data exploration ---------------------------------------------------------------
  ## Note ####

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
# so prop_ind has an important relationship to forest cover but time doesn't really

# try 3d plot

# create new data
propindnewdata <- expand.grid(year = seq(from=2007, to=2012, by=1),
                              prop_ind = seq(from=0, to=1, length=6))

# predict
prop_ind_pred <- predict(glm.prop_ind_year, newdata=propindnewdata, type="response", se=T)
propindnewdata <- cbind(propindnewdata,prop_ind_pred)


p <- plot_ly(propindnewdata, x=~prop_ind, y=~year, z=~fit, opacity=0.6) %>% 
  add_markers()
# So we can see that year doesn't have much of an effect, but that at very high values of prop_ind, year has a slightly positive effect
    
#
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
# pop_den coefficient doesn't change. year has a positive effect

# plot
plot_model(glm.pop_den_year, type="pred")

# model with year as interaction
glm.pop_den_year.int <- glm(ForPix ~ pop_den * year, data=dat, family=poisson)
summary(glm.pop_den_year.int)
# pop_den is negative, year is positive, and year has a positive impact on the pop_den effect

# plot interaction model
plot_model(glm.pop_den_year.int, type="pred", terms = c("year","pop_den[0.3,127,2201]"))
# very weak interaction

# 3D plot
# create new data
popdennewdata <- expand.grid(year = seq(from=2007, to=2012, by=1),
                              pop_den = seq(from=0, to=2201, length=6))

# predict
popden_pred <- predict(glm.pop_den_year.int, newdata=popdennewdata, type="response", se=T)
popdennewdata <- cbind(popdennewdata,popden_pred)


plot_ly(popdennewdata, x=~pop_den, y=~year, z=~fit, opacity=0.6) %>% 
  add_markers()


    # All demographic vars ####


# fit simple glm model with all demographic variables
glm.demog <- glm(ForPix ~ tot_pop + prop_ind + pop_den, data=dat, family=poisson)
summary(glm.demog)
# including all vars appears to have reversed the sign of the tot_pop effect (from negative to positive)  

# add year
glm.demog_year <- glm(ForPix ~ tot_pop + prop_ind + pop_den + year, data=dat, family=poisson)
summary(glm.demog_year)
# tot_pop now positive, prop_ind still positive, pop_den still negative, year positive

# allow interactions between all vars and year
glm.demog_year_int <- glm(ForPix ~ year*tot_pop + year*prop_ind + year*pop_den, 
                          data=dat, family=poisson)
summary(glm.demog_year_int)

# plot partial effects with interactions
plot_model(glm.demog_year_int, type="int")
# small interaction between year and pop_den - year has no effect on forest in communes with high pop_den (but the communes basically have no forest to start with), but communes with low pop_den values start with a lot more forest, and there is a small negative slope over time.  Not really much of an interaction at all between year and prop_ind - the lines are essentially parallel.  There is an interaction between tot_pop and year - confusingly in communes with large populations, forest cover is predicted to increase over time, as is forest in communes with low population (but much flatter slope)

# remove pop_den to see if direction of effect of tot_pop changes
glm.demog_nopopden <- glm(ForPix ~ year*tot_pop + year*prop_ind, data=dat, family=poisson)
summary(glm.demog_nopopden)
# yes - so tot_pop is a negative slope again

# plot
plot_model(glm.demog_nopopden, type="int")
# more of an interaction effect with prop_ind and year with pop_den removed. relationship between tot_pop and ForPix makes much more sense now - decreasing in communes with low populations, and jsut flat low starting value) for communes with large population


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
# M6_24_sch effect not really changed. year has negative effect

# plot
plot_model(glm.edu_year, type="pred", terms=c("year","M6_24_sch"))
# forest cover decreasing over time, and communes with lower proportions of males in school start with higher values. Doesn't look like much of an interaction effect

# interaction
glm.edu_year_int <- glm(ForPix ~ year*M6_24_sch, data=dat, family=poisson)
summary(glm.edu_year_int)
# the effect of year decreases with increasing proportion of males in school

# plot it
plot_model(glm.edu_year_int, type="int")
# large interaction effect - communes with a low number of males in school will have increasing forest cover over time, whereas communes with high proportions of males in school will have decreasing forest cover. This is counterintuitive, so not really sure what's happening.

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
# effect size of propPrimSec has increased. Year has a smaller, positive effect.

# plot
plot_model(glm.PrimSec_year, type="pred", terms=c("year", "propPrimSec[0.01,0.87,1]"))
# the effect of year in commnes with low propPrimSec is very small, but as propprimSec increases, the effect gets stronger. 


# interaction
glm.PrimSec_year_int <- glm(ForPix ~ propPrimSec * year, data=dat, family=poisson)
summary(glm.PrimSec_year_int)
# The direction of the effect has reversed for both propPrimSec and year. When year increases, the propPrimSec slope gets slightly flatter


# plot interaction
plot_model(glm.PrimSec_year_int, type="pred", terms=c("year","propPrimSec[0.01,0.87,1]"))
# Quite a large interaction effect. In communes with very low propPriSec values, forest goes down over time.  In communes with higher propPrimSec values, forest is predicted to go up

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
# similar effect for propSecSec, year has ver small negative effect

# plot
plot_model(glm.SecSec_year, type="pred", terms=c("year","propSecSec[0,0.008,0.8]"))
# doesn't look like there#s much of an interaction. Comunes with low propSecSec hve more forest


# interacion with year
glm.SecSec_year_int <- glm(ForPix ~ propSecSec * year, data=dat, family=poisson)
summary(glm.SecSec_year_int)
# the propSecSec negative effect gets steeper, as does the year effect. interaction suggest as time goes on, the propSecSec slope gets less steep


# plot
plot_model(glm.SecSec_year_int, type="pred", terms=c("year","propSecSec[0,0.008,0.8]"))
# woah. This is silly - something strange going on with 2011 and 2012. 

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
newprimdata <- expand.grid(year = c(2007,2008, 2009, 2010, 2011, 2012),
                          propPrimSec = seq(0.01,1, length.out = 100),
                          propSecSec = 0.008)

newsecdata <- expand.grid(year = c(2007,2008, 2009, 2010, 2011, 2012),
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
plot_model(glm.prim_sec_year_in, type="pred", 
           terms=c("year","propSecSec","propPrimSec[0,0.5,1]"))

# Some pretty gnarli interactions here.

# interaction beteen each var and year
glm.prim_sec_year_in2 <- glm(ForPix ~ year*propPrimSec + year*propSecSec, 
                             data=dat, family=poisson)
summary(glm.prim_sec_year_in2)
  
# plot partial effects with year interaction
plot_model(glm.prim_sec_year_in2, type="pred", terms=c("year","propPrimSec"))
plot_model(glm.prim_sec_year_in2, type="pred", terms=c("year","propSecSec"))


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
# slight incrase in rice effect. year has small positive effect

# create new data
newriceYeardat <- expand.grid(year = c(2007,2008,2009,2010,2011,2012),
                              Les1_R_Land = seq(0,1,length.out = 100))

# predict
riceYearpred <- predict(glm.rice_year, newdata=newriceYeardat, type = "response", se=T)
newriceYeardat <- cbind(newriceYeardat,riceYearpred)

# plot
ggplot(newriceYeardat, aes(x=Les1_R_Land, y=fit, group=year, colour=year))+
  geom_line()
#  

# interaction with year
glm.rice_year_int <- glm(ForPix ~ Les1_R_Land*year, data=dat, family=poisson)
summary(glm.rice_year_int)
# the slope for rice gets steeper


# plot
plot_model(glm.rice_year_int, type="pred", terms=c("year","Les1_R_Land[0,0.35,1]"))
# not much of an interaction effect.   


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
# reduced effect size for pig, year has small positive effect

# plot
plot_model(glm.pig_year, type="pred", terms=c("year", "pig_fam"))


# year as interaction
glm.pig_year_int <- glm(ForPix ~ pig_fam * year, data=dat, family = poisson)
summary(glm.pig_year_int)
# changes direction of pig_fam effect. Year has negative effect

# plot
plot_model(glm.pig_year_int, type="pred", terms=c("year","pig_fam"))
# There is an interaction effect - in communes with low proportion of families keeping pigs, forest cover goes down.  Communes with medium or high proportions, forest cover goes up


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

# plot
plot_model(glm.pigrice_year, type="pred", terms = c("year","Les1_R_Land","pig_fam"))
# looks like 2011 and 2012 are different

# test interactions
glm.pigriceyear_int <- glm(ForPix ~ pig_fam * Les1_R_Land * year, data=dat, family=poisson)
summary(glm.pigriceyear_int)

# plot
plot_model(glm.pigriceyear_int, type="pred", terms = c("year","Les1_R_Land","pig_fam"))
plot_model(glm.pigriceyear_int, type="pred", terms = c("year","pig_fam","Les1_R_Land"))

# summary here is that both pig_fam and Les1_R_Land cause time to have a positive effect on forest cover. The higher the pig_fam value, the steeper the slope, although this is dampened by rice value (i.e. higher rice values have lower forest cover) 

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
glm.sch_year <- glm(ForPix ~ dist_sch + year, data=dat, family=poisson)
summary(glm.sch_year)
# dist_sch and year both positive

# plot 
plot_model(glm.sch_year, type="pred", terms = c("year","dist_sch[0,8.5,83]"))
# small distances to schools mean time has positive effect on forest, but mean and max distance values mean year has negligble effects

# add interaction
glm.sch_year_int <- glm(ForPix ~  year * dist_sch , data=dat, family=poisson)
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
plot_model(glm.garbage_year, type="pred", terms = c("year","garbage"))
# virtually no difference in slope

# test interaction
glm.garbage_year_int <- glm(ForPix ~ year * garbage, data=dat, family=poisson)
summary(glm.garbage_year_int)


# plot
plot_model(glm.garbage_year_int, type="pred", terms=c("year","garbage[0,0.006,1]"))
# so at low/mean values of garbage, time has small negative effect on forest cover.  At max levels of garbage, time has a slight positive effect, particulalry after 2010


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
glm.Comm_year <- glm(ForPix ~ year + KM_Comm , data=dat, family=poisson)
summary(glm.Comm_year)
# year is negative, KM_Comm still positive

# plot 
plot_model(glm.Comm_year, type="pred", terms = c("year","KM_Comm[0,3.3,42]"))
# at low/mean values of KM_Comm, year has basically no effect on forest. At high values of KM_Comm, the communes start with more forest but the effect of year is negative

# add interaction
glm.Comm_year_int <- glm(ForPix ~ year * KM_Comm, data=dat, family=poisson)
summary(glm.Comm_year_int)


# plot
plot_model(glm.Comm_year_int, type="pred", terms=c("year","KM_Comm[0,3.3,42]"))
# as above

## take home message - the communes where the median distance from a given village to the commune office is large, have more forest.  This probably, to some degree, reflects the fact that the more remote communes that are larger and in larger provinces, do tend to have more forest. But these communes also lose forest over time.  In communes where the median distance to commune offfice is low, they tend to hvae very little forest and year has virtually no effect

    # All access to services vars ####

# simple model with all vars
glm.AccServ <- glm(ForPix ~ dist_sch + garbage + KM_Comm, data=dat, family=poisson)
summary(glm.AccServ)
# similar effect for dist_sch (small, positive), similar effect for garbage but reduced effect size (negative), similar effect for KM_Comm (small, positive)

# plot partial effects
plot_model(glm.AccServ, type="pred")

# add year
glm.AccServ_year <- glm(ForPix ~ year + dist_sch + garbage + KM_Comm, data=dat, family=poisson)
summary(glm.AccServ_year)
# year has positive effect

# plot
plot_model(glm.AccServ_year, type="pred", terms=c("year","dist_sch[0,8.5,83]"))
plot_model(glm.AccServ_year, type="pred", terms=c("year","garbage[0,0.006,1]"))
plot_model(glm.AccServ_year, type="pred", terms=c("year","KM_Comm[0,3.3,42]"))

# test interactions between vars (no year)
glm.AccServ_int <- glm(ForPix ~ garbage * dist_sch * KM_Comm, data=dat, family=poisson)
summary(glm.AccServ_int)

# plot
plot_model(glm.AccServ_int, type="int")

# interactions with year
glm.AccServ_int_year <- glm(ForPix ~  year*dist_sch + year*KM_Comm + year*garbage, 
                            data=dat, family=poisson)
summary(glm.AccServ_int_year)

# plot
plot_model(glm.AccServ_int_year, type="int")
# Partial effects with interaction with year shows that year has a positive effect when dist_sch is high, but no effect when it is low. This is also true of KM_Comm, although the positive effect of year when KM_Comm is high is quite strong (steep slope).when garbage values are low, ther eis a small positive effect from year, and when garbage value is low, the initial forest cover is low but there is an increasing positive effect with time.


# test 3-way interactions
glm.AccServ_int_year2 <- glm(ForPix ~  year * dist_sch * KM_Comm, 
                            data=dat, family=poisson)
summary(glm.AccServ_int_year2)

plot_model(glm.AccServ_int_year2, type="int")
#  when distances to KM_Comm are low, and dist_sch values are low, time has no effect on forest. When KM_Comm values are low but dist_sch values are high, year has positive effect on forest. When KM_Comm values are high, and dist_sch values are low, year has a small positive effect on forest. When KM_Comm values are high and dist_sch values are high, year has small negative effect.  


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
glm.crim_year <- glm(ForPix ~  year + crim_case, data=dat, family=poisson)
summary(glm.crim_year)
# woah. When year is accounted for, effect of crim_case is massive. year effect is negative

plot_model(glm.crim_year, type="pred", terms=c("year","crim_case[0,0.002,0.05]"))
# no differnece in slopes, but communes with higher values of crim_case have more forest

# test interaction
glm.crim_year_int <- glm(ForPix ~ year * crim_case, data=dat, family = poisson)
summary(glm.crim_year_int)

# plot
plot_model(glm.crim_year_int, type="pred", terms=c("year","crim_case[0,0.002,0.05]"))
# when crim_case values are low/mean then year has no effect (very small negative), but when crim_case values are high, year has very strong positive effect on forest

## main message at the moment is that as the criminal case per captia increases, so does forest cover. This is the opposite of what I was expecting. I was expecting the more densely populated, urban areas with less forest to have higher per capita crime.

# plot crim_case split by province to check my hypithesis
ggplot(dat, aes(crim_case))+
  geom_histogram()+
  facet_wrap(dat$Province, nrow=4)
# ok so provinces like Koh Kong, Rattanikiri, Stung Treng do have crim-case values that look higher than say, Phnom Penh. 


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
glm.confl_year <- glm(ForPix ~ year + land_confl, data=dat, family=poisson)
summary(glm.confl_year)

# plot
plot_model(glm.confl_year, type="pred", terms=c("year","land_confl[0,16,318]"))
# communes with low land conflict have more forest, but year has negative effect. Comunes with high land conflict have less forest, and year has a very small negative effect

# add interaction
glm.confl_year_int <- glm(ForPix ~ year * land_confl, data=dat, family=poisson)
summary(glm.confl_year_int)


# plot
plot_model(glm.confl_year_int, type="pred", terms=c("year","land_confl[0,16,318]"))
# with the interaction, now communes with high land conflict have positive effect over time




    # All social justice vars ####

# simple model
glm.SocJus <- glm(ForPix ~ crim_case + land_confl, data=dat, family=poisson)
summary(glm.SocJus)
# large positive effect of crim_case, small negative effect of land_confl

# plot partial effects
plot_model(glm.SocJus, type="pred")

# add year
glm.SocJus_year <- glm(ForPix ~ year + crim_case + land_confl, data=dat, family=poisson)
summary(glm.SocJus_year)
# same trends

# plot partial effects with year
plot_model(glm.SocJus_year, type="pred", terms=c("year","crim_case"))
plot_model(glm.SocJus_year, type="pred", terms=c("year","land_confl"))


# add interactions with year
glm.SocJus_year_int <- glm(ForPix ~ year*crim_case + year*land_confl, data=dat, family=poisson)
summary(glm.SocJus_year_int)

# plot
plot_model(glm.SocJus_year_int, type="pred", 
           terms=c("year", "land_confl[0,16,318]","crim_case[0,0.002,0.05]"))
# When crim_case is low or mean, year has little effect, regardles of land conflict. But when crim_cases are high, year has a large positive effect, with differences in starting values and slopes depending on land conflict.



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
# similar effect of migt it, year is positive

# plot
plot_model(glm.migin_year, type="pred", terms=c("year","Pax_migt_in[0,117,2553]"))
# in communes with low/mean in-migration, forest cover start high and increases. In communes with very high in migration, forest cover is low but very slowly increases

# add interaction
glm.migin_year_int <- glm(ForPix ~ year * Pax_migt_in, data=dat, family=poisson)
summary(glm.migin_year_int)
# direciton of migt effect changes to positive, and migt has a negative impact on the effect of year

# plot
plot_model(glm.migin_year_int, type="pred", terms=c("year","Pax_migt_in[0,117,2553]"))
# Interesting. now in communes with very high in migration the effect of year is strongly negative, and in communes with min migt, effect is weakly positive, and with mean migt there is no year effect

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
glm.migout_year <- glm(ForPix ~ year + Pax_migt_out, data=dat, family=poisson)
summary(glm.migout_year)
# migt still negative, year positive

# plot
plot_model(glm.migout_year, type="pred", terms=c("year","Pax_migt_out[0,86,3132]"))


# add interaction
glm.migout_year_int <- glm(ForPix ~ year * Pax_migt_out, data=dat, family=poisson)
summary(glm.migout_year_int)
# migt negative effect size larger

# plot
plot_model(glm.migout_year_int, type="pred", terms=c("year","Pax_migt_out[0,86,3132]"))
# communes with very high out-migration have low forest cover and year has no effect. in communes with low/mean out-migt then year has a positive effect

    # All migration vars ####

# simple model
glm.migration <- glm(ForPix ~ Pax_migt_in + Pax_migt_out, data=dat, family = poisson)
summary(glm.migration)
# in-migration direction reversed to small positive. out-mig effect very similar

# plot partial effects
plot_model(glm.migration, type="pred")

# add year
glm.migration_year <- glm(ForPix ~ year + Pax_migt_in + Pax_migt_out, 
                          data=dat, family = poisson)
summary(glm.migration_year)


# plot partial effects with year
plot_model(glm.migration_year, type="pred", terms=c("year","Pax_migt_in[0,117,2553]"))
# year now has positive effect for all values of migt in
plot_model(glm.migration_year, type="pred", terms=c("year","Pax_migt_out[0,86,3132]"))
# high values of migt out mean year has no effect, low/mean value make year have positive effect

# interaction with year
glm.migration_year_int <- glm(ForPix ~ year*Pax_migt_in + year*Pax_migt_out, 
                              data=dat, family=poisson)
summary(glm.migration_year_int)


# plot partial effects with interaction
plot_model(glm.migration_year_int, type="int")
# migt out means year has no effect at all, high migt in values makes year have neg effect, low values make year have slightly positive effect


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
glm.elev_year <- glm(ForPix ~ year + mean_elev, data=dat, family=poisson)
summary(glm.elev_year)
# year negative, elev positive

# plot
plot_model(glm.elev_year, type="pred", terms=c("year","mean_elev[3,72,740]"))
# at low/mean elevations there is no effect over time, but high elevation give year a negative effect

# interaction
glm.elev_year_int <- glm(ForPix ~ year * mean_elev, data=dat, family=poisson)
summary(glm.elev_year_int)


# plot
plot_model(glm.elev_year_int, type="pred", terms=c("year","mean_elev[3,72,740]"))
# same as above

# basically, the higher elevation communes have more forest cover. This is what I was expecting - Koh Kong, Mondulkiri, Rattanikiri etc. And it is these communes that are losing forest over time, whereas lower elevation communes don't have that much forest in the first place, and the loss is less

    # habitat ####

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
glm.hab_year <- glm(ForPix ~ year + habitat, data=dat, family=poisson)
summary(glm.hab_year)
# year has negative effect

# plot
plot_model(glm.hab_year, type="pred", terms=c("year","habitat"))
# habitat has no impact on the effect of year by the looks of it

# interaction
glm.hab_year_int <- glm(ForPix ~ year * habitat, data=dat, family=poisson)
summary(glm.hab_year_int)

# plot
plot_model(glm.hab_year_int, type="pred", terms=c("year","habitat"))
# some weak looking interactions.  Interestingly, broadleaved deciduous forest causes year to have a negative effect, as does flooded natural cover, and to a lesser degree natuarl mosaic. Surprisingly broadleaved evergreen has small positive effect 


    # All environmental vars ####

# simple model
glm.env <- glm(ForPix ~ mean_elev + habitat, data=dat, family=poisson)
summary(glm.env)
# slightly reduced effect size for elevation. Habitat looks similar 

# plot
plot_model(glm.env, type="pred", terms=c("mean_elev","habitat"))
# elevation has large effect when habitat is FBD, FBE, Mosaic, and a lesser effect if habitat is MC, MN, NF, CP. GL has no effect which makes sense

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
plot_model(glm.env_year_int, type="pred", terms=c("year", "mean_elev[3,72,740]", "habitat"))
# I think I'd need to change the y-axis scale to see whats going on. But, you can see that in high elevation cropland, year has a positive effect, and in high elevation FBD year has a negatvie effect 

# interaction between vars and year
glm.env_year_int2 <- glm(ForPix ~ year*mean_elev + year*habitat, data=dat, family=poisson)
summary(glm.env_year_int2)

# plot
plot_model(glm.env_year_int2, type="pred", terms=c("year", "mean_elev[3,72,740]", "habitat"))
# Ok this makes more sense. year has no real effect at any elevation, unless it is FBD, FBE, Mosaic natural (or no data). 


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
glm_border_year <- glm(ForPix ~ year + dist_border, data=dat, family=poisson)
summary(glm_border_year)
# dist_border still negative, year also negative

# plot
plot_model(glm_border_year, type="pred", terms=c("year","dist_border"))
# demonstrates that forest cover is going down over time, and that communes in the middle of the country have less forest than those on the edges

# interaction
glm_border_year_int <- glm(ForPix ~ year * dist_border, data=dat, family=poisson)
summary(glm_border_year_int)

# plot
plot_model(glm_border_year_int, type="pred", terms=c("year","dist_border[0.4,63,179]"))
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
glm.provCap_year <- glm(ForPix ~ year + dist_provCap, data=dat, family=poisson)
summary(glm.provCap_year)
# year has negative effect 

# plot 
plot_model(glm.provCap_year, type="pred", terms=c("year","dist_provCap[1,32,92]"))
# communes that are remote and further away from ProvCap have more forest, but negative effect of year. no real efect of year for commmunes clsoe to provcap

# interaction
glm.provCap_year_int <- glm(ForPix ~ year * dist_provCap, data=dat, family=poisson)
summary(glm.provCap_year_int)

# plot
plot_model(glm.provCap_year_int, type="pred", terms=c("year","dist_provCap[1,32,92]"))
# interaction has reversed the slope for communes far away from provcap


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
glm.elc_year <- glm(ForPix ~ year + elc, data=dat, family=poisson)
summary(glm.elc_year)
# year has negative effect

# plot
plot_model(glm.elc_year, type="pred", terms=c("year","elc"))
# year has negative effect regardless of ELC presence, but in communes where there is an ELC, the slope is steeper, which is what I would expect


# interaction
glm.elc_year_int <- glm(ForPix ~ year * elc, data=dat, family=poisson)
summary(glm.elc_year_int)


# plot
plot_model(glm.elc_year_int, type="pred", terms=c("year","elc"))
# This makes less sense. THe slope is reversed for communes with ELC, as now it is positive


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
glm.pa_year <- glm(ForPix ~ year + PA, data=dat, family=poisson)
summary(glm.pa_year)
# year has negative effect

# plot
plot_model(glm.pa_year, type="pred", terms=c("year","PA"))
# Communes with PAs have more forest, but both slopes are negative

# interaction
glm.pa_year_int <- glm(ForPix ~ year * PA, data=dat, family=poisson)
summary(glm.pa_year_int)


# plot
plot_model(glm.pa_year_int, type="pred", terms=c("year","PA"))
# in communes with no PA, year has no real effect. In communes with PAs, year has a negative effect, which is not a great message for conservation!

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
glm.pacat_year <- glm(ForPix ~ year + PA_cat, data=dat, family=poisson)
summary(glm.pacat_year)
# year has negative efffect

# plot
plot_model(glm.pacat_year, type="pred", terms=c("year","PA_cat"))
# the categories that predict the most forest are MULTI, WS, NP, RMS 

# interaction
glm.pacat_year_int <- glm(ForPix ~ year * PA_cat, data=dat, family=poisson)
summary(glm.pacat_year_int)

# plot
plot_model(glm.pacat_year_int, type="pred", terms=c("year","PA_cat"))
# ok so in communes with MULTI, year has strong negative effect.  WS very small negative, and NP very small positive, RMS positive.  PL also positive, and MUA small negative.




### Need to look properly to see if there are any communes that gain forest pixels over time
#### Centering all variables ####

# following from Harrison et al 2018 and Schielzeth 2010, I will mean centre all (numerical) input variables
str(dat)

dat1 <- dat %>% 
        mutate(tot_pop = tot_pop-mean(tot_pop)) %>% 
        mutate(prop_ind = prop_ind-mean(prop_ind)) %>% 
        mutate(pop_den = pop_den-mean(pop_den)) %>% 
        mutate(M6_24_sch = M6_24_sch-mean(M6_24_sch)) %>% 
        mutate(propPrimSec = propPrimSec-mean(propPrimSec)) %>% 
        mutate(propSecSec = propSecSec-mean(propSecSec)) %>% 
        mutate(Les1_R_Land = Les1_R_Land-mean(Les1_R_Land)) %>% 
        mutate(pig_fam = pig_fam-mean(pig_fam)) %>% 
        mutate(dist_sch = dist_sch-mean(dist_sch)) %>% 
        mutate(garbage = garbage-mean(garbage)) %>% 
        mutate(KM_Comm = KM_Comm-mean(KM_Comm)) %>% 
        mutate(land_confl = land_confl-mean(land_confl)) %>% 
        mutate(crim_case = crim_case-mean(crim_case)) %>% 
        mutate(Pax_migt_in = Pax_migt_in-mean(Pax_migt_in)) %>% 
        mutate(Pax_migt_out = Pax_migt_out-mean(Pax_migt_out)) %>% 
        mutate(mean_elev = mean_elev-mean(mean_elev)) %>% 
        mutate(dist_border = dist_border-mean(dist_border)) %>% 
        mutate(dist_provCap = dist_provCap-mean(dist_provCap)) 

str(dat1)

ggplot(melt(dat1),aes(x=value)) + geom_histogram() + facet_wrap(~variable)

#### Mixed models -----------------------------------------------------------
  ## experimenting ####

# try varying intercept model with no predictors 
m0 <- lmer(ForPix ~ 1 + (1|Commune), data = dat)
display(m0)
coef(m0) # intercepts for the communes

ranef(m0) # shows how much the intercept for each commune goes up or down from the mean

interc <- coef(m0) # extract intercepts

# put into dataframe with commune names
interc.df <- data.frame(commune = rownames(interc$Commune),
                        intercept = interc$Commune$`(Intercept)`)

# plot
ggplot(interc.df, aes(x=commune, y=intercept))+
  geom_point()


# now try with commune nested inside province
m0.1 <- lmer(ForPix ~ 1 + (1|Province/Commune), data = dat)
display(m0.1)

interc.prov <- coef(m0.1)
interc.prov.df <- data.frame(commune = rownames(interc.prov$Commune),
                        intercept = interc.prov$Commune$`(Intercept)`)

head(interc.prov.df)
ggplot(interc.prov.df, aes(x=commune, y=intercept))+
  geom_point()
# now the intercepts seem to be able to go below 0, so is these intercepts against some reference level?


# now try with a single predictor
m1.tot_pop <- lmer(ForPix ~ tot_pop + (1|Province), data=dat)
display(m1.tot_pop)

coef(m1.tot_pop) # intercepts and (fixed) slopes for the Provinces

fixef(m1.tot_pop) # intercept and slope for tot_pop
ranef(m1.tot_pop) # Difference in intercept for each province from the mean

# 95% CIs for the slope (which in this model does not vary by Province)
fixef(m1.tot_pop)["tot_pop"] + c(-2,2)*se.fixef(m1.tot_pop)["tot_pop"]

# extract intercepts and slopes 
interc.prov <- coef(m1.tot_pop)
interc.prov.df <- data.frame(province = rownames(interc.prov$Province),
                        intercept = interc.prov$Province$`(Intercept)`,
                        tot_pop = interc.prov$Province$tot_pop)
interc.prov.df$fit <- predict(m1.tot_pop)


plot(m1.tot_pop)
hist(residuals(m1.tot_pop))
qqnorm(residuals(m1.tot_pop))
plot_model(m1.tot_pop, type = "diag")
plot_model(m1.tot_pop, type="re")
plot_model(m1.tot_pop, type="pred", terms=c("tot_pop","Province"))


  ## Random effects structure ####

# according to Zuur et al 2009 and Barr et al 2015a, a good approach for establishing your random effects structure is to include all fixed effects (maximal / above optimal model) and then test different random effects structures (using REML)

# Commune - this wants to be a random effect because of repreat measurements (year), because there are a LOT of levels and so would eat up a lot of degrees of freedom as a fixed effect.  My one concern is that I have read that a variable should be a random effect if it is a sample of the 'global' population, which in this case is not true - I have all of the communes in the country (where there is forest), and so this IS the gobal population. Not sure if this is something to worry about.

# Province - this is a random effect, and commune should be nested inside Province (ie a commune can only feature in one Province). I wonder whether communes with the same name but in different provinces is going to be an issue here?

# year - I think year needs to be a crossed randome effect, as communes appear in multiple years. But Jeoren mentioned that year should also be a fixed effect interacting with all other fixed effects to allow for the slope to vary by year

# Habitat - I want to test whether intercepts and slopes vary by habitat, and so this should be a random effect. I think it should be a crossed effect, because a province (and in theory a commune) can include different habitat types

# Protected area presence (PA) - like habitat, I want to test differences in intercept and slope for effects for communes with and without PAs.  And again, I think this wants to be a crossed effect

# Protected area category (PA_cat) - Same as PA above

# surely I've got too many variables for them all to be included as interactions?!
re.str.1 <- glmer(ForPix ~ )

### to remember ####

# when testing variance components of random effects REML should be used. But when comparing models with differing fixed effects, ML should be used. But then when preenting final model results, use REML.  
