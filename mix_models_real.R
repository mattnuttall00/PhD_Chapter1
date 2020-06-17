### This script is the analysis of the socioeconomic data from the Commune Database, as predictors of forest loss between 2007 and 2012. This will be chapter 1 in my Phd. All data have been cleaned and variables checked for correlation, and relevant variables selected. For data creation, data cleaning, and variable selection, see the "Socioecon_dataClean.R" script in H:PhD_Chapter1

#### Load libraries and data ####

library(tidyverse)
library(arm)
library(sjPlot)
library(DHARMa)
library(lme4)
#library(nlme)
library(Matrix)
#library(rcpp)
library(psych)
library(visreg)
library(car)
library(ggeffects)
library(plotly)
library(patchwork)
library(lattice)
library(reshape2)
library(pbkrtest)
library(MuMIn)
library(RColorBrewer)

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

# following from Harrison et al 2018 and Schielzeth 2010, I will mean centre and scale all (numerical) input variables
str(dat)
 

dat1 <- dat %>% 
  mutate_at(c("year","tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec","propSecSec","Les1_R_Land",
              "pig_fam","dist_sch","garbage","KM_Comm","land_confl","crim_case","Pax_migt_in",
              "Pax_migt_out","mean_elev","dist_border","dist_provCap"), ~(scale(.) %>% as.vector))


# merge Province and Commune into a new unique variable (to remove issue of communes with the same name)
dat1 <- dat1 %>% mutate(Provcomm = paste(dat1$Province, dat1$Commune, sep = "_"))

# add original year (i.e. not scaled)
dat1$year.orig <- dat$year


#### Mixed models -----------------------------------------------------------
### Approach ####

# I am going to follow the below approach for each set

# 1) Run maximal model (with no interactions) to get a feel for the effects of variables without interactions
# 2) assess the model diagnostics
# 3) assess each variables effect via plotting (main effect plot plus communes with highest intercept)
# 4) run maximal model with interactions (if I think that interactions are plausible and if I have an a priori hypothesis about the interactions) and compare with the first model (LRT, profiled confidence intervals &       parametric bootstrapping)
# 5) conduct model simplification and selection until the best model is selected
# 6) assess model diagnostics
# 7) Plot effects from selected model:
  # a) manual predict from observed data and compare to predict()
  # b) predict main effects, from an "average" commune
  # c) add on predictions from communes with highest intercepts
  # d) plot the communes within the two provinces with the highest RE intercept and lowest RE intercept. 
  # e) select the province with RE intercept closest to 0 and plot all the communes (or subset) in a panel grid with the         global effect plus the effect for that commune
# 8) run simple binomial glm (as per Jeroen's suggestion) to check that the results are vaguely plausible 

  ### Random effects structure ####

# according to Zuur et al 2009 and Barr et al 2015a, a good approach for establishing your random effects structure is to include all fixed effects (maximal / above optimal model) and then test different random effects structures (using REML)

# Commune - this wants to be a random effect because of repreat measurements (year), because there are a LOT of levels and so would eat up a lot of degrees of freedom as a fixed effect. .

# Province - this is a random effect, and commune should be nested inside Province (ie a commune can only feature in one Province). Having Province in teh RE structure adds complexity to the interpretation, but there does appear to be some variance associated with province. And although I am interested in the differences between Provinces, perhaps I am not intersted in the effect of province per se, but more the way in which other things vary by province (see Jeroen's e-mail - he explains it well).

# Year. This is tricky.  See e-mail conversation with Jeroen and his script "basic_analysis_fromJeroen". At the moment I am moving forward with year as a random slope, but not a fixed effect.  


  ## Population demographics ####

# total population (tot_pop), population density (pop_den), proportion indigneous (prop_ind)


    # popdem.m1 (tot_pop, pop_den, prop_ind - no interactions) ####


# model with all pop dem vars a fixed effects. Offset as areaKM to account for differences in size of communes
popdem.m1 <- glmer(ForPix ~ tot_pop + pop_den + prop_ind + offset(log(areaKM)) + (year|Province/Provcomm),
                   data=dat1, family="poisson")


summary(popdem.m1)
# variance for Province and commune are similar. Variance for year at both levels is small, but an order of magnitude smaller for the Province level. Approximate p values are sig for pop_den only. tot_pop is positive effect, pop_den and prop_ind are negative. no real correlation between fixed effects.
# Note: I did run the model without offset and the variance for Province and Provcomm:Province were double what they are now that offset is included. 

fixef(popdem.m1)

ranef(popdem.m1)

# so for this model I have a global intercept, and global slopes for tot_pop, pop_den, and prop_ind.  I also have individual intercepts for each province, and for each commune/province combination, and a random slope for year for each province and each commune.  

# plot the random effects
plot_model(popdem.m1, type="re")


      # diagnostics ####


# USing DHARMa package for diagnostics - first calculate residuals using all RE levels
simulationOutput <- simulateResiduals(fittedModel = popdem.m1, plot = T, 
                                      re.form = ~(year|Province/Provcomm))

plot(simulationOutput)
# the QQ plot shows underdispersion, and the res vs pred plot looks ok (at this scale - see plotResiduals below)

# QQ plot
plotQQunif(simulationOutput)
# According to the DHARMa vignette, this QQ plot shows underdispersion - i.e. too many value around 0.5 and not enough residuals at the tail ends of the distribution. Although on the plot the dispersion test is not sig, but the deviation from the distribution is

# Another test for dispersion
testDispersion(simulationOutput)
# this looks pretty good to me. Perhaps slightly too many in the middle and not enough at the tails

# residual vs predcted plot
plotResiduals(simulationOutput)
# this to me shows fairly major heteroskedasicity - much more residual variance for small values of predicted ForPix
hist(simulationOutput)

# plot residuals from individual predictors
par(mfrow=c(2,2))
plotResiduals(simulationOutput, dat1$tot_pop)
# tot_pop residuals look good
plotResiduals(simulationOutput, dat1$pop_den)
# pop_den residuals show high variation close to zero
plotResiduals(simulationOutput, dat1$prop_ind)
# I suspect prop_ind is the problem - I guess because there are just too many 0's in the prop_ind variable


# test temporal and spatial autocorrelation
testTemporalAutocorrelation(simulationOutput)
# no major problem with temporal autocorr
testSpatialAutocorrelation(simulationOutput)
# I haven't provided any x values. It says random values assigned. If these values were assigned in order, then this should partly work because the communes are listed by province and so communes in the same province should have similar "random" values and so should act as a proxy for x values. 

# test for zero inflation
par(mfrow=c(1,1))
testZeroInflation(simulationOutput)

# testing residuals per commune
simulationOutput.com = recalculateResiduals(simulationOutput, dat1$Provcomm)
# QQ plot
plotQQunif(simulationOutput.com)
# still shows underdispersion

# Another test for dispersion
testDispersion(simulationOutput.com)

# residual vs predcted plot
plotResiduals(simulationOutput.com)
# looks like too many small residuals at lower model predictions.


# testing residuals per province
simulationOutput.prov = recalculateResiduals(simulationOutput, dat1$Province)
# QQ plot
plotQQunif(simulationOutput.prov)
# Not as bad as communes

# Another test for dispersion
testDispersion(simulationOutput.prov)
# looks pretty good to me

# residual vs predcted plot
plotResiduals(simulationOutput.prov)
# Not really enough points but I think that looks ok!




      # predictions (manual, observed data) ####

## manually calculate predictions

# first create subset data
popdem.m1.dat <- select(dat1, ForPix, tot_pop, pop_den, prop_ind, areaKM, year, Province, Provcomm)

# add global intercept
popdem.m1.dat$Iglobal <- fixef(popdem.m1)[["(Intercept)"]]

# add RE intercepts for each Province 
popdem.m1.dat$Iprovince <- ranef(popdem.m1)$Province[,"(Intercept)"][
                            match(popdem.m1.dat$Province, row.names(ranef(popdem.m1)$Province))]

# add Provcomm:Province name column to match the RE output
popdem.m1.dat$Provcomm_P = paste(popdem.m1.dat$Provcomm,popdem.m1.dat$Province,sep=":")

# add RE intercepts for each commune
popdem.m1.dat$Icommune <- ranef(popdem.m1)[[1]][, "(Intercept)"][
                            match(popdem.m1.dat$Provcomm_P, row.names(ranef(popdem.m1)[[1]]))]


# add RE slope for year for province
popdem.m1.dat$b_year_prov <- ranef(popdem.m1)$Province[,"year"][
                              match(popdem.m1.dat$Province, row.names(ranef(popdem.m1)$Province))]

# add random slope for year for commune
popdem.m1.dat$b_year_com <- ranef(popdem.m1)$'Provcomm:Province'[,"year"][
                              match(popdem.m1.dat$Provcomm_P, row.names(ranef(popdem.m1)$'Provcomm:Province'))]

# now add the fixed effects of tot_pop, pop_den, prop_ind
popdem.m1.dat$b_tot_pop <- fixef(popdem.m1)['tot_pop']
popdem.m1.dat$b_pop_den <- fixef(popdem.m1)['pop_den']
popdem.m1.dat$b_prop_ind <- fixef(popdem.m1)['prop_ind']

# add offset. When offset is a term in the model you have to be careful when predicting. You need to be explicit about what values the offset is taking.  In this case though,I am predicting from the observed data, and so the offset is the actual areaKM for each commune from the data.
popdem.m1.dat$offset <- log(dat1$areaKM)[match(popdem.m1.dat$Provcomm, dat1$Provcomm)]

head(popdem.m1.dat)

# y ~ I + (Ip + Ic)|Prov/Comm + tot_pop*b1 + pop_den*b2 + prop_ind*b3 + (year*(cP + Cc)|Prov/Comm)

# (global_intercept+(IProvince+Icommune)_given province/commune) + tot_pop*b_totpops + pop_den*b_pop_den + prop_ind*b_prop_ind + (year.s*(b_year_province+b_year_commune)_given province_commune).

# predict manually
m_popdem.m1_pred <- with(popdem.m1.dat, {
  Iglobal +
  tot_pop*b_tot_pop +
  pop_den*b_pop_den +
  prop_ind*b_prop_ind +
  Iprovince +
  Icommune +
  year*(b_year_prov+b_year_com)+
  offset
})

# compare with predict()
pred_popdem.m1 <- predict(popdem.m1, type = "response")
plot(exp(m_popdem.m1_pred),pred_popdem.m1)
abline(a = 0, b = 1, col = "red")
# predictions match



      # predictions (newdata, tot_pop varying) ####

# create new dataframe for predict (offset as term, so required here). I am predicting for an average commune and so the offset will be set to an "average sized" commune
tot_pop_newdat <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                             pop_den = mean(dat1$pop_den),
                             prop_ind = mean(dat1$prop_ind),
                             areaKM = mean(dat1$areaKM))



# predict for "average" commune
pred_tot_pop_av.m1 <- as.vector(predict(popdem.m1, newdata = tot_pop_newdat, type="response", re.form = NA))

tot_pop_newdat$pred <- pred_tot_pop_av.m1

# plot. I am not adding the original points because the model is predicting forest pixels per unit area (KM)
ggplot(tot_pop_newdat, aes(x=tot_pop))+
    geom_line(aes(y=pred), colour="red", size=1)+
    ylim(0,170)+
    ylab("Forest pixels")+
    xlab("Total population (scaled)")
  


## Let's add some of the communes with the highest intercepts 

commune_re <- ranef(popdem.m1)[[1]]
# Sort this from high to low intercept:
commune_re <- commune_re[order(commune_re[,"(Intercept)"], decreasing = TRUE),]
# So the top five are:
head(commune_re, 5)
# Save the names of these
top5 <- row.names(head(commune_re, 5))

# make sure to set the areaKM to the correct value for the selected commune
top5_newdat <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                          pop_den = mean(dat1$pop_den),
                          prop_ind = mean(dat1$prop_ind),
                          year = mean(dat1$year),
                          Province = "Kampong Chhnang",
                          Provcomm = "Kampong Chhnang_Chieb",
                          areaKM = unique(dat1$areaKM[dat1$Provcomm=="Kampong Chhnang_Chieb"]))

pred_topcom1 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                        newdata = top5_newdat))

top5_newdat$pred <- pred_topcom1

# plot
ggplot()+
  geom_line(data=tot_pop_newdat, aes(x=tot_pop, y=pred_tot_pop_av.m1), colour="red", size=1)+
  geom_line(data=top5_newdat, aes(tot_pop, y=pred), colour="blue", size=1)+
  ylim(0,2300)+
  ylab("Forest pixels")+
  xlab("Total population (scaled)")

# next
top5_newdat1 <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                          pop_den = mean(dat1$pop_den),
                          prop_ind = mean(dat1$prop_ind),
                          year = mean(dat1$year),
                          Province = "Kampot",
                          Provcomm = "Kampot_Preaek Tnaot",
                          areaKM = unique(dat1$areaKM[dat1$Provcomm=="Kampot_Preaek Tnaot"]))

pred_topcom2 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat1))

top5_newdat1$pred <- pred_topcom2

# plot
ggplot()+
  geom_line(data=tot_pop_newdat, aes(x=tot_pop, y=pred_tot_pop_av.m1), colour="red", size=1)+
  geom_line(data=top5_newdat, aes(tot_pop, y=pred), colour="blue", size=1)+
  geom_line(data=top5_newdat1, aes(tot_pop, y=pred), colour="green", size=1)+
  ylim(0,2300)+
  ylab("Forest pixels")+
  xlab("Total population (scaled)")


# next
top5_newdat2 <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                           pop_den = mean(dat1$pop_den),
                           prop_ind = mean(dat1$prop_ind),
                           year = mean(dat1$year),
                           Province = "Battambang",
                           Provcomm = "Battambang_Chhnal Moan",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Battambang_Chhnal Moan"]))

pred_topcom3 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat2))

top5_newdat2$pred <- pred_topcom3

# plot
ggplot()+
  geom_line(data=tot_pop_newdat, aes(x=tot_pop, y=pred_tot_pop_av.m1), colour="red", size=1)+
  geom_line(data=top5_newdat, aes(tot_pop, y=pred), colour="blue", size=1)+
  geom_line(data=top5_newdat1, aes(tot_pop, y=pred), colour="green", size=1)+
  geom_line(data=top5_newdat2, aes(tot_pop, y=pred), colour="orange", size=1)+
  ylim(0,3000)+
  ylab("Forest pixels")+
  xlab("Total population (scaled)")



## add lines for communes with largest effect of year (ie. steepest slope)

# order REs by year
commune_re <- commune_re[order(commune_re[,"year"], decreasing = TRUE),]
head(commune_re,5)

top5_newdat3 <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                           pop_den = mean(dat1$pop_den),
                           prop_ind = mean(dat1$prop_ind),
                           year = mean(dat1$year),
                           Province = "Siem Reap",
                           Provcomm = "Siem Reap_Kouk Chak",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Siem Reap_Kouk Chak"]))

pred_topcom4 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat3))

top5_newdat3$pred <- pred_topcom4

# plot
ggplot()+
  geom_line(data=tot_pop_newdat, aes(x=tot_pop, y=pred_tot_pop_av.m1), colour="red", size=1)+
  geom_line(data=top5_newdat, aes(tot_pop, y=pred), colour="blue", size=1)+
  geom_line(data=top5_newdat1, aes(tot_pop, y=pred), colour="green", size=1)+
  geom_line(data=top5_newdat2, aes(tot_pop, y=pred), colour="orange", size=1)+
  geom_line(data=top5_newdat3, aes(tot_pop, y=pred), colour="purple", size=1)+
  ylim(0,3000)+
  ylab("Forest pixels")+
  xlab("Total population (scaled)")


# order REs by year, but in reverse (largest negative slope is steeper than largest positive)
commune_re <- commune_re[order(commune_re[,"year"], decreasing = FALSE),]
head(commune_re,5)

top5_newdat4 <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                           pop_den = mean(dat1$pop_den),
                           prop_ind = mean(dat1$prop_ind),
                           year = mean(dat1$year),
                           Province = "Siem Reap",
                           Provcomm = "Siem Reap_Kampong Khleang",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Siem Reap_Kampong Khleang"]))

pred_topcom5 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat4))

top5_newdat4$pred <- pred_topcom5

# plot
ggplot()+
  geom_line(data=tot_pop_newdat, aes(x=tot_pop, y=pred_tot_pop_av.m1), colour="red", size=1)+
  geom_line(data=top5_newdat, aes(tot_pop, y=pred), colour="blue", size=1)+
  geom_line(data=top5_newdat1, aes(tot_pop, y=pred), colour="green", size=1)+
  geom_line(data=top5_newdat2, aes(tot_pop, y=pred), colour="orange", size=1)+
  geom_line(data=top5_newdat3, aes(tot_pop, y=pred), colour="purple", size=1)+
  geom_line(data=top5_newdat4, aes(tot_pop, y=pred), colour="yellow", size=1)+
  ylim(0,3000)+
  ylab("Forest pixels")+
  xlab("Total population (scaled)")


### Main take home point here I think is that total population does not predict forest cover very well!
### However, we can see that the effect of tot_pop is greater (steeper positive slope) in communes with more forest. This could be reflecting the fact that communes with more forest tend to be larger communes, which will tend to have larger absolute total populations. 


      # Predictions (newdata, pop_den varying) ####

# create new dataframe for predict (offset as term, so need to specify). Here we are predicting for an "average" commune and so the area should be set the mean
pop_den_newdat <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                             tot_pop = mean(dat1$tot_pop),
                             prop_ind = mean(dat1$prop_ind),
                             areaKM = mean(dat1$areaKM))



# predict for "average" commune
pred_pop_den_av.m1 <- as.vector(predict(popdem.m1, newdata = pop_den_newdat, type="response", re.form = NA))

pop_den_newdat$pred <- pred_pop_den_av.m1

# plot
p <- ggplot()+
     geom_line(data=pop_den_newdat, aes(x=pop_den, y=pred), colour="red", size=1)+
     ylim(0,3200)+
     ylab("Forest pixels")+
     xlab("Population density (scaled)")



## Let's add some of the communes with the highest intercepts 

commune_re <- ranef(popdem.m1)[[1]]
# Sort this from high to low intercept:
commune_re <- commune_re[order(commune_re[,"(Intercept)"], decreasing = TRUE),]
# So the top five are:
head(commune_re, 5)
# Save the names of these
top5 <- row.names(head(commune_re, 5))

top5_newdat <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                          tot_pop = mean(dat1$tot_pop),
                          prop_ind = mean(dat1$prop_ind),
                          year = mean(dat1$year),
                          Province = "Kampong Chhnang",
                          Provcomm = "Kampong Chhnang_Chieb",
                          areaKM = unique(dat1$areaKM[dat1$Provcomm=="Kampong Chhnang_Chieb"]))

pred_topcom1 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat))

top5_newdat$pred <- pred_topcom1

# plot
p <- p + geom_line(data=top5_newdat, aes(pop_den, y=pred), colour="blue", size=1)

# next
top5_newdat1 <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                           tot_pop = mean(dat1$tot_pop),
                           prop_ind = mean(dat1$prop_ind),
                           year = mean(dat1$year),
                           Province = "Kampot",
                           Provcomm = "Kampot_Preaek Tnaot",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Kampot_Preaek Tnaot"]))



pred_topcom2 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat1))

top5_newdat1$pred <- pred_topcom2

# plot
p <- p + geom_line(data=top5_newdat1, aes(pop_den, y=pred), colour="green", size=1)


# next
top5_newdat2 <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                           tot_pop = mean(dat1$tot_pop),
                           prop_ind = mean(dat1$prop_ind),
                           year = mean(dat1$year),
                           Province = "Battambang",
                           Provcomm = "Battambang_Chhnal Moan",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Battambang_Chhnal Moan"]))



pred_topcom3 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat2))

top5_newdat2$pred <- pred_topcom3

# plot
p <- p + geom_line(data=top5_newdat2, aes(pop_den, y=pred), colour="orange", size=1)



## add lines for communes with largest effect of year (ie. steepest slope)

# order REs by year
commune_re <- commune_re[order(commune_re[,"year"], decreasing = TRUE),]
head(commune_re,5)

top5_newdat3 <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                           tot_pop = mean(dat1$tot_pop),
                           prop_ind = mean(dat1$prop_ind),
                           year = mean(dat1$year),
                           Province = "Siem Reap",
                           Provcomm = "Siem Reap_Kouk Chak",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Siem Reap_Kouk Chak"]))



pred_topcom4 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat3))

top5_newdat3$pred <- pred_topcom4

# plot
p <- p + geom_line(data=top5_newdat3, aes(pop_den, y=pred), colour="purple", size=1)


# order REs by year, but in reverse (largest negative slope is steeper than largest positive)
commune_re <- commune_re[order(commune_re[,"year"], decreasing = FALSE),]
head(commune_re,5)

top5_newdat4 <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                           tot_pop = mean(dat1$tot_pop),
                           prop_ind = mean(dat1$prop_ind),
                           year = mean(dat1$year),
                           Province = "Siem Reap",
                           Provcomm = "Siem Reap_Kampong Khleang",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Siem Reap_Kampong Khleang"]))

pred_topcom5 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat4))

top5_newdat4$pred <- pred_topcom5

# plot
p <- p + geom_line(data=top5_newdat4, aes(pop_den, y=pred), colour="yellow", size=1)


### Main message here is that population density does appear to have some rpedictive power for forest cover.  Also, in communes with more forest, this effect is much larger.  I think this is potentially an important finding.


#
      # Predictions (newdata, prop_ind varying) ####

# create new dataframe for predict (offset as term so required here). Predicting for "average" commune and so mean area will be used
prop_ind_newdat <- data.frame(prop_ind = seq(from=min(dat1$prop_ind), to=max(dat1$prop_ind), length.out = 100),
                             tot_pop = mean(dat1$tot_pop),
                             pop_den = mean(dat1$prop_ind),
                             areaKM = mean(dat1$areaKM))



# predict for "average" commune
pred_prop_ind_av.m1 <- as.vector(predict(popdem.m1, newdata = prop_ind_newdat, type="response", re.form = NA))

prop_ind_newdat$pred <- pred_prop_ind_av.m1

# plot. I am not adding the original points because the model is predicting forest pixels per unit area (KM)
p <- ggplot()+
  geom_line(data=prop_ind_newdat, aes(x=prop_ind, y=pred), colour="red", size=1)+
  ylim(0,3000)+
  ylab("Forest pixes per KM2")+
  xlab("Proportion indigenous (scaled)")



## Let's add some of the communes with the highest intercepts 

commune_re <- ranef(popdem.m1)[[1]]
# Sort this from high to low intercept:
commune_re <- commune_re[order(commune_re[,"(Intercept)"], decreasing = TRUE),]
# So the top five are:
head(commune_re, 5)
# Save the names of these
top5 <- row.names(head(commune_re, 5))

top5_newdat <- data.frame(prop_ind = seq(from=min(dat1$prop_ind), to=max(dat1$prop_ind), length.out = 100),
                          tot_pop = mean(dat1$tot_pop),
                          pop_den = mean(dat1$pop_den),
                          year = mean(dat1$year),
                          Province = "Kampong Chhnang",
                          Provcomm = "Kampong Chhnang_Chieb",
                          areaKM = unique(dat1$areaKM[dat1$Provcomm=="Kampong Chhnang_Chieb"]))

pred_topcom1 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat))

top5_newdat$pred <- pred_topcom1

# plot
p <- p + geom_line(data=top5_newdat, aes(prop_ind, y=pred), colour="blue", size=1)

# next
top5_newdat1 <- data.frame(prop_ind = seq(from=min(dat1$prop_ind), to=max(dat1$prop_ind), length.out = 100),
                           tot_pop = mean(dat1$tot_pop),
                           pop_den = mean(dat1$pop_den),
                           year = mean(dat1$year),
                           Province = "Kampot",
                           Provcomm = "Kampot_Preaek Tnaot",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Kampot_Preaek Tnaot"]))



pred_topcom2 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat1))

top5_newdat1$pred <- pred_topcom2

# plot
p <- p + geom_line(data=top5_newdat1, aes(prop_ind, y=pred), colour="green", size=1)


# next
top5_newdat2 <- data.frame(prop_ind = seq(from=min(dat1$prop_ind), to=max(dat1$prop_ind), length.out = 100),
                           tot_pop = mean(dat1$tot_pop),
                           pop_den = mean(dat1$pop_den),
                           year = mean(dat1$year),
                           Province = "Battambang",
                           Provcomm = "Battambang_Chhnal Moan",
                           areaKM = unique(dat1$areaKM[dat1$Provcomm=="Battambang_Chhnal Moan"]))



pred_topcom3 <- as.vector(predict(popdem.m1, type="response", re.form = ~(year|Province/Provcomm), 
                                  newdata = top5_newdat2))

top5_newdat2$pred <- pred_topcom3

# plot
p <- p + geom_line(data=top5_newdat2, aes(prop_ind, y=pred), colour="orange", size=1)


# I'm not bothering with plotting the others as I have above for the other variables. There is clearly sweet fuck all going on here. Those slopes couln't be flatter :(



    # popdem.m2 (tot_pop, pop_den, prop_ind - interactions) ####

# model with all pop dem vars as fixed effects. Offset (areaKM) as term to account for differences in size of communes. All interactions tested
popdem.m2 <- glmer(ForPix ~ tot_pop * pop_den * prop_ind + offset(log(areaKM)) + (year|Province/Provcomm),
                   data=dat1, family="poisson")


summary(popdem.m2)
# ran with convergence and eigenvector warnings.  Variances for province and commune are similar.  Year variance is very small, but order of mag smaller for the province level. tot_pop is positive, pop_den and prop_ind are negative.  As pop_den increases, the effect size of tot_pop increases (gets more positive). As prop_ind increases, effect of tot_pop increases (but by a tiny amount). As prop_ind increases the effect size of pop_den increases (get's more negative), but not by a lot. Approximate sig suggests tot_pop, pop_den, and tot_pop:pop_den are sig. 

fixef(popdem.m2)
fixef(popdem.m1)
# this model has larger effect sizes for all terms than m1

ranef(popdem.m2)

# so for this model I have a global intercept, and global slopes for tot_pop, pop_den, and prop_ind, and their interactions.  I also have individual intercepts for each province, and for each commune/province combination, and a random slope for year for each province and each commune.  

# plot the random effects
plot_model(popdem.m2, type="re")





      # model comparison - popdem.m4 selected for further investigations ####

# when using pbkrtest package, always ensure the simpler model is listed second, or else the function will fail

# Likelihood ratio test

# test just the terms in popdem.m2
drop1(popdem.m2, test="Chisq")
# the tot_pop:pop_den:prop_ind interaction is not sig

# anova
anova(popdem.m2, popdem.m1, test="Chisq")
# simpler model is worse

# remove interaction with prop_ind
popdem.m3 <- glmer(ForPix ~ tot_pop * pop_den + prop_ind + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")


# anova
anova(popdem.m2, popdem.m3, test="Chisq")
# simpler model is better. The interaction with prop_ind is not doing anything - this is supported by the plots from the prop_ind section above (no effects at all)

# remove prop_ind altogether
popdem.m4 <- glmer(ForPix ~ tot_pop * pop_den + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")

# anova
anova(popdem.m3, popdem.m4, test="Chisq")
# model with no prop_ind is better - this was expected and is supported by the plots etc.

# remove interaction between tot_pop and pop_den
popdem.m5 <- glmer(ForPix ~ tot_pop + pop_den + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")

# anova
anova(popdem.m4, popdem.m5, test="Chisq")
# The simpler model with no interaction is signficantly worse 


# remove tot_pop altogether
popdem.m6 <- glmer(ForPix ~ pop_den + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")

plot_model(popdem.m6, type = "pred")

# anova
anova(popdem.m4, popdem.m6, test="Chisq")
# the model with tot_pop is better


## None of the p values have been questionable (i.e. close to or around 0.05). I would use another test for prop_ind if I had any doubts but I don't, thanks to the plotting of the effects in the above section.  If I need to get final p-values for reporting, I can use parametric bootstrapping, but I won't bother just now as it takes ages.




#
      # model diagnostics popdem.m4 ####
        # DHARMa ####

# USing DHARMa package for diagnostics - first calculate residuals using all RE levels
simulationOutput <- simulateResiduals(fittedModel = popdem.m4, plot = T, 
                                      re.form = ~(year|Province/Provcomm))

# QQ plot
plotQQunif(simulationOutput)
# According to the DHARMa vignette, this QQ plot shows underdispersion - i.e. too many value around 0.5 and not enough residuals at the tail ends of the distribution. Although on the plot the dispersion test is not sig, but the deviation from the distribution is

# Another test for dispersion
testDispersion(simulationOutput)
# this looks pretty good to me. Perhaps slightly too many in the middle and not enough at the tails

# residual vs predcted plot
plotResiduals(simulationOutput)
# this to me shows fairly major heteroskedasicity - much more residual variance for small values of predicted ForPix.  I was hoping that the removal of prop_ind was going to make this look better!
hist(simulationOutput)

# plot residuals from individual predictors
par(mfrow=c(1,2))
plotResiduals(simulationOutput, dat1$tot_pop)
# tot_pop residuals look ok
plotResiduals(simulationOutput, dat1$pop_den)
# pop_den residuals show high variation close to zero


par(mfrow=c(1,1))
# test temporal and spatial autocorrelation
testTemporalAutocorrelation(simulationOutput)
# no major problem with temporal autocorr. NOthing significant, and no clear pattern in the lags with higher values
testSpatialAutocorrelation(simulationOutput)
# I haven't provided any x values. It says random values assigned. If these values were assigned in order, then this should partly work because the communes are listed by province and so communes in the same province should have similar "random" values and so should act as a proxy for x values. 

# test for zero inflation
par(mfrow=c(1,1))
testZeroInflation(simulationOutput)

# testing residuals per commune
simulationOutput.com = recalculateResiduals(simulationOutput, dat1$Provcomm)
# QQ plot
plotQQunif(simulationOutput.com)
# still shows underdispersion. Outlier test and deviation test are both significant.

# Another test for dispersion
testDispersion(simulationOutput.com)
# not as well spread as the one above

# residual vs predcted plot
plotResiduals(simulationOutput.com)
# looks like too many small residuals at lower model predictions. This suggests that the model is better fitted at smaller ForPix predictions. There are a lot more smaller communes which will be definition have less forest


# testing residuals per province
simulationOutput.prov = recalculateResiduals(simulationOutput, dat1$Province)
# QQ plot
plotQQunif(simulationOutput.prov)
# Not as bad as communes

# Another test for dispersion
testDispersion(simulationOutput.prov)
# looks pretty good to me. This suggests that the issues with residuals are coming from the communes, not the provinces

# residual vs predicted plot
plotResiduals(simulationOutput.prov)
# Not really enough points but I think that looks ok!


# get outliers
outliers(simulationOutput)
str(simulationOutput)


        # Manual diagnostics ####

# copy data
m4.diag.dat <- dat1
m4.diag.dat$Provcomm <- as.factor(m4.diag.dat$Provcomm)

### Make "fitted" predictions, i.e. fully conditional:
m4.diag.dat$m4pred <- predict(popdem.m4, type = "response")

### Plot predicted against observed:
plot(m4.diag.dat$ForPix, m4.diag.dat$m4pred, ylab = "Predicted ForPix", xlab = "Observed ForPix")
### Nice!

### Extract model residuals:
m4.diag.dat$m4res <- resid(popdem.m4)

### Plot residuals against fitted values:
plot(m4.diag.dat$m4pred, m4.diag.dat$m4res)
### Less nice - quite a bit of heterogeneity here - especially at low predicted values.
### So al low predicted values, "error" is greater.
### Given the structure in the data, this is almost inevitable given the extent of variation across communes.


### This is an attempt at repeating the plot but colouring by commune:
colfunc = colorRampPalette(c("red","royalblue"))
m4.diag.dat$Provcomm_colours = m4.diag.dat$Provcomm
levels(m4.diag.dat$Provcomm_colours) = heat.colors((nlevels(m4.diag.dat$Provcomm)))

# predicted values vs residuals, coloured by commune
plot(m4.diag.dat$m4pred, m4.diag.dat$m4res, col = m4.diag.dat$Provcomm_colours)

### Not quite as highly correlated with commune at the lower end of the range:
plot(m4.diag.dat$m4pred, m4.diag.dat$m4res, col = m4.diag.dat$Provcomm_colours, xlim = c(0,2000))

### Essentially what this suggests, is that the current model seems to be less good at accurately predicting patterns of forest cover in the lower range... perhaps this is due to missing predictors?
plot(m4.diag.dat$ForPix, m4.diag.dat$m4res, col = m4.diag.dat$Provcomm_colours)

### Bit more exploration of residuals, but now over each explanatory variable:
par(mfrow=c(2,2))
plot(m4.diag.dat$tot_pop, m4.diag.dat$m4res, ylab = "residuals", xlab = "total pop size")
plot(m4.diag.dat$pop_den, m4.diag.dat$m4res, ylab = "residuals", xlab = "pop density")
boxplot(m4res ~ factor(Province), data = m4.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Province")
boxplot(m4res ~ factor(Provcomm), data = m4.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Commune")
### Again, a lot of this heteregeneity looks like its driven by a relatively small number of Provinces/Communes. It may
### be worthwhile to do some more data exploration - find which ones there are and see if I can see any obvious
### commonalities for those.

# extract the provinces and communes with extreme residuals
high.res <- m4.diag.dat %>% filter(m4res > 2 | m4res < -2) %>% 
            select(Province, Commune, Provcomm, ForPix, year.orig, pop_den, tot_pop, m4res) %>% 
            arrange(Provcomm)

# extract the communes with residuals closest to 0 (for comparison)
low.res <- m4.diag.dat %>% filter(m4res > -0.001 & m4res < 0.001) %>% 
           select(Province, Commune, Provcomm, ForPix, year.orig, pop_den, tot_pop, m4res) %>% 
           arrange(Provcomm)

# only a few communes that have very extreme residual values. take a closer look
dat %>% filter(Province=="Siem Reap" & Commune=="Chi Kraeng") %>% select(year,ForPix,pop_den,tot_pop)
dat %>% filter(Province=="Siem Reap" & Commune=="Kampong Khleang") %>% select(year,ForPix,pop_den,tot_pop)
dat %>% filter(Province=="Kampong Thom" & Commune=="Popok") %>% select(year,ForPix,pop_den,tot_pop)
dat %>% filter(Province=="Kracheh" & Commune=="Preaek Prasab") %>% select(year,ForPix,pop_den,tot_pop)

# closer look at communes with low residuals
dat %>% filter(Province=="Kampong Speu" & Commune=="Ta Sal") %>% select(year,ForPix,pop_den,tot_pop)
dat %>% filter(Province=="Koh Kong" & Commune=="Phnhi Meas") %>% select(year,ForPix,pop_den,tot_pop)
dat %>% filter(Province=="Koh Kong" & Commune=="Ta Tey Leu") %>% select(year,ForPix,pop_den,tot_pop)
dat %>% filter(Province=="Mondul Kiri" & Commune=="Srae Sangkom") %>% select(year,ForPix,pop_den,tot_pop)

# extract original data from dat1 for the communes with extreme/low residuals (so I get ALL years from each commune)
high.res.orig <- dat1[dat1$Provcomm %in% high.res$Provcomm, c("year.orig", "ForPix", "Province", "Commune","pop_den")]
high.res.orig$residuals <- "high"
low.res.orig <- dat1[dat1$Provcomm %in% low.res$Provcomm, c("year.orig", "ForPix", "Province", "Commune","pop_den")]
low.res.orig$residuals <- "low"

res.comp <- rbind(high.res.orig,low.res.orig)

### plots of changes in ForPix

# plot high residual communes
ggplot(high.res.orig, aes(x=year.orig, y=ForPix, group=Commune, colour=Commune))+
  geom_line()+
  facet_wrap(high.res.orig$Commune, scales = "free")

# plot low residual communes
ggplot(low.res.orig, aes(x=year.orig, y=ForPix, group=Commune, colour=Commune))+
  geom_line()+
  facet_wrap(low.res.orig$Commune, scales = "free")

# plot together
ggplot(res.comp, aes(x=year.orig, y=ForPix, group=Commune, colour=residuals))+
  geom_line(size=0.7)+
  theme(element_blank())+
  ylim(0,1000)

### So it looks like the error is potentially coming from two sources: 1) the model is not predicting well for communes with generally low forest cover (fewer pixels), but 2) also the communes that are not predicted well tend to be communes that lose a decent % of their forest cover, often in one time step.  

### Plots of changes in pop_den

# plot high residual communes
ggplot(high.res.orig, aes(x=year.orig, y=pop_den, group=Commune, colour=Commune))+
  geom_line()+
  facet_wrap(high.res.orig$Commune, scales = "free")

# plot low residual communes
ggplot(low.res.orig, aes(x=year.orig, y=pop_den, group=Commune, colour=Commune))+
  geom_line()+
  facet_wrap(low.res.orig$Commune, scales = "free")

# plot together
ggplot(res.comp, aes(x=year.orig, y=pop_den, group=Commune, colour=residuals))+
  geom_line(size=0.7)+
  theme(element_blank())+
  ylim(-1,0)

### less of an obvious pattern here - there is similar variation in pop_den between years in the communes with high and low residuals.  One potential pattern is that the communes with low residuals tend to either have very high or very low values for pop_den, whereas the communes with high residuals sit in the middle.  Not really sure that means much though


### I forgot to look at 'year':
plot(m4.diag.dat$year, m4.diag.dat$m4res, ylab = "residuals", xlab = "year")
### This looks a lot better (at least heterogeneity wise) than the others.




### This looks at a bunch of other potential models with a few extra fixed effects:
par(mfrow=c(3,2))
popdem.m5 <- glmer(ForPix ~ tot_pop * pop_den + dist_border + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")
m4.diag.dat$m5pred = predict(popdem.m5, type = "response")
plot(m4.diag.dat$ForPix, m4.diag.dat$m5pred)
m4.diag.dat$m5res = resid(popdem.m5)
plot(m4.diag.dat$m5pred , m4.diag.dat$m5res, col = m4.diag.dat$Provcomm_colours)

popdem.m6 <- glmer(ForPix ~ tot_pop * pop_den + dist_provCap + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")
m4.diag.dat$m6pred = predict(popdem.m6, type = "response")
plot(m4.diag.dat$ForPix, m4.diag.dat$m6pred)
m4.diag.dat$m6res = resid(popdem.m6)
plot(m4.diag.dat$m6pred, m4.diag.dat$m6res, col = m4.diag.dat$Provcomm_colours)

popdem.m7 <- glmer(ForPix ~ tot_pop * pop_den + propPrimSec + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")
m4.diag.dat$m7pred = predict(popdem.m7, type = "response")
plot(m4.diag.dat$ForPix, m4.diag.dat$m7pred)
m4.diag.dat$m7res = resid(popdem.m7)
plot(m4.diag.dat$m7pred, m4.diag.dat$m7res, col = m4.diag.dat$Provcomm_colours)


### What happens if we take out all fixed effects and just rely on the RE structure for overall fit? We can use this as
### "null" model - if the "null" is "best" it means you basically have nil variance left for any fixed effects, after
### fitting RE's.
popdem.m0 <- glmer(ForPix ~ 1 + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")

### Compare the above models:
### I'm just using MuMin here for convenience, because it has an AICc function:
library(MuMIn)
aicc = AICc(popdem.m0, popdem.m4,popdem.m5,popdem.m6,popdem.m7)
bic =  BIC(popdem.m0, popdem.m4,popdem.m5,popdem.m6,popdem.m7)

aicc = aicc[order(aicc[,"AICc"]),]
aicc$diff = aicc[,"AICc"] - aicc[1,"AICc"]
aicc

bic = bic[order(bic[,"BIC"]),]
bic$diff = bic[,"BIC"] - bic[1,"BIC"]
bic

### So from the above it looks like that whatever the comparison metric, the models WITH fixed effects give much better
### fits compared to the intercept only - which is good!

### This also suggests that the apparent issue with heterogeneity of variance as referred to above (larger residuals at
### lower forest cover values) isn't solely a function of lack of variance following fitting of RE's.



#
    # predictions - popdem.m4 ####
      # manual predictions from observed data ####

summary(popdem.m4)
# RE variances for province and commune, and for year at each level are similar to m1 and m2.  Approximate p values suggest sig effects for tot_pop, pop_den and tot_pop:pop_den.  tot_pop has a positive effect, and pop_den has a negative effect. The interaction suggests that as pop_den increases, the effect of tot_pop gets larger.  

fixef(popdem.m4)

ranef(popdem.m4)

#  quick plots of fixed effects
plot_model(popdem.m4, type="pred", terms=c("pop_den","tot_pop"))
# this plot suggests that increasing population density reduces predicted forest cover. When a commune has low total population, forest cover decreases more quickly. As tot_pop increases, the slope for pop_den gets flatter.
plot_model(popdem.m4, type="pred", terms=c("tot_pop","pop_den"))
# This plot suggests that at low population densities, tot_pop has no real effect on forest cover.  But as population density increases, tot_pop then has a positive effect on predicted forest cover. I can't think of a reasonable explanation for this at the moment


## manually calculate predictions

# first create subset data
popdem.m4.dat <- select(dat1, ForPix, tot_pop, pop_den, areaKM, year, Province, Provcomm)

# add global intercept
popdem.m4.dat$Iglobal <- fixef(popdem.m4)[["(Intercept)"]]

# add RE intercepts for each Province 
popdem.m4.dat$Iprovince <- ranef(popdem.m4)$Province[,"(Intercept)"][
  match(popdem.m4.dat$Province, row.names(ranef(popdem.m4)$Province))]

# add Provcomm:Province name column to match the RE output
popdem.m4.dat$Provcomm_P = paste(popdem.m4.dat$Provcomm,popdem.m4.dat$Province,sep=":")

# add RE intercepts for each commune
popdem.m4.dat$Icommune <- ranef(popdem.m4)[[1]][, "(Intercept)"][
  match(popdem.m4.dat$Provcomm_P, row.names(ranef(popdem.m4)[[1]]))]


# add RE slope for year for province
popdem.m4.dat$b_year_prov <- ranef(popdem.m4)$Province[,"year"][
  match(popdem.m4.dat$Province, row.names(ranef(popdem.m4)$Province))]

# add random slope for year for commune
popdem.m4.dat$b_year_com <- ranef(popdem.m4)$'Provcomm:Province'[,"year"][
  match(popdem.m4.dat$Provcomm_P, row.names(ranef(popdem.m4)$'Provcomm:Province'))]

# now add the fixed effects of tot_pop & pop_den
popdem.m4.dat$b_tot_pop <- fixef(popdem.m4)['tot_pop']
popdem.m4.dat$b_pop_den <- fixef(popdem.m4)['pop_den']

# add offset
popdem.m4.dat$offset <- log(dat1$areaKM)[match(popdem.m4.dat$Provcomm, dat1$Provcomm)]

head(popdem.m4.dat)

# y ~ I + (Ip + Ic)|Prov/Comm + tot_pop*b1 * pop_den*b2 + (year*(cP + Cc)|Prov/Comm)

# (global_intercept+(IProvince+Icommune)_given province/commune) + tot_pop*b_totpops * pop_den*b_pop_den + 
#  year.s*(b_year_province+b_year_commune)_given province_commune).

# predict manually
m_popdem.m4_pred <- with(popdem.m4.dat, {
  Iglobal +
    tot_pop*b_tot_pop *
    pop_den*b_pop_den +
    Iprovince +
    Icommune +
    year*(b_year_prov+b_year_com)+
    offset
})

# compare with predict()
pred_popdem.m4 <- predict(popdem.m4, type = "response")
plot(exp(m_popdem.m4_pred),pred_popdem.m4)
# they match pretty well



#
      # predict main effects ####

## for average commune. In this case, the offset needs to be set to the mean i.e. an "average sized" commune

# tot_pop with pop_den held at mean
tot_pop_newdat_noint <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), length.out = 100),
                                   pop_den = mean(dat1$pop_den),
                                   areaKM = mean(dat1$areaKM))

# tot_pop with pop_den at min
tot_pop_newdat_int_min <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), 
                                                   length.out = 100),
                                     pop_den = min(dat1$pop_den),
                                     areaKM = mean(dat1$areaKM))

# tot_pop with pop_den at max
tot_pop_newdat_int_max <- data.frame(tot_pop = seq(from=min(dat1$tot_pop), to=max(dat1$tot_pop), 
                                                   length.out = 100),
                                     pop_den = max(dat1$pop_den),
                                     areaKM = mean(dat1$areaKM))

                                                 
# pop_den with tot_pop held at mean
pop_den_newdat_noint <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                                   tot_pop = mean(dat1$tot_pop),
                                   areaKM = mean(dat1$areaKM))

# pop_den with tot_pop at min
pop_den_newdat_int_min <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), 
                                                   length.out = 100),
                                     tot_pop = min(dat1$tot_pop),
                                     areaKM = mean(dat1$areaKM))

# pop_den with tot_pop at max
pop_den_newdat_int_max <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), 
                                                   length.out = 100),
                                     tot_pop = max(dat1$tot_pop),
                                     areaKM = mean(dat1$areaKM))


# predict
tot_pop_pred_noint     <- predict(popdem.m4, newdata=tot_pop_newdat_noint, type="response", re.form=NA)
tot_pop_pred_int_min   <- predict(popdem.m4, newdata=tot_pop_newdat_int_min, type="response", re.form=NA)
tot_pop_pred_int_max   <- predict(popdem.m4, newdata=tot_pop_newdat_int_max, type="response", re.form=NA)

pop_den_pred_noint     <- predict(popdem.m4, newdata=pop_den_newdat_noint, type="response", re.form=NA)
pop_den_pred_int_min   <- predict(popdem.m4, newdata=pop_den_newdat_int_min, type="response", re.form=NA)
pop_den_pred_int_max   <- predict(popdem.m4, newdata=pop_den_newdat_int_max, type="response", re.form=NA)

# merge
tot_pop_newdat_noint$pred    <- tot_pop_pred_noint
tot_pop_newdat_int_min$pred  <- tot_pop_pred_int_min
tot_pop_newdat_int_max$pred  <- tot_pop_pred_int_max
tot_pop_newdat_all <- rbind(tot_pop_newdat_noint,tot_pop_newdat_int_min,tot_pop_newdat_int_max)
tot_pop_newdat_all$pop_den_value <- rep(c("mean", "min", "max"), each=100)

pop_den_newdat_noint$pred    <- pop_den_pred_noint
pop_den_newdat_int_min$pred  <- pop_den_pred_int_min
pop_den_newdat_int_max$pred  <- pop_den_pred_int_max
pop_den_newdat_all <- rbind(pop_den_newdat_noint,pop_den_newdat_int_min,pop_den_newdat_int_max)
pop_den_newdat_all$tot_pop_value  <- rep(c("mean", "min", "max"), each=100)

# plot
p.tot_pop <- ggplot(tot_pop_newdat_all, aes(x=tot_pop, y=pred, group=pop_den_value, colour=pop_den_value))+
  geom_line(size=1)+
  theme(element_blank())
 
  
p.pop_den <- ggplot(pop_den_newdat_all, aes(x=pop_den, y=pred, group=tot_pop_value, colour=tot_pop_value))+
  geom_line(size=1)+
  theme(element_blank())
  
p.tot_pop + p.pop_den


### total population still has a postive effect on forest cover. One explanation for this is that the communes withe more forest tend to be the more remote commmunes in the large, rural provinces.  So although population density may be low, absolute total population is still large (i.e. they have more villages).  Communes with high pop_den values tend to have less forest to start with, but as population increases, so does predicted forest cover. Commmunes with the lowest population density have the most forest (this makes sense), and the smallest effect (flattest slope) as population increases.
### population density has a negative effect on forest cover.  Communes with the lowest population density have the most forest, which makes sense. Communes with lower total populations are affected more by increasing population density (i.e. steeper slopes). This could reflect the more remote communes (which have more forest), being more impacted by increases in population density.  Communes with a higher absolute population, are affected less by increasing population density.
### I am still not sure whether tot_pop is a sensible variable to have in, and whether it is actually telling me anything. Jeroen is concerned about collinearity between the two, which I understand, but I don't think they are always directly linked. Population density can change without the total population changing, depending on the size of the commune. But Kez pointed out that tot_pop is not that meaningful because communes are "arbitrary" delineations, and tot_pop reflects those arbitrary deliniations. And so surely pop_den is the more relevant measure of the impact of population?  
#
      # predict for sets of communes ####

# here I want to plot grids of different communes with the overall predicted effect, plus the commune-specific effect. I want to do this for communes with commune-level intercepts close to the mean, and communes with commune-level intercpets at the extremes

# save the popdem.m4 commune-level random effects
m4.re.com <- ranef(popdem.m4)[[1]]
plot_model(popdem.m4, type="re")

# re-order
m4.re.com <- m4.re.com[order(m4.re.com[ ,"(Intercept)"], decreasing = TRUE),]

# the 4 communes closest to 0 are:
# Kampot_Chres:Kampot, 
# Koh Kong_Tuol Kokir Leu:Koh Kong, 
# Kampong Cham_Cheyyou:Kampong Cham, 
# Kracheh_Ta Mau:Kracheh

# the 4 communes furthest above 0 are:
# Phnom Penh_Chak Angrae Kraom:Phnom Penh
# Kampong Cham_Pongro:Kampong Cham
# Kampong Chhnang_Chieb:Kampong Chhnang
# Kampot_Preaek Tnaot:Kampot

# the 4 communes furthest below 0 are:
# Kampong Thom_Chaeung Daeung:Kampong Thom
# Kracheh_Han Chey:Kracheh
# Siem Reap_Nokor Pheas:Siem Reap
# Pursat_Boeng Bat Kandaol:Pursat

# which communes
coms <- c("Kampot_Chres","Koh Kong_Tuol Kokir Leu","Kampong Cham_Cheyyou","Kracheh_Ta Mau",
          "Phnom Penh_Chak Angrae Kraom","Kampong Cham_Pongro","Kampong Chhnang_Chieb","Kampot_Preaek Tnaot",
          "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Siem Reap_Nokor Pheas","Pursat_Boeng Bat Kandaol")

# which provinces
provs <- c("Kampot","Koh Kong", "Kampong Cham","Kracheh","Phnom Penh","Kampong Cham","Kampong Chhnang","Kampot",
           "Kampong Thom","Kracheh","Siem Reap","Pursat")


### I am making commune-specific predictions, so probably should customise both the mean total population size,
### as well as the range of population densities you are predicting for, on the basis of each commune.

### Easiest to define the range of pop_den to predict for, first. Min/max per commune:
pop_den_min <- tapply(dat1$pop_den, dat1$Provcomm, min)
pop_den_max <- tapply(dat1$pop_den, dat1$Provcomm, max)
### Min/max within your selection of communes:
pop_den_min <- min(pop_den_min[names(pop_den_min) %in% coms])
pop_den_max <- max(pop_den_max[names(pop_den_max) %in% coms])
### Note these are quite different to the overall min/max:
min(dat1$pop_den)
max(dat1$pop_den)

# create new prediction grid for specific communes with varying pop_den
m4_newdat_com <- data.frame(Provcomm = rep(coms, each=100),
                            Province = rep(provs, each=100),
                            pop_den = seq(from=pop_den_min, to=pop_den_max, length.out = 100),
                            year = mean(dat1$year))

### Add commune-specific mean pop size:
provcomm_mean_totpop <- as.data.frame(tapply(dat1$tot_pop, dat1$Provcomm, mean))
m4_newdat_com$tot_pop <- provcomm_mean_totpop[,1][match(m4_newdat_com$Provcomm, row.names(provcomm_mean_totpop))]

# add commune-specific areaKM offset                         
m4_newdat_com$areaKM <-  dat1$areaKM[match(m4_newdat_com$Provcomm, dat1$Provcomm)]

# re-order levels so they plot in the correct sets
m4_newdat_com$Provcomm <- factor(m4_newdat_com$Provcomm, 
                                 levels = c("Kampot_Chres","Koh Kong_Tuol Kokir Leu","Kampong Cham_Cheyyou",
                                            "Kracheh_Ta Mau","Phnom Penh_Chak Angrae Kraom",
                                            "Kampong Cham_Pongro","Kampong Chhnang_Chieb","Kampot_Preaek Tnaot",
                                            "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Siem Reap_Nokor Pheas",
                                            "Pursat_Boeng Bat Kandaol"))


# attach commune-specific predictions
m4_newdat_com$pred.com <- as.vector(predict(popdem.m4, type="response", newdata=m4_newdat_com, 
                                        re.form=~(year|Province/Provcomm)))


# create new prediction grid for global effects with varying pop_den
#m4_newdat_glo <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                            #tot_pop = mean(dat1$tot_pop),
                            #areaKM = mean(dat1$areaKM))


# global predictions (i.e. ignoring RE's)
#pred.glo <- as.vector(predict(popdem.m4, type="response", newdata=m4_newdat_glo, re.form=NA))

# attach global predictions
#m4_newdat_com$pred.glo <- rep(pred.glo, times=12)

# attach real data points
#m4_newdat_com$pop_den_real <- dat1$pop_den[match(m4_newdat_com$Provcomm, dat1$Provcomm)]
#m4_newdat_com$ForPix <- dat1$ForPix[match(m4_newdat_com$Provcomm, dat1$Provcomm)]

# pull out real data points for the above communes
#pop_den_dat <- data.frame(Provcomm = as.factor(dat1$Provcomm[dat1$Provcomm %in% coms]) ,
                         #pop_den = dat1$pop_den[dat1$Provcomm %in% coms],
                         #ForPix = dat1$ForPix[dat1$Provcomm %in% coms])

# plot
#ggplot()+
 # geom_line(data=m4_newdat_com, aes(x=pop_den, y=pred.glo))+
  #geom_line(data=m4_newdat_com, aes(x=pop_den, y=pred.com), linetype="dashed")+
  #facet_wrap(m4_newdat_com$Provcomm, nrow=3)+
  #ylim(0,1000)+
  #ylab("Predicted forest pixels")+
  #xlab("Population density (scaled)")+
  #theme(element_blank())

# plot
#ggplot(m4_newdat_com, aes(x=pop_den))+
  #geom_line(aes(y=pred.glo))+
  #geom_line(aes(y=pred.com), linetype="dashed")+
  #facet_wrap(m4_newdat_com$Provcomm, nrow=3)+
  #ylim(0,1000)+
  #ylab("Predicted forest pixels")+
  #xlab("Population density (scaled)")+
  #theme(element_blank())


### The following plot "overlays" the observed ForPix count against observed population densities for the communes.

### Pick some colours using RColorBrewer using a completely overelaborate piece of crap code... Anyway this is just to
#try to help see the differences between communes more clearly in the observed points in particular (you can comment the
#following lines out if you want)
require(RColorBrewer)
com_colours <- brewer.pal(11, "RdYlBu")
com_colours <- c(head(com_colours,4),tail(com_colours,4))
com_colours_greys <- tail(brewer.pal(9, "Greys"),4)
com_colours <- c(com_colours, com_colours_greys)
com_colours <- com_colours[sample(1:length(com_colours),12,replace=F)]

### This is just to check if you have commented tbe above out: :)
if(!exists("com_colours")) {
  com_colours <- rep("black", 12)
}
provcomm_lvls <- levels(m4_newdat_com$Provcomm) 
par(mfrow = c(3,4))
### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### population density range across all communes, so we need to do this overall:
ylo <- min(m4_newdat_com$pred.com)*0.9
yhi <- max(m4_newdat_com$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(m4_newdat_com$Provcomm),"pop_den"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(m4_newdat_com$Provcomm),"pop_den"])
### Iterate through the communes (levels in m4_newdat_com$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- m4_newdat_com[m4_newdat_com$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
 # if(i == 1) {
    # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
    plot(preddat_i$pop_den,preddat_i$pred.com, 
         type = "l", 
         col = com_colours[i], 
         ylim = c(ylo,yhi), xlim = c(xlo,xhi),
         xlab = "Population density (scaled & standardised",
         ylab = "Predicted forest cover (forest pixel count)")
 # } else {
    lines(preddat_i$pop_den,preddat_i$pred.com, col = com_colours[i])
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$pop_den, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

#
    # popdem.m6 (pop_den only) ####
      # manual diagnostics ####

# copy data
m6.diag.dat <- dat1
m6.diag.dat$Provcomm <- as.factor(m6.diag.dat$Provcomm)

### Make "fitted" predictions, i.e. fully conditional:
m6.diag.dat$m6pred <- predict(popdem.m6, type = "response")

### Plot predicted against observed:
plot(m6.diag.dat$ForPix, m6.diag.dat$m6pred, ylab = "Predicted ForPix", xlab = "Observed ForPix")
### good

### Extract model residuals:
m6.diag.dat$m6res <- resid(popdem.m6)

### Plot residuals against fitted values:
plot(m6.diag.dat$m6pred, m6.diag.dat$m6res)
### still quite a bit of heterogeneity here - especially at low predicted values.
### So al low predicted values, "error" is greater.
### Given the structure in the data, this is almost inevitable given the extent of variation across communes.

# plot predictions against residuals for m4 vs m6
par(mfrow=c(2,1))
plot(m6.diag.dat$m6pred, m6.diag.dat$m6res)
plot(m4.diag.dat$m4pred, m4.diag.dat$m4res)
# not much difference at all


### This is an attempt at repeating the plot but colouring by commune:
colfunc = colorRampPalette(c("red","royalblue"))
m6.diag.dat$Provcomm_colours = m6.diag.dat$Provcomm
levels(m6.diag.dat$Provcomm_colours) = heat.colors((nlevels(m6.diag.dat$Provcomm)))

# predicted values vs residuals, coloured by commune
plot(m6.diag.dat$m6pred, m6.diag.dat$m6res, col = m6.diag.dat$Provcomm_colours)

### Not quite as highly correlated with commune at the lower end of the range:
plot(m6.diag.dat$m6pred, m6.diag.dat$m6res, col = m6.diag.dat$Provcomm_colours, xlim = c(0,2000))

# above zoomed in plot of m6 and m4
plot(m6.diag.dat$m6pred, m6.diag.dat$m6res, col = m6.diag.dat$Provcomm_colours, xlim = c(0,2000))
plot(m4.diag.dat$m4pred, m4.diag.dat$m4res, col = m4.diag.dat$Provcomm_colours, xlim = c(0,2000))

### Essentially what this suggests, is that the current model seems to be less good at accurately predicting patterns of forest cover in the lower range... perhaps this is due to missing predictors?
plot(m6.diag.dat$ForPix, m6.diag.dat$m6res, col = m6.diag.dat$Provcomm_colours)

### Bit more exploration of residuals, but now over the explanatory variable:
par(mfrow=c(2,2))
plot(m6.diag.dat$pop_den, m6.diag.dat$m6res, ylab = "residuals", xlab = "pop density")
boxplot(m6res ~ factor(Province), data = m6.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Province")
boxplot(m6res ~ factor(Provcomm), data = m6.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Commune")
### similar to m4

# check m4 vs m6 for the provinces causing issues
par(mfrow=c(2,1))
boxplot(m6res ~ factor(Province), data = m6.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Province")
boxplot(m4res ~ factor(Province), data = m4.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Province")
# They're the same provinces causing the issues in both models

## dig a little deeper into the provinces
provs <- c("Battambang","Kampong Chhnang","Kampong Thom","Kracheh","Pursat","Ratanak Kiri","Siem Reap",
              "Stung Treng")
## if you look at the map (in GIS), the provinces with the smallest residuals are the smaller provinces in the south surrounding PP. These are provinces that generally have very little forest. But they are not the only provinces with no forest cover, so I am not sure that is the (only) reason. I think I need to look at the communes within the provinces


# extract all original data from those provinces
prov.dat <- dat %>% filter(Province %in% provs) %>% select(Province,Commune,year,ForPix,pop_den,tot_pop)

# create subset of all data NOT including the above
prov.excl <- dat %>% filter(!Province %in% provs)

# compare ForPix between the above selection and the data overall, and the rest of the data (excluding selection)
par(mfrow=c(2,2))
hist(prov.dat$ForPix, main="prov subset")
hist(dat$ForPix, main="All data")
hist(prov.excl$ForPix, main="exclude provs")

# compare scatter plot of ForPix across communes, split by the provinces
ggplot()+
  geom_point(data=prov.dat, aes(x=Commune, y=ForPix),colour="red")+
  geom_point(data=prov.excl, aes(x=Commune, y=ForPix), colour="green")+
  ylim(0,250)
# I would say that the provinces that are causing the issues tend to have higher ForPix than the others. It also look potentially like the provinces causing issues are almost always losing forest cover over time (lots of vertical lines of dots)

# scatter plot of ForPix against pop_den, split by provinces
ggplot()+
  geom_point(data=prov.dat, aes(x=pop_den, y=ForPix),colour="red")+
  geom_point(data=prov.excl, aes(x=pop_den, y=ForPix),colour="green")+
  ylim(0,10000)+
  xlim(0,1000)
# This plot makes it also looks like the problem provinces tend to have communes with higher ForPix. There is a chunk of red with no green where pop_den has increased to about 100 but ForPix has not decreased (whereas all the green dots are lower, i.e. in the other provinces/communes at that population density forest cover is lower). 

# boxplot of ForPix and province, coloured by the two groups of provinces 
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)
# this plot suggests that the problem provinces don't necessarily have more forest cover, but they do tend to have more outliers that are very high forest cover.  

# boxplot of pop_den and province, coloured by the two groups of provinces
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=pop_den, colour="Other provinces"))
# This plot suggests that overall, the problem provinces tend to have lower median pop_den value compared to the other provinces, but they again tend to have more outliers

# boxplot of Commune and pop_den, coloured by the two groups of provinces
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=pop_den, colour="Other provinces"))+
  ylim(0,1000)
# again this looks like there are more communes with higher pop_den values in the non-problematic provinces


# boxplot of Commune and ForPix, coloured by the two groups of provinces
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)
# this plot is quite hard to see much, as there's too much info.  However, I would say that it looks like the problem provinces in general have more variation in ForPix, both within communes and between communes. 


### If you look at the plot of the main effect in the section below, the line is pretty flat, even at low pop_den and ForPix values. This is where the model is not predicitng well for communes with low population density but higher forest cover. This is because there is so much variation in the communes - i.e. there are so many that have low pop_den but low ForPix, which is dragging the model down.  So the communes with large residuals are going to be the commune with low pop_den values and higher ForPix values I think.

#
      # predict main effects ####

### predict main effects for an average commune

# create new data
m6.newdat <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                        areaKM = mean(dat1$areaKM))

# add predictions
m6.newdat$pred <- as.vector(predict(popdem.m6, type="response", newdata=m6.newdat, re.form=NA))

# plot
ggplot(m6.newdat, aes(x=pop_den, y=pred))+
  geom_line()+
  ylim(0,5000)


#
      # predict for sets of communes ####

# here I want to plot grids of different communes with the overall predicted effect, plus the commune-specific effect. I want to do this for communes with commune-level intercepts close to the mean, and communes with commune-level intercpets at the extremes

# save the popdem.m4 commune-level random effects
m6.re.com <- ranef(popdem.m6)[[1]]
plot_model(popdem.m6, type="re")

# re-order
m6.re.com <- m6.re.com[order(m6.re.com[ ,"(Intercept)"], decreasing = FALSE),]
head(m6.re.com)

# the 4 communes closest to 0 are:
# Pursat_Ansa Chambak:Pursat, 
# Kampong Cham_Kraek:Kampong Cham, 
# Ratanak Kiri_Pak Nhai:Ratanak Kiri, 
# Kampong Speu_Tang Samraong:Kampong Speu

# the 4 communes furthest above 0 are:
# Kampong Chhnang_Chieb:Kampong Chhnang
# Kampot_Preaek Tnaot:Kampot
# Battambang_Chhnal Moan:Battambang
# Phnom Penh_Chak Angrae Kraom:Phnom Pen

# the 4 communes furthest below 0 are:
# Kampong Thom_Chaeung Daeung:Kampong Thom
# Kracheh_Han Chey:Kracheh
# Siem Reap_Nokor Pheas:Siem Reap
# Kampong Thom_Tang Krasang:Kampong Thom


# which communes
coms <- c("Pursat_Ansa Chambak","Kampong Cham_Kraek", "Ratanak Kiri_Pak Nhai","Kampong Speu_Tang Samraong",
          "Kampong Chhnang_Chieb","Kampot_Preaek Tnaot","Battambang_Chhnal Moan","Phnom Penh_Chak Angrae Kraom",
          "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Siem Reap_Nokor Pheas","Kampong Thom_Tang Krasang")

# which provinces
provs <- c("Pursat","Kampong Cham","Ratanak Kiri","Kampong Speu",
           "Kampong Chhnang","Kampot","Battambang","Phnom Penh",
           "Kampong Thom","Kracheh","Siem Reap","Kampong Thom")


### I am making commune-specific predictions, so should customise the range of population densities I am predicting for, on the basis of each commune.

### Easiest to define the range of pop_den to predict for, first. Min/max per commune:
pop_den_min <- tapply(dat1$pop_den, dat1$Provcomm, min)
pop_den_max <- tapply(dat1$pop_den, dat1$Provcomm, max)
### Min/max within your selection of communes:
pop_den_min <- min(pop_den_min[names(pop_den_min) %in% coms])
pop_den_max <- max(pop_den_max[names(pop_den_max) %in% coms])
### Not really any different from the overall min and max
min(dat1$pop_den)
max(dat1$pop_den)

# create new prediction grid for specific communes with varying pop_den
m6_newdat_com <- data.frame(Provcomm = rep(coms, each=100),
                            Province = rep(provs, each=100),
                            pop_den = seq(from=pop_den_min, to=pop_den_max, length.out = 100),
                            year = mean(dat1$year))


# add commune-specific areaKM offset                         
m6_newdat_com$areaKM <-  dat1$areaKM[match(m6_newdat_com$Provcomm, dat1$Provcomm)]

# re-order levels so they plot in the correct sets
m6_newdat_com$Provcomm <- factor(m6_newdat_com$Provcomm, 
                                 levels = c("Pursat_Ansa Chambak","Kampong Cham_Kraek", "Ratanak Kiri_Pak Nhai",
                                            "Kampong Speu_Tang Samraong","Kampong Chhnang_Chieb","Kampot_Preaek Tnaot",
                                            "Battambang_Chhnal Moan","Phnom Penh_Chak Angrae Kraom",
                                            "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Siem Reap_Nokor Pheas",
                                            "Kampong Thom_Tang Krasang"))


# attach commune-specific predictions
m6_newdat_com$pred.com <- as.vector(predict(popdem.m6, type="response", newdata=m6_newdat_com, 
                                        re.form=~(year|Province/Provcomm)))


# attach global predictions
m6_newdat_com$pred.glo <- rep(m6.newdat$pred, times=12)


### The following plot can either "overlay" the observed ForPix count against observed population densities for the communes, or I can split by commune. comment out the if(i==1) statement and set par() if you want a grid

### Pick some colours using RColorBrewer using a completely overelaborate piece of crap code... Anyway this is just to
#try to help see the differences between communes more clearly in the observed points in particular (you can comment the
#following lines out if you want)
require(RColorBrewer)
com_colours <- brewer.pal(11, "RdYlBu")
com_colours <- c(head(com_colours,4),tail(com_colours,4))
com_colours_greys <- tail(brewer.pal(9, "Greys"),4)
com_colours <- c(com_colours, com_colours_greys)
com_colours <- com_colours[sample(1:length(com_colours),12,replace=F)]

### This is just to check if you have commented tbe above out: :)
if(!exists("com_colours")) {
  com_colours <- rep("black", 12)
}
provcomm_lvls <- levels(m6_newdat_com$Provcomm) 
par(mfrow = c(3,4))
### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### population density range across all communes, so we need to do this overall:
ylo <- min(m6_newdat_com$pred.com)*0.9
yhi <- max(m6_newdat_com$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(m6_newdat_com$Provcomm),"pop_den"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(m6_newdat_com$Provcomm),"pop_den"])
### Iterate through the communes (levels in m6_newdat_com$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- m6_newdat_com[m6_newdat_com$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
 # if(i == 1) {
    # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
    plot(preddat_i$pop_den,preddat_i$pred.com, 
         type = "l", 
         col = com_colours[i], 
         ylim = c(ylo,yhi), xlim = c(xlo,xhi),
         xlab = "Population density (scaled & standardised",
         ylab = "Predicted forest cover (forest pixel count)")
 # } else {
    lines(preddat_i$pop_den,preddat_i$pred.com, col = com_colours[i])
    lines(preddat_i$pop_den,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$pop_den, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}


### so the plots support the diagnostics in the section above - the global model does not predict well for communes with high forest cover - because the global model always predicts low forest cover, even for communes with low population density.  Essentially there are too many communes with low forest cover, and so the global model is dragged down, meaning that the communes with high forest cover get poor predictions. This does suggest though that the global model predicts well at high population density values, as these communes tend to have low forest cover.  


### now let's do the same as above but for a random selection of communes

# randomly sample communes
rand.com <- sample(dat1$Provcomm, 12, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17,19,21,23)]

# define the range of pop_den to predict for, first. Min/max per commune:
pop_den_min <- tapply(dat1$pop_den, dat1$Provcomm, min)
pop_den_max <- tapply(dat1$pop_den, dat1$Provcomm, max)
# Min/max within your selection of communes:
pop_den_min <- min(pop_den_min[names(pop_den_min) %in% rand.com])
pop_den_max <- max(pop_den_max[names(pop_den_max) %in% rand.com])
# min not really different but max very different
min(dat1$pop_den)
max(dat1$pop_den)

# create new prediction grid for specific communes with varying pop_den
m6_newdat_com_ran <- data.frame(Provcomm = rep(rand.com, each=100),
                            Province = rep(rand.prov, each=100),
                            pop_den = seq(from=pop_den_min, to=pop_den_max, length.out = 100),
                            year = mean(dat1$year))


# add commune-specific areaKM offset                         
m6_newdat_com_ran$areaKM <-  dat1$areaKM[match(m6_newdat_com_ran$Provcomm, dat1$Provcomm)]


# attach commune-specific predictions
m6_newdat_com_ran$pred.com <- as.vector(predict(popdem.m6, type="response", newdata=m6_newdat_com_ran, 
                                        re.form=~(year|Province/Provcomm)))


# attach global predictions
m6_newdat_com_ran$pred.glo <- rep(m6.newdat$pred, times=12)

# set levels
m6_newdat_com_ran$Provcomm <- as.factor(m6_newdat_com_ran$Provcomm)
provcomm_lvls <- levels(m6_newdat_com_ran$Provcomm) 
par(mfrow = c(3,4))

# set scales
ylo <- min(m6_newdat_com_ran$pred.com)*0.9
yhi <- max(m6_newdat_com_ran$pred.com)*1.1
xlo <- pop_den_min
xhi <- pop_den_max

# Iterate through the communes (levels in m6_newdat_com$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- m6_newdat_com_ran[m6_newdat_com_ran$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
 # if(i == 1) {
    # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
    plot(preddat_i$pop_den,preddat_i$pred.com, 
         type = "l", 
         col = com_colours[i], 
         ylim = c(ylo,yhi), xlim = c(xlo,xhi),
         xlab = "Population density (scaled & standardised",
         ylab = "Predicted forest cover (forest pixel count)")
 # } else {
    lines(preddat_i$pop_den,preddat_i$pred.com, col = com_colours[i])
    lines(preddat_i$pop_den,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$pop_den, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}


### after running the above random plotting a few times, the trend seems to hold - i.e. the global model predics well for communes with low forest cover and/or high population density, but does not perform well for communes with high forest cover

### I am not convinced that having both pop_den and tot_pop in the model is sensible (especially as an interaction). I have gone back and forth because I do think they tell us different things about a commune, and between communes they are not necessarily collinear. However within communes over time they are - i.e. in a given commune, over time, in order for pop_den to increase tot_pop must increase. Therefore as Jeroen said, it is a bit dangerous to have both in the same model as an interaction.  I also have a LOT of potential predictors, and so I need to be selective in which ones I take forward, and pop_den is the more important of the two.

### pop_den taken forward


#
  ## Education ####

# just one var - M6_24_sch

    # edu.m1 ####

# only one variable here
edu.m1 <- glmer(ForPix ~ M6_24_sch + offset(log(areaKM)) + (year|Province/Provcomm), family="poisson", data=dat1)

summary(edu.m1)
# Province and commune variance similar to popdem models. Year variance relatively very small both for province and commune (although same order of magnitude as each other, unlike popdem models).  predictor has very small positive effect, with approximate p value >0.9. 

# quick plot
plot_model(edu.m1, type="pred", terms="M6_24_sch")
# flat as a pancake

    # diagnostics ####

# check fixed var 
drop1(edu.m1, test="Chisq")

# Not much point in going any further - there is no relationship

#
  ## Employment ####
    # emp.m1 (propPrimsec, propSecSec) ####

# I am not going to test an interaction because these two variables are likely to be related, i.e. when there are fewer people in the primary sector there are likely to be more people in the secondary sector, and vice versa
emp.m1 <- glmer(ForPix ~ propPrimSec + propSecSec + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(emp.m1)
# province and commune variance similar to other model sets.  year variance for commune is order of magnitude larger than for province, but both very small. propPrimSec has positive effect, propSecSec has negative, but both very small and approximate p values > 0.5

# quick plot
plot_model(emp.m1, type="pred")

    # model selection ####

# see if we remove propSecSec
emp.m2 <- glmer(ForPix ~ propPrimSec + offset(log(areaKM)) + (year|Province/Provcomm), family="poisson", data=dat1)

summary(emp.m2)

anova(emp.m1, emp.m2, test="Chisq")
# simpler model is better

plot_model(emp.m2, type="pred")

# still a useless model though. No relationship

#
  ## Economic security ####

# Les_1_R_Land & pig_fam

    # econ.m1 ####

# model with both variables. I don't have any a priori hypothesis about an interaction, and so I will not test one
econ.m1 <- glmer(ForPix ~ Les1_R_Land + pig_fam + offset(log(areaKM)) + (year|Province/Provcomm),
                 family="poisson", data=dat1)

summary(econ.m1)

# quick plot
plot_model(econ.m1, type="pred")

    # model selection ####

# try models with only single vars
econ.m2 <- glmer(ForPix ~ Les1_R_Land + offset(log(areaKM)) + (year|Province/Provcomm),
                 family="poisson", data=dat1)

summary(econ.m2)

anova(econ.m1, econ.m2, test="Chisq")
# m2 is better than m1


econ.m3 <- glmer(ForPix ~ pig_fam + offset(log(areaKM)) + (year|Province/Provcomm),
                 family="poisson", data=dat1)

summary(econ.m3)

anova(econ.m1, econ.m3, test="Chisq")
# m3 is better than m1

anova(econ.m1, econ.m2, econ.m3, test="Chisq")
# m3 is the best model

# quick plot
plot_model(econ.m3, type="pred")

## no relationship with either variable

#
  ## Access to services ####

# dist_sch, garbage, KM_Comm

    # acc.m1 ####

# I have no a priori hypotheses regarding interactions, and so I won't test them
acc.m1 <- glmer(ForPix ~ dist_sch + garbage + KM_Comm + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(acc.m1)

# quick plot
plot_model(acc.m1, type="pred")

    # model selection ####

acc.m2 <- glmer(ForPix ~ dist_sch + garbage + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

acc.m3 <- glmer(ForPix ~ dist_sch + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

acc.m4 <- glmer(ForPix ~ garbage + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

acc.m5 <- glmer(ForPix ~ KM_Comm + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(acc.m2)
summary(acc.m3)
summary(acc.m4)


anova(acc.m1,acc.m2,acc.m3,acc.m4,acc.m5, test="Chisq")
# acc.m4 is the best apparently

plot_model(acc.m4, type="pred")

#
  ## Social justice ####

# crim_case, land_confl

    # jus.m1 ####

# no a priori reason to think there is an interaction and so I won't test
jus.m1 <- glmer(ForPix ~ crim_case + land_confl + offset(log(areaKM)) + (year|Province/Provcomm),
                          family="poisson", data=dat1)

summary(jus.m1)

# quick plot
plot_model(jus.m1, type="pred")

    # jus.m2 ####

# remove land_confl
jus.m2 <- glmer(ForPix ~ crim_case + offset(log(areaKM)) + (year|Province/Provcomm),
                          family="poisson", data=dat1)

summary(jus.m2)


  ## migration ####
    # mig.m1 ####

# Here I think there is a possible cause to test interactions. Migration in an out of a commune, and what this means for socioeconomics and forest cover is potentially quite complex. 
mig.m1 <- glmer(ForPix ~ Pax_migt_in*Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m1)

plot_model(mig.m1, type="int")
plot_model(mig.m1, type="pred")

    # mig.m2 ####

# remove migt_in
mig.m2 <- glmer(ForPix ~ Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m2)

plot_model(mig.m2, type="pred")

#
  ## Environmental vars ####
    # env.m1 ####

### UPDATE - I have decided not to use habitat.  This is because the habitat layer was used to create the response (ForPix), and so the two are intrinsically linked, and makes interpretation challenging. Skip to env.m2 sections 


# CP - cropland
# MC - Mosaic crop
# MN - Mosaic natural
# FBE - Forest broadleaved, evergreen
# FBD - Forest broadleaved, deciduous
# MTSH - Mosaic, tree, shrub, herbaceous cover 
# SL - Shrubland
# GL - Grassland
# NF - Natural cover, flooded
# W - Water
# nd - No data


# there is plausible reasons why the effect of elevation might vary depending on habitat type, therefore an interacion was tested, but there were model issues (rank deficiency and non-convergence). Therefore the interaction was removed and the model ran with no warning
env.m1 <- glmer(ForPix ~ mean_elev+habitat + offset(log(areaKM)) + (year|Province/Provcomm),
                family = "poisson", data=dat1)

summary(env.m1)

# quick plot
plot_model(env.m1, type="pred")

ranef(env.m1)

fixef(env.m1)

    # Diagnostics m1 - IGNORE ####

# copy data
env.m1.diag <- dat1
env.m1.diag$Provcomm <- as.factor(env.m1.diag$Provcomm)

# attach residuals
env.m1.diag$m1res <- resid(env.m1)

# attach conditional predictions
env.m1.diag$m1pred <- as.vector(predict(env.m1, type="response"))

# plot predicted vs observed
plot(env.m1.diag$m1pred, env.m1.diag$ForPix)
# good

# residuals vs fitted
plot(env.m1.diag$m1pred, env.m1.diag$m1res)
# heteroskedasicity - particularly bad a low predicted forest cover. Similar to the popdem models.


# repeating the plot but colouring by commune:
colfunc = colorRampPalette(c("red","royalblue"))
env.m1.diag$Provcomm_colours = env.m1.diag$Provcomm
levels(env.m1.diag$Provcomm_colours) = heat.colors((nlevels(env.m1.diag$Provcomm)))

# predicted values vs residuals, coloured by commune
plot(env.m1.diag$m1pred, env.m1.diag$m1res, col = env.m1.diag$Provcomm_colours)

# zoom in on the x axis
plot(env.m1.diag$m1pred, env.m1.diag$m1res, col = env.m1.diag$Provcomm_colours, xlim = c(0,2000))

# ForPix vs residuals, coloured
plot(env.m1.diag$ForPix, env.m1.diag$m1res, col = env.m1.diag$Provcomm_colours)

## Bit more exploration of residuals, but now over the explanatory variable:
par(mfrow=c(2,2))
plot(env.m1.diag$mean_elev, env.m1.diag$m1res, ylab = "residuals", xlab = "mean elevation")
boxplot(m1res ~ factor(habitat), data = env.m1.diag, outline = T, xlab("habitat"), 
        ylab = "residuals w/i habitat")
boxplot(m1res ~ factor(Province), data = env.m1.diag, outline = T, xlab = "Province", 
        ylab = "Residuals w/i Province")
boxplot(m1res ~ factor(Provcomm), data = env.m1.diag, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")
# CP, FBE, and NF are particularly bad, but FBD, MC, MTSH, and W aren't great either. It doesn't appear to be a problem with number of observations in each category - for example CP has nearly half of all the obs, and it is one of the worst.  Whereas SL is one of the best, and it only has 17 obs.  

# check if the provinces that have the communes with the largest residuals are the same as from the popdem model
levels(env.m1.diag$Province)
# Battambang, Kampong Chhnang, Kampong Thom, Kracheh, Otdar Meanchey, Pursat, Siem Reap, Stung Treng.

# Yes - basically the same provinces causing the issues

# compare ForPix between the problem provinces and the rest
provs <- c("Battambang","Kampong Chhnang","Kampong Thom","Kracheh","Otdar Meanchey","Pursat","Siem Reap",
           "Stung Treng")
prob.provs <- dat[dat$Province %in% provs,]
prob.provs$type <- "problem"
other.provs <- dat %>% filter(!Province %in% provs) 
other.provs$type <- "other"
all.provs <- rbind(prob.provs,other.provs)


ggplot()+
  geom_boxplot(data=prob.provs, aes(x=Province, y=ForPix, colour="problem provinces"))+
  geom_boxplot(data=other.provs, aes(x=Province, y=ForPix, colour="other provinces"))
# I dunno, not much to see really.  Perhaps the problem provinces tend to have more outliers, but not really any obvious pattern

# plot habitat between the provinces
ggplot(all.provs, aes(x=habitat, y=Province, fill=type))+
  geom_boxplot()
# I think some of the issues are coming from the fact that not all the provinces have communes with all of the habitat types

# plots comparing elevation
ggplot()+
  geom_boxplot(data=prob.provs, aes(x=Province, y=mean_elev, colour="problem provinces"))+
  geom_boxplot(data=other.provs, aes(x=Province, y=mean_elev, colour="other provinces"))
# looks like the provinces causing issues tend to have lower mean elevations across their communes

# compare mean_elev and ForPix between the groups of provinces
ggplot(all.provs, aes(x=mean_elev, y=ForPix, colour=type))+
  geom_point()
# looks like the problem province tend to have higher ForPix (same as the popdem models) but lower elevation

# QQ plot
par(mfrow=c(1,1))
qqnorm(resid(env.m1))
qqline(resid(env.m1))

    # model selection ####

env.m2 <- glmer(ForPix ~ mean_elev + offset(log(areaKM)) + (year|Province/Provcomm),
                family = "poisson", data=dat1)

anova(env.m1, env.m2, test="Chisq")
# The simplified model is not better

env.m3 <- glmer(ForPix ~ habitat + offset(log(areaKM)) + (year|Province/Provcomm),
                family = "poisson", data=dat1)

anova(env.m1, env.m3, test="Chisq")
# the simplified model is not better

### UPDATE - I am not using habitat anymore (see text in env.m1 section above)

#
    # predictions - env.m1 - IGNORE ####

### predict for an average commune (i.e. ignoring REs) 

# create new data
m1_newdat <- expand.grid(mean_elev = seq(from=min(dat1$mean_elev), to=max(dat1$mean_elev), length.out = 100),
                         habitat = levels(dat1$habitat),
                         areaKM = mean(dat1$areaKM))
# remove "nd"
m1_newdat <- m1_newdat[m1_newdat$habitat != "nd",]


# predict
m1_newdat$pred <- as.vector(predict(env.m1, type="response", newdata=m1_newdat, re.form=NA))

# colours
my_pal <- colorRampPalette(brewer.pal(10,"Set3"))
my_pal <- colorRampPalette(colorRamp(10,"primary.colors"))

# plot
ggplot(m1_newdat, aes(x=mean_elev, y=pred, group=habitat, colour=habitat))+
  geom_line(size=1)+
  scale_colour_manual(values = my_pal(nlevels(m1_newdat$habitat)))+
  theme(element_blank())

ggplot(m1_newdat, aes(x=mean_elev, y=pred, group=habitat, colour=habitat))+
  geom_line(size=1)+
  scale_colour_brewer(palette = "Paired")+
  theme(element_blank())
# Ok so predicted forest cover increases as mean elevation increases. The effect is differnet for the different habitats. The highest predicted forest cover is in Mosaic (tree, shrub, herbaceous), Forest (broadleaved, evergree), and water.  The lowest predicted forest cover is in mosaic (natural), shrubland. Interestingly predicted forest cover is higher in cropland than in mosaic (natural) and shrubland. This is possible because the habitat is allocated based on >50% of the commune, which means that there can still be a decent proportion of the communes that is forested, despite the habitat being classified as, say, cropland. 

#
    # diagnostics m2 ####

# copy data
env.m2.diag <- dat1
env.m2.diag$Provcomm <- as.factor(env.m2.diag$Provcomm)

# attach residuals
env.m2.diag$m2res <- resid(env.m2)

# attach conditional predictions
env.m2.diag$m2pred <- as.vector(predict(env.m2, type="response"))

# plot predicted vs observed
plot(env.m2.diag$m2pred, env.m2.diag$ForPix)
# good

# residuals vs fitted
plot(env.m2.diag$m2pred, env.m2.diag$m2res)
# heteroskedasicity - particularly bad a low predicted forest cover. Similar to the popdem models.


# repeating the plot but colouring by commune:
colfunc = colorRampPalette(c("red","royalblue"))
env.m2.diag$Provcomm_colours = env.m2.diag$Provcomm
levels(env.m2.diag$Provcomm_colours) = heat.colors((nlevels(env.m2.diag$Provcomm)))

# predicted values vs residuals, coloured by commune
plot(env.m2.diag$m2pred, env.m2.diag$m2res, col = env.m2.diag$Provcomm_colours)

# zoom in on the x axis
plot(env.m2.diag$m2pred, env.m2.diag$m2res, col = env.m2.diag$Provcomm_colours, xlim = c(0,2000))

# ForPix vs residuals, coloured
plot(env.m2.diag$ForPix, env.m2.diag$m2res, col = env.m2.diag$Provcomm_colours)

## Bit more exploration of residuals, but now over the explanatory variable:
par(mfrow=c(2,2))
plot(env.m2.diag$mean_elev, env.m2.diag$m2res, ylab = "residuals", xlab = "mean elevation")
boxplot(m2res ~ factor(Province), data = env.m2.diag, outline = T, xlab = "Province", 
        ylab = "Residuals w/i Province")
boxplot(m2res ~ factor(Provcomm), data = env.m2.diag, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")
# There seems to be a slightly odd pattern in the first plot - it looks like there are certain elevation values that produce large residuals

# zoom in
plot(env.m2.diag$mean_elev, env.m2.diag$m2res, xlim = c(-1,2),ylab = "residuals", xlab = "mean elevation")
# looks like elevation values of approx -0.6, -0.4, -0.1, 0.2, 0.8, 

# What are the provinces with the largest residuals?
levels(env.m2.diag$Province)

provs <- c("Battambang","Kampong Cham","Kampong Chhnang","Kampong Thom","Koh Kong","Kracheh","Mondul Kiri"
               ,"Otdar Meanchey","Preah Sihanouk","Pursat","Ratanak Kiri","Siem Reap","Stung Treng")
prob.provs <- dat %>% filter(Province %in% provs)
prob.provs$type <- "problem"
other.provs <- dat %>% filter(!Province %in% provs)
other.provs$type <- "other"
all.provs <- rbind(prob.provs,other.provs)

# get provincial means
prov.elev.mean <- all.provs %>% group_by(Province) %>% summarise(mean = mean(mean_elev))
prov.elev.mean$type <- ifelse(prov.elev.mean$Province %in% provs, "problem", "other")

# plot mean elevation by province
ggplot(prov.elev.mean, aes(x=Province, y=mean, colour=type))+
  geom_point(size=4)+
  theme(element_blank())
# not a stiking trend, but the 4 provinces with the lowest mean elevation are all not problematic ones. 

arrange(prov.elev.mean, mean)
# lowest means are Svay Rieng, Kandal, PP, Prey Veng, which are numbers 8, 23, 15, 18 (to compare with plot).So those provinces have very small residuals.

# now do the same but using dat1 (scaled vars) so I can try and see where that odd pattern is coming from in the plot above
prob.provs <- dat1 %>% filter(Province %in% provs)
prob.provs$type <- "problem"
other.provs <- dat1 %>% filter(!Province %in% provs)
other.provs$type <- "other"
all.provs <- rbind(prob.provs,other.provs)

# get provincial means
prov.elev.mean <- all.provs %>% group_by(Province) %>% summarise(mean = mean(mean_elev))
prov.elev.mean$type <- ifelse(prov.elev.mean$Province %in% provs, "problem", "other")

# plot mean elevation by province
ggplot(prov.elev.mean, aes(x=Province, y=mean, colour=type))+
  geom_point(size=4)+
  theme(element_blank())
# this is the wrong plot to investigate that pattern - I need to look at communes

# pull out the communes from env.m2.diag that have residual values that match the weird pattern and see if there is any other pattern that might explain the residual pattern
# elevation values of approx -0.6, -0.4, -0.1, 0.2, 0.8

# zoom further
plot(env.m2.diag$mean_elev, env.m2.diag$m2res, xaxp = c(-0.6, 1, 8), xlim = c(-0.6,1), ylim = c(-5,5),
     ylab = "residuals", xlab = "mean elevation")

prob.coms <- env.m2.diag %>% filter(m2res > 0.5 | m2res < -0.5)
hist(prob.coms$m2res)

plot(prob.coms$mean_elev, prob.coms$m2res)
# ok so these are now the communes with the weird shape

unique(prob.coms$Province)
# Battambang, Kampong Cham, Kampong Chhnang, Kampong Thom, Kampot, Kandal, Koh Kong, Kracheh, Mondul Kiri, Pursat, Ratanak Kiri, Siem Reap, Stung Treng, Otdar Meanchey, Preah Sihanouk, Preah Vihear. 
provs

# plot the elevation
plot(prob.coms$Commune, prob.coms$mean_elev)
# hard to see much here

# pull out these communes from dat so I can see the elevation on original scale
coms <- prob.coms$Commune
prob.coms.orig <- dat[dat$Commune %in% coms,]
other.coms.orig <- dat %>% filter(!Commune %in% coms)

ggplot()+
  geom_point(data=other.coms.orig, aes(x=Commune, y=mean_elev, colour="other"))+
  geom_point(data=prob.coms.orig, aes(x=Commune, y=mean_elev, colour="problem"))+
  theme(element_blank())
# no obvious pattern here.

# the other fixed effect is the offset - areaKM.  Perhaps this is the source of the issue
ggplot()+
  geom_point(data=other.coms.orig, aes(x=Commune, y=areaKM, colour="other"))+
  geom_point(data=prob.coms.orig, aes(x=Commune, y=areaKM, colour="problem"))+
  theme(element_blank())

# look at ForPix between the two sets
ggplot()+
  geom_point(data=other.coms.orig, aes(x=Commune, y=ForPix, colour="other"))+
  geom_point(data=prob.coms.orig, aes(x=Commune, y=ForPix, colour="problem"))+
  theme(element_blank())
# ok so there is an obvious difference here.  There problem communes clearly mostly lose forest over time (vertical lines of dots), whereas the others generally do not (single points). This is the same issue as highlighted in the popdem model.

# just for shits and giggles, lets see if this pattern exists if I put year in as a fixed effect
env.m4 <- glmer(ForPix ~ mean_elev + year + offset(log(areaKM)) + (year|Province/Commune),
                family="poisson", data=dat1)

summary(env.m4)

m4.diag.dat <- dat1
m4.diag.dat$m4res <- resid(env.m4)
m4.diag.dat$m4pred <- as.vector(predict(env.m4, type="response"))

plot(m4.diag.dat$m4pred, m4.diag.dat$m4res)
# pretty similar to m2

plot(m4.diag.dat$mean_elev, m4.diag.dat$m4res)
# pattern is still there

#
    # predict main effects m2 ####

### predict from env.m2 for an average commune

# create new data
env_m2_newdata <- data.frame(mean_elev = seq(from=min(dat1$mean_elev), to=max(dat1$mean_elev),
                                             length.out = 100),
                             areaKM = mean(dat1$areaKM))
# predict
env_m2_newdata$pred <- as.vector(predict(env.m2, type="response", newdata=env_m2_newdata, re.form=NA))

# plot
ggplot(env_m2_newdata, aes(x=mean_elev, y=pred))+
  geom_line()+
  theme(element_blank())+
  ylim(0,20000)+
  xlab("Mean elevation (scaled)")+
  ylab("Predicted forest cover (pixels)")

    # predict for sets of communes ####

# here I want to plot grids of different communes with the overall predicted effect, plus the commune-specific effect. I want to do this for communes with commune-level intercepts close to the mean, and communes with commune-level intercpets at the extremes

# save the env.m2 commune-level random effects
env.m2.com <- ranef(env.m2)[[1]]
plot_model(env.m2, type="re")

# re-order
env.m2.com <- env.m2.com[order(env.m2.com[ ,"(Intercept)"], decreasing = FALSE),]
head(env.m2.com)

# the 4 communes closest to 0 are:
# Koh Kong_Bak Khlang:Koh Kong, 
# Kracheh_Bos Leav:Kracheh, 
# Preah Vihear_Kuleaen Tboung:Preah Vihear, 
# Ratanak Kiri_Saom Thum:Ratanak Kiri

# the 4 communes furthest above 0 are:
# Kampong Cham_Tuol Snuol:Kampong Cham
# Banteay Meanchey_Paoy Char:Banteay Meanchey
# Kampong Cham_Khpob Ta Nguon:Kampong Cham
# Kampong Chhnang_Dar:Kampong Chhnang

# the 4 communes furthest below 0 are:
# Kampong Thom_Chaeung Daeung:Kampong Thom
# Kracheh_Han Chey:Kracheh 
# Kampong Thom_Tang Krasang:Kampong Thom 
# Siem Reap_Nokor Pheas:Siem Reap


# which communes
coms <- c("Koh Kong_Bak Khlang","Kracheh_Bos Leav","Preah Vihear_Kuleaen Tboung","Ratanak Kiri_Saom Thum",
          "Kampong Cham_Tuol Snuol","Banteay Meanchey_Paoy Char","Kampong Cham_Khpob Ta Nguon","Kampong Chhnang_Dar",
          "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Kampong Thom_Tang Krasang","Siem Reap_Nokor Pheas")

# which provinces
provs <- c("Koh Kong","Kracheh","Preah Vihear","Ratanak Kiri",
           "Kampong Cham","Banteay Meanchey","Kampong Cham","Kampong Chhnang",
           "Kampong Thom","Kracheh","Kampong Thom","Siem Reap")


### I am making commune-specific predictions, so should customise the range of elevations I am predicting for, on the basis of each commune.

### Easiest to define the range of mean_elev to predict for, first. Min/max per commune:
mean_elev_min <- tapply(dat1$mean_elev, dat1$Provcomm, min)
mean_elev_max <- tapply(dat1$mean_elev, dat1$Provcomm, max)
### Min/max within your selection of communes:
mean_elev_min <- min(mean_elev_min[names(mean_elev_min) %in% coms])
mean_elev_max <- max(mean_elev_max[names(mean_elev_max) %in% coms])
### Not really any different for the min, but the max is different
min(dat1$mean_elev)
max(dat1$mean_elev)

# create new prediction grid for specific communes with varying mean_elev
env_m2_newdat_com <- data.frame(Provcomm = rep(coms, each=100),
                            Province = as.factor(rep(provs, each=100)),
                            mean_elev = seq(from=mean_elev_min, to=mean_elev_max, length.out = 100),
                            year = mean(dat1$year))


# add commune-specific areaKM offset                         
env_m2_newdat_com$areaKM <-  dat1$areaKM[match(env_m2_newdat_com$Provcomm, dat1$Provcomm)]

# re-order levels so they plot in the correct sets
env_m2_newdat_com$Provcomm <- factor(env_m2_newdat_com$Provcomm, 
                                 levels = c("Koh Kong_Bak Khlang",
                                            "Kracheh_Bos Leav",
                                            "Preah Vihear_Kuleaen Tboung",
                                            "Ratanak Kiri_Saom Thum",
                                            "Kampong Cham_Tuol Snuol",
                                            "Banteay Meanchey_Paoy Char",
                                            "Kampong Cham_Khpob Ta Nguon",
                                            "Kampong Chhnang_Dar",
                                            "Kampong Thom_Chaeung Daeung",
                                            "Kracheh_Han Chey",
                                            "Kampong Thom_Tang Krasang",
                                            "Siem Reap_Nokor Pheas"))


# attach commune-specific predictions
env_m2_newdat_com$pred.com <- as.vector(predict(env.m2, type="response", newdata=env_m2_newdat_com, 
                                            re.form=~(year|Province/Provcomm)))


# attach global predictions
env_m2_newdat_com$pred.glo <- rep(env_m2_newdata$pred, times=12)


### The following plot can either "overlay" the observed ForPix count against observed population densities for the communes, or I can split by commune. comment out the if(i==1) statement and set par() if you want a grid

### Pick some colours using RColorBrewer using a completely overelaborate piece of crap code... Anyway this is just to
#try to help see the differences between communes more clearly in the observed points in particular (you can comment the
#following lines out if you want)
require(RColorBrewer)
com_colours <- brewer.pal(11, "RdYlBu")
com_colours <- c(head(com_colours,4),tail(com_colours,4))
com_colours_greys <- tail(brewer.pal(9, "Greys"),4)
com_colours <- c(com_colours, com_colours_greys)
com_colours <- com_colours[sample(1:length(com_colours),12,replace=F)]

### This is just to check if you have commented tbe above out: :)
if(!exists("com_colours")) {
  com_colours <- rep("black", 12)
}
provcomm_lvls <- levels(env_m2_newdat_com$Provcomm) 
par(mfrow = c(3,4))
### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### population density range across all communes, so we need to do this overall:
ylo <- min(env_m2_newdat_com$pred.com)*0.9
yhi <- max(env_m2_newdat_com$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(env_m2_newdat_com$Provcomm),"mean_elev"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(env_m2_newdat_com$Provcomm),"mean_elev"])
### Iterate through the communes (levels in m6_newdat_com$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- env_m2_newdat_com[env_m2_newdat_com$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  # if(i == 1) {
  # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$mean_elev,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Mean elevation (scaled & standardised",
       ylab = "Predicted forest cover (forest pixel count)",
       main = unique(preddat_i$Provcomm))
  # } else {
  lines(preddat_i$mean_elev,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$mean_elev,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$mean_elev, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}



### now let's do the same as above but for a random selection of communes

# randomly sample communes
rand.com <- sample(dat1$Provcomm, 12, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17,19,21,23)]

# define the range of pop_den to predict for, first. Min/max per commune:
mean_elev_min <- tapply(dat1$mean_elev, dat1$Provcomm, min)
mean_elev_max <- tapply(dat1$mean_elev, dat1$Provcomm, max)
# Min/max within your selection of communes:
mean_elev_min <- min(mean_elev_min[names(mean_elev_min) %in% rand.com])
mean_elev_max <- max(mean_elev_max[names(mean_elev_max) %in% rand.com])
# min not really different but max very different
min(dat1$mean_elev)
max(dat1$mean_elev)

# create new prediction grid for specific communes with varying pop_den
env.m2_newdat_com_ran <- data.frame(Provcomm = rep(rand.com, each=100),
                                Province = rep(rand.prov, each=100),
                                mean_elev = seq(from=mean_elev_min, to=mean_elev_max, length.out = 100),
                                year = mean(dat1$year))


# add commune-specific areaKM offset                         
env.m2_newdat_com_ran$areaKM <-  dat1$areaKM[match(env.m2_newdat_com_ran$Provcomm, dat1$Provcomm)]


# attach commune-specific predictions
env.m2_newdat_com_ran$pred.com <- as.vector(predict(env.m2, type="response", newdata=env.m2_newdat_com_ran, 
                                                re.form=~(year|Province/Provcomm)))


# attach global predictions
env.m2_newdat_com_ran$pred.glo <- rep(env_m2_newdata$pred, times=12)

# set levels
env.m2_newdat_com_ran$Provcomm <- as.factor(env.m2_newdat_com_ran$Provcomm)
provcomm_lvls <- levels(env.m2_newdat_com_ran$Provcomm) 
par(mfrow = c(3,4))

# set scales
ylo <- min(env.m2_newdat_com_ran$pred.com)*0.9
yhi <- max(env.m2_newdat_com_ran$pred.com)*1.1
xlo <- mean_elev_min
xhi <- mean_elev_max

# Iterate through the communes (levels in m6_newdat_com$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- env.m2_newdat_com_ran[env.m2_newdat_com_ran$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  # if(i == 1) {
  # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$mean_elev,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Mean elevation (scaled & standardised",
       ylab = "Predicted forest cover (forest pixel count)",
       main = unique(preddat_i$Provcomm))
  # } else {
  lines(preddat_i$mean_elev,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$mean_elev,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$mean_elev, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

### after running the above random sampling code a few times, it looks as though the global model is good at predicting at low forest cover and low elevation (as with the popdem model), as this is where most of the communes sit on the spectrum.  It performs ok for communes with higher elevation provided they don't have too much forest!  As soon as the commune has lots of forest or very high elevation, the global model performs badly.  


#
  ## Human additional variables ####
    # hum.m1 ####

# there are 5 variables in this set - distance to border (dist_border), distance to the provincial captial (dist_provCap), presence of economic land concession (elc), presence of any protected area (PA), and the protecte area category (PA_cat, includes "none")

# attempt saturated model
hum.m1 <- glmer(ForPix ~ dist_border+dist_provCap+elc+PA+PA_cat+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m1)
# interestingly, PA doesn't appear to look important.  This surprises me as you would assume that PAs would be placed in areas with high forest cover. elc doesn't appear to be important, which is also surprising because I could imagine two scenarios 1) it would have a postive relationship ebcause elcs were often placed in forested areas, and 2) a negative relationship because the elc's would have cleared a lot of forest in the areas they were placed.  However, during this time period, perhaps not much forest clearing had happened yet. dist_border and dst_provCap appear to be important, with both having apositive effect. dist_provCap I can understand - communes further away from urban centres are likely to be more forested. Not sure yet about dist_border. I think all vars require further investigation 

# quick plots
plot_model(hum.m1, type="pred", terms="dist_border")
plot_model(hum.m1, type="pred", terms="dist_provCap")
plot_model(hum.m1, type="pred", terms="elc")
plot_model(hum.m1, type="pred", terms="PA")
plot_model(hum.m1, type="pred", terms="PA_cat") # surprised RMS level is not sig given the coefficient


#
    # model selection ####

# remove PA_cat
hum.m2 <- glmer(ForPix ~ dist_border+dist_provCap+elc+PA+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m2)

# remove elc but keep PA_cat in
hum.m3 <- glmer(ForPix ~ dist_border+dist_provCap+PA_cat+PA+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m3)
# PA_cat is causing the rank deficient matrix warning

# remove elc and PA_cat
hum.m4 <- glmer(ForPix ~ dist_border+dist_provCap+PA+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m4)

# remove PA
hum.m5 <- glmer(ForPix ~ dist_border+dist_provCap+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m5)

# remove dist_border
hum.m6 <- glmer(ForPix ~ dist_provCap+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m6)


# compare with LRT
anova(hum.m1,hum.m2,hum.m3,hum.m4,hum.m5,hum.m6)

# compare with AIC



#
### simple test ####

# becasue there is so little forest cover change over time, we want a simple test to look at the relationship between whether forest cover has changed at all over the years, and the mean of each predictor

testdat <- dat1 %>% select(year, Province, Commune, diffPix)
head(testdat)

# create new unique commmune name
testdat$new.commune <- paste(testdat$Province,testdat$Commune, sep="_")
head(testdat$new.commune)

# create new column which sums the differnece in forest pixels over the years for each commune
testdat1 <- testdat %>% group_by(new.commune) %>%  
            arrange(year, .by_group=TRUE)  %>% 
            summarise(sum = sum(diffPix))

hist(testdat1$sum)

# create new column "bin" (binary) - if the sum of the difference in pixels is 0 (i.e. no change) then it is 0, if the sum is greater than 0 (increase in pixels) then it is 1, if the sum of forest pixels is less than 0 (decrease in forest pixels) then it is -1
testdat1$bin <- case_when(testdat1$sum == 0 ~  0, 
                          testdat1$sum  > 0 ~  1,
                          testdat1$sum  < 0 ~ -1)
hist(testdat1$bin)


# pull out predictor variables
testvarsdat <- dat1 %>% select(!c(commGIS,areaKM,ForPix,diffPix,habitat,elc,PA,PA_cat,year_fac))

# create new commune variable
testvarsdat$new.commune <- paste(testvarsdat$Province,testvarsdat$Commune, sep="_")

# Now create a new variable which is the mean of each variable in that commune across the years
testvarsdat1 <- testvarsdat %>% group_by(new.commune) %>% 
                summarise_at(c("tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec",
                               "propSecSec","Les1_R_Land","pig_fam","dist_sch","garbage",
                               "KM_Comm","land_confl","crim_case","Pax_migt_in","Pax_migt_out"
                               ,"mean_elev","dist_border","dist_provCap"), mean)

testdat_df <- cbind(testdat1,testvarsdat1)
testdat_df <- testdat_df[,-4]
head(testdat_df)
str(testdat_df)

testdat_df$bin <- as.character(testdat1$bin)

plot_tot_pop <- ggplot(testdat_df, aes(x=bin,  y=tot_pop))+
                geom_boxplot()+
                xlab("Forest cover lost / no change / gained")+
                ylab("total population")

plot_prop_ind <- ggplot(testdat_df, aes(x=bin,  y=prop_ind))+
                 geom_boxplot()+
                 xlab("Forest cover lost / no change / gained")+
                 ylab("proportion indigneous")

plot_pop_den <- ggplot(testdat_df, aes(x=bin,  y=pop_den))+
                 geom_boxplot()+
                 xlab("Forest cover lost / no change / gained")+
                 ylab("Population density")

plot_M6_24_sch <- ggplot(testdat_df, aes(x=bin,  y=M6_24_sch))+
                 geom_boxplot()+
                 xlab("Forest cover lost / no change / gained")+
                 ylab("Proportion males in school")

plot_propPrimSec <- ggplot(testdat_df, aes(x=bin,  y=propPrimSec))+
                  geom_boxplot()+
                  xlab("Forest cover lost / no change / gained")+
                  ylab("Proportion employed in primary sector")

plot_Les1_R_Land <- ggplot(testdat_df, aes(x=bin,  y=Les1_R_Land))+
                    geom_boxplot()+
                    xlab("Forest cover lost / no change / gained")+
                    ylab("Proportion population with no agricultural land")

library(patchwork)
plot_tot_pop+plot_prop_ind+plot_pop_den+plot_M6_24_sch+plot_propPrimSec+plot_Les1_R_Land


### to remember ####



