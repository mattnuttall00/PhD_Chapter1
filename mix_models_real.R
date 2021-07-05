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

# load data (centred)
dat1 <- read.csv("Data/commune/dat1.csv", header = TRUE, stringsAsFactors = TRUE)
dat1$elc <- as.factor(dat1$elc)
dat1$PA <- as.factor(dat1$PA)



# load raw data (uncentered)
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

### NOTE there will be 2 sections within each model set - one for the analysis which only considered communes with forest, and one with ALL communes.


# I am going to follow the below approach for each set

# 1) Run maximal model (with no interactions) to get a feel for the effects of variables without interactions
# 2) assess each variables effect via plotting 
# 3) run maximal model with interactions (if I think that interactions are plausible and if I have an a priori hypothesis about the interactions) and compare with the first model (LRT, profiled confidence intervals &       parametric bootstrapping)
# 4) If there is only 1 variable in the set then that variable will be carried forward and I will move on to the next set
# 5) conduct model simplification and selection until the best model is selected
# 6) after all sets have been run and I have a final set of variables, I will try to run a maximal model with all final variables. If this doesn't work I will construct several smaller models (with environmental and other human vars as controls) which represent hypotheses.
# 7) model simplification of the above model(s)
# 8) Plot effects from selected model(s):
  # a) manual predict from observed data and compare to predict()
  # b) predict main effects, from an "average" commune
  # c) add on predictions from communes with highest intercepts
  # d) plot the "mean effect" from each Province (using function)
  # e) plot the "mean effect" from each set of communes (PA / no PA) using function
  # f) select the province with RE intercept closest to 0 and plot all the communes (or subset) in a panel grid        with the global effect plus the effect for that commune
# 9) run simple binomial glm (as per Jeroen's suggestion) to check that the results are vaguely plausible 

  ### Random effects structure ####

# according to Zuur et al 2009 and Barr et al 2015a, a good approach for establishing your random effects structure is to include all fixed effects (maximal / above optimal model) and then test different random effects structures (using REML)

# Commune - this wants to be a random effect because of repreat measurements (year), because there are a LOT of levels and so would eat up a lot of degrees of freedom as a fixed effect. .

# Province - this is a random effect, and commune should be nested inside Province (ie a commune can only feature in one Province). Having Province in teh RE structure adds complexity to the interpretation, but there does appear to be some variance associated with province. And although I am interested in the differences between Provinces, perhaps I am not intersted in the effect of province per se, but more the way in which other things vary by province (see Jeroen's e-mail - he explains it well).

# Year. This is tricky.  See e-mail conversation with Jeroen and his script "basic_analysis_fromJeroen". At the moment I am moving forward with year as a random slope, but not a fixed effect.  


  ## Population demographics ####

# total population (tot_pop), population density (pop_den), proportion indigneous (prop_ind)


    ## FORESTED COMMUNES ONLY ####
      # popdem.m1 (tot_pop, pop_den, prop_ind - no interactions) ####


# model with all pop dem vars as fixed effects. Offset as areaKM to account for differences in size of communes
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





        # model comparison  ####

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

# variance component analysis
print(VarCorr(popdem.m6),comp="Variance") 
vars <- data.frame(term = c("Commune","year/com", "Province", "year/Prov"),
                   variance = c(1.51329170,0.00459509,1.67420722,0.00067864))
vars$relative.contrib <- vars$variance/sum(vars$variance)

# marginal and conditional r2
r.squaredGLMM(popdem.m6)
# marginal r2 (fixed effects) is extremely low 0.007, and the conditional (fixed + random) is 0.99.  This basically means that the random effects are explaing essentially all the variance.


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
prov.dat <- dat %>% filter(Province %in% provs) 
prov.dat$type <- "problem"
# create subset of all data NOT including the above
prov.excl <- dat %>% filter(!Province %in% provs)
prov.excl$type <- "other"
prov.all <- rbind(prov.dat,prov.excl)

# compare ForPix between the above selection and the data overall, and the rest of the data (excluding selection)
par(mfrow=c(2,2))
hist(prov.dat$ForPix, main="prov subset")
hist(dat$ForPix, main="All data")
hist(prov.excl$ForPix, main="exclude provs")

# compare scatter plot of ForPix across communes, split by the provinces
ggplot(prov.all, aes(Commune,y=ForPix, colour=type))+
  geom_point()+
  theme(element_blank())
# I would say that the provinces that are causing the issues tend to have higher ForPix than the others. It also look potentially like the provinces causing issues are almost always losing forest cover over time (lots of vertical lines of dots)

# scatter plot of ForPix against pop_den, split by provinces
ggplot(prov.all, aes(x=pop_den, y=ForPix, colour=type))+
  geom_point()+
  ylim(0,10000)+
  xlim(0,1000)+
    theme(element_blank())
# This plot makes it also looks like the problem provinces tend to have communes with higher ForPix. There is a chunk of red with no green where pop_den has increased to about 100 but ForPix has not decreased (whereas all the green dots are lower, i.e. in the other provinces/communes at that population density forest cover is lower). 

# boxplot of ForPix and province, coloured by the two groups of provinces 
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)+
  theme(element_blank())
# this plot suggests that the problem provinces don't necessarily have more forest cover, but they do tend to have more outliers that are very high forest cover.  

# boxplot of pop_den and province, coloured by the two groups of provinces
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=pop_den, colour="Other provinces"))+
  theme(element_blank())
# This plot suggests that overall, the problem provinces tend to have lower median pop_den value compared to the other provinces, but they again tend to have more outliers

# boxplot of Commune and pop_den, coloured by the two groups of provinces
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=pop_den, colour="Other provinces"))+
  ylim(0,1000)+
  theme(element_blank())
# again this looks like there are more communes with higher pop_den values in the non-problematic provinces


# boxplot of Commune and ForPix, coloured by the two groups of provinces
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)+
  theme(element_blank())
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

# plot with free y axis
popdem.m6.p1 <- ggplot(m6.newdat, aes(x=pop_den, y=pred))+
  geom_line()+
  theme(element_blank())

# plot
popdem.m6.p2 <- ggplot(m6.newdat, aes(x=pop_den, y=pred))+
  geom_line()+
  ylim(0,10000)+
  theme(element_blank())

popdem.m6.p1 + popdem.m6.p2


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
par(mfrow = c(1,1))

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
 if(i == 1) {
    # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
    plot(preddat_i$pop_den,preddat_i$pred.com, 
         type = "l", 
         col = com_colours[i], 
         ylim = c(ylo,yhi), xlim = c(xlo,xhi),
         xlab = "Population density (scaled & standardised",
         ylab = "Predicted forest cover (forest pixel count)")
 } else {
    lines(preddat_i$pop_den,preddat_i$pred.com, col = com_colours[i])
    lines(preddat_i$pop_den,preddat_i$pred.glo, lty=2)
  }
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$pop_den, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

## check whether communes with more forest are more likely to lose forest
hist(dat1$ForPix)
summary(dat1$ForPix) # 3rd quantile is > 982

# separate dat1 into the quarter with the most forest and then the rest
com.for <- dat1[dat1$ForPix>982,]
com.for1 <- dat1[!(dat1$Provcomm %in% com.for$Provcomm),]

# summarise forest loss
com.for <- com.for %>% group_by(Provcomm) %>% 
              summarise(diffPix_sum = sum(diffPix))
com.for$type <- "high forest"
com.for1 <- com.for1 %>% group_by(Provcomm) %>% 
              summarise(diffPix_sum = sum(diffPix))
com.for1$type <- "low forest"
com.for.all <- rbind(com.for,com.for1)

ggplot(com.for.all, aes(x=type, y=diffPix_sum, colour=type))+
  geom_boxplot()+
  xlab("Commune")+
  ylab("Forest pixels lost over study period")+
  theme(element_blank())

### after running the above random plotting a few times, the trend seems to hold - i.e. the global model predics well for communes with low forest cover and/or high population density, but does not perform well for communes with high forest cover

### I am not convinced that having both pop_den and tot_pop in the model is sensible (especially as an interaction). I have gone back and forth because I do think they tell us different things about a commune, and between communes they are not necessarily collinear. However within communes over time they are - i.e. in a given commune, over time, in order for pop_den to increase tot_pop must increase. Therefore as Jeroen said, it is a bit dangerous to have both in the same model as an interaction.  I also have a LOT of potential predictors, and so I need to be selective in which ones I take forward, and pop_den is the more important of the two.

### pop_den taken forward


#
    ## ALL COMMUNES ####
      # popdem.m1 & popdem.m2 ####


# model with pop dem vars as fixed effects. I have not included tot_pop as it is instrincally linked to pop_den, and previous analyses above suggest that pop_den is way more important. Offset as areaKM to account for differences in size of communes
popdem.m1 <- glmer(ForPix ~ pop_den + prop_ind + offset(log(areaKM)) + (year|Province/Provcomm),
                   data=dat1, family="poisson")

summary(popdem.m1)
# prop_ind has very small effect and non-sig approximate p value
ranef(popdem.m1)
fixef(popdem.m1)



# Likelihood ratio test

# test just the terms in popdem.m1
drop1(popdem.m1, test="Chisq")
# not entirely clear about the output here


# drop prop_ind
popdem.m2 <- glmer(ForPix ~ pop_den + offset(log(areaKM)) + (year|Province/Provcomm),
                   data=dat1, family="poisson")

summary(popdem.m2)

# anova
anova(popdem.m2, popdem.m1, test="Chisq")
# simpler model is better

        # variance component analysis ####


print(VarCorr(popdem.m2),comp="Variance") 
vars <- data.frame(term = c("Commune","year/com", "Province", "year/Prov"),
                   variance = c(11.5,0.0047,11.25,0.00048))
vars$relative.contrib <- vars$variance/sum(vars$variance)
# Interestingly quite large changes between m1 and m2.  For m1 Province is contributing the most variance proportionately (75%), followed by Commune (24.9%). The two years then make up the negligable rest. Whereas for m2 Commune and Province make up roughly 50% each, with Commune slightly higher at 50.3 and Province at 49.4. Year makes up the neglible rest. 

# marginal and conditional r2
r.squaredGLMM(popdem.m2)
# For m1 marginal r2 (fixed effects) is relatively high 0.65, and the conditional (fixed + random) is 1.  This means that the fixed effects are actually accounting for most of the variance. For m2 the marginal r2 (fixed effects) is even higher at 0.78.  I think this is further evidence that prop_ind wasn't doing very much. 


        # manual predictions (observed data) ####

## m1

# create prediction dataframe using real data
m1_est <- subset(dat1, select = c("ForPix","pop_den","prop_ind","Province","Commune", "Provcomm", "year"))
m1_est$Iglobal <- fixef(popdem.m1)[["(Intercept)"]]
m1_est$Iprovince <- ranef(popdem.m1)$Province[,"(Intercept)"][match(m1_est$Province, row.names(ranef(popdem.m1)$Province))]
m1_est$CommProv <- paste(m1_est$Provcomm,m1_est$Province,sep=":")
m1_est$Icommune <- ranef(popdem.m1)[[1]][,"(Intercept)"][match(m1_est$CommProv, row.names(ranef(popdem.m1)[[1]]))]
m1_est$b_year_province <- ranef(popdem.m1)$Province[,"year"][match(m1_est$Province, row.names(ranef(popdem.m1)$Province))]
m1_est$b_year_commune <- ranef(popdem.m1)[[1]][,"year"][match(m1_est$CommProv, row.names(ranef(popdem.m1)[[1]]))]
m1_est$b_pop_den <- fixef(popdem.m1)[["pop_den"]]
m1_est$b_prop_ind <- fixef(popdem.m1)[["prop_ind"]]
m1_est$offset <- log(dat1$areaKM)[match(m1_est$Provcomm, dat1$Provcomm)]


# Manually predict (using actual data)
mpred_m1 <- with(m1_est, {
  Iglobal+                            
    pop_den*b_pop_den +
    prop_ind*b_prop_ind +
    Iprovince +                       
    Icommune +                         
    year*(b_year_province+b_year_commune)+
    offset
})

# using predict and real data
pred_m1 <- predict(popdem.m1, type="response")

# plot together to check
plot(exp(mpred_m1), pred_m1)
abline(a = 0, b = 1, col = "red")




## m2

# create prediction dataframe using real data
m2_est <- subset(dat1, select = c("ForPix","pop_den","Province","Commune", "Provcomm", "year"))
m2_est$Iglobal <- fixef(popdem.m2)[["(Intercept)"]]
m2_est$Iprovince <- ranef(popdem.m2)$Province[,"(Intercept)"][match(m2_est$Province, row.names(ranef(popdem.m2)$Province))]
m2_est$CommProv <- paste(m2_est$Provcomm,m2_est$Province,sep=":")
m2_est$Icommune <- ranef(popdem.m2)[[1]][,"(Intercept)"][match(m2_est$CommProv, row.names(ranef(popdem.m2)[[1]]))]
m2_est$b_year_province <- ranef(popdem.m2)$Province[,"year"][match(m2_est$Province, row.names(ranef(popdem.m2)$Province))]
m2_est$b_year_commune <- ranef(popdem.m2)[[1]][,"year"][match(m2_est$CommProv, row.names(ranef(popdem.m2)[[1]]))]
m2_est$b_pop_den <- fixef(popdem.m2)[["pop_den"]]
m2_est$offset <- log(dat1$areaKM)[match(m2_est$Provcomm, dat1$Provcomm)]


# Manually predict (using actual data)
mpred_m2 <- with(m2_est, {
  Iglobal+                            
    pop_den*b_pop_den +
    Iprovince +                       
    Icommune +                         
    year*(b_year_province+b_year_commune)+
    offset
})

# using predict and real data
pred_m2 <- predict(popdem.m2, type="response")

# plot together to check
plot(exp(mpred_m2), pred_m2)
abline(a = 0, b = 1, col = "red")


        # predict main effects ####
          # m1 ####

# m2 is the prefered model

### population density
m1.popden.newdat <- data.frame(pop_den = seq(min(dat1$pop_den),max(dat1$pop_den), length.out = 100),
                               prop_ind = mean(dat1$prop_ind),
                               areaKM = mean(dat1$areaKM))
m1.popden.newdat$pred <- as.vector(predict(popdem.m1, newdata=m1.popden.newdat, type="response", re.form=NA))

# plot 
ggplot(m1.popden.newdat, aes(x=pop_den, y=pred))+
  geom_line()+
  theme(element_blank())
# pretty narli effect - predicted forest cover decreases almost vertically at very low values of population density, and then is essentially 0 beyond that. This suggests that the vast majority of communes with any decent level of pop_den have no forest whatsoever.


# What if we take a closer look at predictions for communes at the lower end of the pop_den scale
m1.popden.newdat2 <- data.frame(pop_den = seq(min(dat1$pop_den),0, length.out = 100),
                               prop_ind = mean(dat1$prop_ind),
                               areaKM = mean(dat1$areaKM))
m1.popden.newdat2$pred <- as.vector(predict(popdem.m1, newdata=m1.popden.newdat2, type="response", re.form=NA))

ggplot(m1.popden.newdat2, aes(x=pop_den, y=pred))+
  geom_line()+
  theme(element_blank())




### proportion indigneous 
m1.propind.newdat <- data.frame(prop_ind = seq(min(dat1$prop_ind),max(dat1$prop_ind), length.out = 100),
                               pop_den = mean(dat1$pop_den),
                               areaKM = mean(dat1$areaKM))
m1.propind.newdat$pred <- as.vector(predict(popdem.m1, newdata=m1.propind.newdat, type="response", re.form=NA))



ggplot(m1.propind.newdat, aes(x=prop_ind, y=pred))+
  geom_line()+
  theme(element_blank())+
  ylim(0,1000)


#
          # m2 ####

m2.popden.newdat <- data.frame(pop_den = seq(min(dat1$pop_den),max(dat1$pop_den), length.out = 100),
                               areaKM = mean(dat1$areaKM))
m2.popden.newdat$pred <- as.vector(predict(popdem.m2, newdata=m2.popden.newdat, type="response", re.form=NA))

# plot 
ggplot(m2.popden.newdat, aes(x=pop_den, y=pred))+
  geom_line()+
  theme(element_blank())+
  ylim(0,20000)
# The problem with displaying this is that places like Phnom Penh that have very large pop_den values are dragging the x axis out.


# if you look at a histogram of pop_den there are only a couple of very high values (>10). I will try and remove these and see what it does to the curve
m2.popden.newdat <- data.frame(pop_den = seq(min(dat1$pop_den),5, length.out = 100),
                               areaKM = mean(dat1$areaKM))
m2.popden.newdat$pred <- as.vector(predict(popdem.m2, newdata=m2.popden.newdat, type="response", re.form=NA))

# plot 
popden.m2.glob <- ggplot(m2.popden.newdat, aes(x=pop_den, y=pred))+
                  geom_line(size=1)+
                  ylim(0,20000)+
                  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                  ylab("Predicted number of forest pixels")+
                  xlab("Population density")+
                  theme(axis.title = element_text(size=20))+
                  theme(axis.text = element_text(size=17))
# doesn't make much difference really - just shortens the x axis but the result is the same. When you give the plot a realistic y axis the line is flat

ggsave("Results/Socioeconomics/Plots/population_density/popdenm2_global_effect.png", popden.m2.glob,
       height = 20, width = 30, units = "cm", dpi = 300)


        # predict effects for specific locations ####

# as we have discovered, the global effects are quite misleading, as there is so much between commune variation. So now I want to explore the differences in effects for different provinces and communes


          # pop_den effects between provinces ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### The below produces 95% variation error bars

# this function spits out a dataframe with a range of pop_den values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.popden <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(pop_den = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pop_den = seq(min(dat$pop_den[dat$Province==province]),
                                       max(dat$pop_den[dat$Province==province]), length.out = 100), # range in province
                         prop_ind = mean(dat$prop_ind[dat$Province==province]), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("pop_den","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
mean.df <- compred %>% group_by(pop_den) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(pop_den = compred_wide$pop_den,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="pop_den")

return(mean.df)
  
}
 
test <- ProvMean.popden(dat1,"Stung Treng",popdem.m2)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.popden(province=provs[i], model=popdem.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
popden_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)


ggplot(popden_allprovs, aes(x=pop_den, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# phnom penh is messing with the axes ranges

# remove PP and no free axis
ggplot(popden_allprovs[popden_allprovs$Province!="Phnom Penh",], aes(x=pop_den, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)+
  ylim(0,15000)
# This plot shows that there is huge variation in pop_den between provinces, but also that despite this, there are still visible differences in the effect of pop_den on forest cover. The provinces that stand out as having larger relative effects are Otdar Meanchey, Preah Vihear, Stung Treng, Kratie, Pursat, Koh Kong. These are more remote provinces, with higher forest cover values, and more PAs. 

# remove PP and free axis
ggplot(popden_allprovs[popden_allprovs$Province!="Phnom Penh",], aes(x=pop_den, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# This plot shows the same as above but with free axes. It means you can see more of what is going on at the individual province level. This shows that in some provinces, even though they don't have much forest cover, pop_den has some effect.  This is particularly obvious in Battambang, Kampot, Preah Sihanouk.  Interestingly this shows that Mondulkiri and Ratanakiri (the only two very forested provinces that are not obvious in the above plot) do not have much of an effect. But these two provinces have such low pop_den values, that no effect can be detected. 




### The below is the same as above but instead of 95% error bars it outputs a prediction line for each commune. I will plot the mean as a thick line and the others as thin faded lines. It might show the within-province variation better

ProvMeanLine.popden <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(pop_den = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pop_den = seq(min(dat$pop_den[dat$Province==province]),
                                       max(dat$pop_den[dat$Province==province]), length.out = 100), # range in province
                         prop_ind = mean(dat$prop_ind[dat$Province==province]), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("pop_den","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
    mean.df <- compred %>% group_by(pop_den) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
  }

test <- ProvMeanLine.popden(dat1,"Stung Treng", popdem.m2)  


## now use the function to get the mean effects and all other predictions for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.popden(province=provs[i], model=popdem.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
popden_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

### plot

# no PP, fixed axis
ggplot(popden_allprovs[popden_allprovs$province!="Phnom Penh",], aes(x=pop_den, y=pred, group=commune))+
  geom_line()+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~province, nrow=6)+
  ylim(0,15000)


# add new grouping level to allow line types to be set. This was for plotting the communes predictions and mean predictions from the same dataframe. But I can't for the life of me get ggplot to plot the lines in the correct order so that the mean line is on top of the commune lines. Therefore I have had to use the approach immediately below where I plot using two separate dataframes
popden_allprovs$grp <- ifelse(popden_allprovs$commune=="mean", "mean", "commune")
popden_allprovs$grp <- as.factor(popden_allprovs$grp)
popden_allprovs$grp <- factor(popden_allprovs$grp, levels=c("commune","mean"))

# extract all mean predictions (plus province) into new dataframe
means.df <- popden_allprovs %>% filter(commune=="mean") %>% select(pop_den,pred,commune,province)


# no PP, free axis
ggplot(popden_allprovs[popden_allprovs$province!="Phnom Penh",], aes(x=pop_den, y=pred, group=commune))+
  geom_line(aes(color=grp, size=grp))+
  scale_color_manual(values=c('#000000','#FF0000'), breaks=c("commune","mean"))+
  scale_size_manual(values=c(0.5,2))+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~province, nrow=6, scales = "free")+
  ylim(0,26000)

# no PP, free axis, separate dataframes
popden.m2.provs <- ggplot(NULL, aes(x=pop_den, y=pred))+
                    geom_line(data=popden_allprovs[popden_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=means.df, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Population density (Centerd and scaled)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/population_density/popden.m2.provs.png",popden.m2.provs,
       height=20, width=30, units="cm", dpi=300)
  

# plot single provinces
ggplot(popden_allprovs[popden_allprovs$province=="Koh Kong",], 
       aes(x=pop_den, y=pred, group=commune))+
  geom_line(aes(color=grp, size=grp))+
  scale_color_manual(values=c('#999999','#000000'), breaks=c("commune","mean"))+
  scale_size_manual(values=c(0.2,1.5))+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  ylim(0,15000)







          # pop_den effects between Pa/noPA ####

## now I want to have a look at differences between groups of communes. Specifically I want to look at communes in and around PAs. This is to see if there is a difference in effect of pop_den on the more rural/remote areas with high forest cover and PAs. 

# I need to do the same as above - split the communes into groups, predict for each commune within a group, and then get the mean, using the quantiles to show variation.  I can adapt my function from above

# I can use the PA variable in dat1 to split the groups


### This function outputs the mean predictions for each group, and the 95% quantiles.  Further below is an edited function that will output all of the individual commune predictions
PAmean.popden <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(pop_den = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pop_den = seq(min(dat$pop_den[dat$PA==pa]),
                                       max(dat$pop_den[dat$PA==pa]), length.out = 100), # range in province
                         prop_ind = mean(dat$prop_ind[dat$PA==pa]), # range in group
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(popdem.m2, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("pop_den","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
  mean.df <- compred %>% group_by(pop_den) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(pop_den = compred_wide$pop_den,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="pop_den")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.popden(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.popden(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
popden.m2.PA.plot <- ggplot(pa_all, aes(x=pop_den, y=pred, group=PA, colour=PA, fill=PA))+
                      geom_line(size=1.5)+
                      scale_color_manual(values=c('gray1','gray1'))+
                      geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5, fill=PA), alpha=0.5, color=NA)+
                      scale_fill_manual(values=c('blue1','green4'))+
                      theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"),
                            axis.title = element_text(size=20),
                            axis.text = element_text(size=17),
                            legend.key.size = unit(1, 'cm'),
                            legend.title = element_text(size=17),
                            legend.text = element_text(size=17))+
                      xlim(-0.15,0.2)+
                      xlab("Population density (centered and scaled)")+
                      ylab("Predicted number of forest pixels")
  
ggsave("Results/Socioeconomics/Plots/population_density/popden.m2.PA.png", popden.m2.PA.plot,
       height=20, width=30, units="cm", dpi=300)



# facet wrap and free axis
ggplot(pa_all, aes(x=pop_den, y=pred, group=PA))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~PA, nrow=1, scales = "free")



### The below function outputs all of the individual communes predictions instead of the 95% quantiles
PAmeanLines.popden <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(pop_den = NULL,
                        pred = NULL,
                        Provcomm = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pop_den = seq(min(dat$pop_den[dat$PA==pa]),
                                       max(dat$pop_den[dat$PA==pa]), length.out = 100), # range in group
                         prop_ind = mean(dat$prop_ind[dat$PA==pa]), # range in group
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(popdem.m2, type="response",newdata=newdat, 
                                     re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("pop_den","pred","Provcomm")]
    #split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    #comname <- split[1,2]
    #df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
  mean.df <- compred %>% group_by(pop_den) %>% summarise_at(vars(pred),mean) %>% 
    mutate(Provcomm = "mean") %>% mutate(PA = pa)
  
  # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
  
}


# run function for communes with PAs
pa_mean <- PAmeanLines.popden(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmeanLines.popden(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# extract mean predictions
pa_means <- pa_all %>% filter(Provcomm=="mean") %>% select(pop_den,pred,Provcomm,PA) 

popden.m2.PA.lines <- ggplot(NULL, aes(x=pop_den, y=pred))+
                      geom_line(data=pa_all, aes(group=Provcomm), col="grey", size=0.5)+
                      geom_line(data=pa_means, col="black", size=1)+
                      theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                      facet_wrap(~PA)+
                      ylim(0,26000)+
                      xlim(-0.24,0.25)+
                      xlab("Population density (Centerd and scaled)")+
                                        ylab("Predicted number of forest pixels")+
                                        theme(axis.title = element_text(size=20))+
                                        theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/population_density/popden.m2.PA.lines.png", popden.m2.PA.lines,
       height=20, width=30, units="cm", dpi=300)



#
          # prop_ind effects between provinces ####





    ## Education ####

# just one var - M6_24_sch

      ## FORESTED COMMUNES ONLY ####
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
        # Predict for sets of communes ####

# randomly sample communes
rand.com <- sample(dat1$Provcomm, 12, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17,19,21,23)]

# define the range of pop_den to predict for, first. Min/max per commune:
M6_24_sch_min <- tapply(dat1$M6_24_sch, dat1$Provcomm, min)
M6_24_sch_max <- tapply(dat1$M6_24_sch, dat1$Provcomm, max)
# Min/max within your selection of communes:
M6_24_sch_min <- min(M6_24_sch_min[names(M6_24_sch_min) %in% rand.com])
M6_24_sch_max <- max(M6_24_sch_max[names(M6_24_sch_max) %in% rand.com])


# create new prediction grid for specific communes with varying pop_den
edu.m1_newdat_com_ran <- data.frame(Provcomm = rep(rand.com, each=100),
                                Province = rep(rand.prov, each=100),
                                M6_24_sch = seq(from=M6_24_sch_min, to=M6_24_sch_max, length.out = 100),
                                year = mean(dat1$year))


# add commune-specific areaKM offset                         
edu.m1_newdat_com_ran$areaKM <-  dat1$areaKM[match(edu.m1_newdat_com_ran$Provcomm, dat1$Provcomm)]


# attach commune-specific predictions
edu.m1_newdat_com_ran$pred.com <- as.vector(predict(edu.m1, type="response", 
                                                    newdata=edu.m1_newdat_com_ran, 
                                                    re.form=~(year|Province/Provcomm)))

# global predictions
edu.m1_glo_pred <- data.frame(M6_24_sch = rep(min(dat1$M6_24_sch),max(dat1$M6_24_sch), length.out=100),
                              areaKM = mean(dat1$areaKM))
edu.m1_glo_pred$pred <- as.vector(predict(edu.m1, type="response", newdata=edu.m1_glo_pred,re.form=NA))

# attach global predictions
edu.m1_newdat_com_ran$pred.glo <- rep(edu.m1_glo_pred$pred, times=12)

# set levels
edu.m1_newdat_com_ran$Provcomm <- as.factor(edu.m1_newdat_com_ran$Provcomm)
provcomm_lvls <- levels(edu.m1_newdat_com_ran$Provcomm) 
par(mfrow = c(3,4))

# set scales
ylo <- min(edu.m1_newdat_com_ran$pred.com)*0.9
yhi <- max(edu.m1_newdat_com_ran$pred.com)*1.1
xlo <- M6_24_sch_min
xhi <- M6_24_sch_max

# Iterate through the communes 
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- edu.m1_newdat_com_ran[edu.m1_newdat_com_ran$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  #if(i == 1) {
    # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
    plot(preddat_i$M6_24_sch,preddat_i$pred.com, 
         type = "l", 
         col = com_colours[i], 
         ylim = c(ylo,yhi), xlim = c(xlo,xhi),
         xlab = "Proportion of males aged 6-24 in school (scaled & standardised",
         ylab = "Predicted forest cover (forest pixel count)")
  #} else {
    lines(preddat_i$M6_24_sch,preddat_i$pred.com, col = com_colours[i])
    lines(preddat_i$M6_24_sch,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$M6_24_sch, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}


      ## ALL COMMUNES ####
        # edu.m1 ####

# only one variable here
edu.m1 <- glmer(ForPix ~ M6_24_sch + offset(log(areaKM)) + (year|Province/Provcomm), 
                family="poisson", data=dat1)

summary(edu.m1)
# very tiny effect

# quick plot
plot_model(edu.m1, type="pred", terms="M6_24_sch")
# flat as you like


# predict and plot global effect
m2.edu.newdat <- data.frame(M6_24_sch = seq(min(dat1$M6_24_sch),max(dat1$M6_24_sch), length.out = 100),
                               areaKM = mean(dat1$areaKM))
m2.edu.newdat$pred <- as.vector(predict(edu.m1, newdata=m2.edu.newdat, type="response", re.form=NA))

# plot 
edu.m1.glob <- ggplot(m2.edu.newdat, aes(x=M6_24_sch, y=pred))+
              geom_line(size=1)+
              ylim(0,20000)+
              theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
              ylab("Predicted number of forest pixels")+
              xlab("Proportion of males (aged 6-24) in school)")+
              theme(axis.title = element_text(size=20))+
              theme(axis.text = element_text(size=17))

        # variance component analysis ####


print(VarCorr(edu.m1),comp="Variance") 
vars <- data.frame(term = c("Commune","year/com", "Province", "year/Prov"),
                   variance = c(12.5,0.0046,13.3,0.00051))
vars$relative.contrib <- vars$variance/sum(vars$variance)
# Commune and province contributing pretty much all the variance (year is tiny). Province is ~51% and commune ~48%

# marginal and conditional r2
r.squaredGLMM(edu.m1)
# Marginal effect (fixed effect) is miniscule - 1.65E-10. Education has not effect 



        # M6_24_sch effects between Provinces ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 



### this function spits out a dataframe with a range of M6_24_sch values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.edu <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(M6_24_sch = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(M6_24_sch = seq(min(dat$M6_24_sch[dat$Province==province]),
                                       max(dat$M6_24_sch[dat$Province==province]), length.out = 100), 
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("M6_24_sch","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
mean.df <- compred %>% group_by(M6_24_sch) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(M6_24_sch = compred_wide$M6_24_sch,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="M6_24_sch")

return(mean.df)
  
}
 
#test <- ProvMean.edu(dat1,"Stung Treng",edu.m1)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.edu(province=provs[i], model=edu.m1)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
edu_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)


ggplot(edu_allprovs, aes(x=M6_24_sch, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(edu_allprovs[edu_allprovs$Province!="Phnom Penh",], aes(x=M6_24_sch, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)+
  ylim(0,15000)
# This plot shows that there is no effect at the province level

# remove PP and free axis
ggplot(edu_allprovs[edu_allprovs$Province!="Phnom Penh",], aes(x=M6_24_sch, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Yep - no effect at all 




### this function spits out a dataframe with a range of M6_24_sch values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.edu <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(M6_24_sch = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(M6_24_sch = seq(min(dat$M6_24_sch[dat$Province==province]),
                                       max(dat$M6_24_sch[dat$Province==province]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of M6_24_sch and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("M6_24_sch","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of M6_24_sch)  
    mean.df <- compred %>% group_by(M6_24_sch) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.edu(dat1, "Stung Treng", edu.m1)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.edu(province=provs[i], model=edu.m1)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
edu_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
edu_means <- edu_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
edu.m1.provs <- ggplot(NULL, aes(x=M6_24_sch, y=pred))+
                    geom_line(data=edu_allprovs[edu_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=edu_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Number of males aged 16 to 24 in school (Centerd and scaled)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

# no effect at all, from any of the communes

#

        # M6_24_sch effects between PA and non-PA communes ####

## now I want to have a look at differences between groups of communes. Specifically I want to look at communes in and around PAs. This is to see if there is a difference in effect of M6_24_sch on the more rural/remote areas with high forest cover and PAs. 

# I need to do the same as above - split the communes into groups, predict for each commune within a group, and then get the mean, using the quantiles to show variation.  I can adapt my function from above

# I can use the PA variable in dat1 to split the groups


PAmean.edu <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(M6_24_sch = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(M6_24_sch = seq(min(dat$M6_24_sch[dat$PA==pa]),
                                       max(dat$M6_24_sch[dat$PA==pa]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(edu.m1, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of M6_24_sch and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("M6_24_sch","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
  mean.df <- compred %>% group_by(M6_24_sch) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(M6_24_sch = compred_wide$M6_24_sch,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="M6_24_sch")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.edu(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.edu(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
ggplot(pa_all, aes(x=M6_24_sch, y=pred, group=PA, colour=PA, fill=PA))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.3, colour=NA)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))
  #xlim(-0.15,0.2)
  #ylim(0,7500)
# no effect at all for either group




### I am not going to bother to run the predictions and pull out the individual communes as I did for pop_den as there is clearly nothing going on.


  ## Employment ####
    ## FORESTED COMMUNES ONLY ####
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
    ## ALL COMMUNES ####
      # emp.m1 & emp.m2 ####

# I am not going to test an interaction because these two variables are likely to be related, i.e. when there are fewer people in the primary sector there are likely to be more people in the secondary sector, and vice versa
emp.m1 <- glmer(ForPix ~ propPrimSec + propSecSec + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(emp.m1)
# tiny effect sizes and large approx p values

# quick plot
plot_model(emp.m1, type="pred")
# flat

# try removing the smallest effect (propSecSec)
emp.m2 <- glmer(ForPix ~ propPrimSec + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(emp.m2)
# tiny increase in effect size

# quick plot
plot_model(emp.m2, type="pred")
# still flat


anova(emp.m1, emp.m2)
# Simpler model better. I will take propPrimSec forward, even though it is unlikely to have an effect


# predict and plot global effect
m2.emp.newdat <- data.frame(propPrimSec = seq(min(dat1$propPrimSec),max(dat1$propPrimSec), length.out = 100),
                            areaKM = mean(dat1$areaKM))
m2.emp.newdat$pred <- as.vector(predict(emp.m2, newdata=m2.emp.newdat, type="response", re.form=NA))

# plot 
emp.m1.glob <- ggplot(m2.emp.newdat, aes(x=propPrimSec, y=pred))+
              geom_line(size=1)+
              ylim(0,20000)+
              theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
              ylab("Predicted number of forest pixels")+
              xlab("Proportion of adults employed in the primary sector")+
              theme(axis.title = element_text(size=20))+
              theme(axis.text = element_text(size=17))

      # propPrimSec effects between provinces ####


# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 



### this function spits out a dataframe with a range of propPrimSec values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.emp <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(propPrimSec = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(propPrimSec = seq(min(dat$propPrimSec[dat$Province==province]),
                                       max(dat$propPrimSec[dat$Province==province]), length.out = 100), 
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("propPrimSec","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
mean.df <- compred %>% group_by(propPrimSec) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(propPrimSec = compred_wide$propPrimSec,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="propPrimSec")

return(mean.df)
  
}
 
test <- ProvMean.emp(dat1,"Stung Treng",emp.m2)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.emp(province=provs[i], model=emp.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
emp_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and no free axis
ggplot(emp_allprovs, aes(x=propPrimSec, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(emp_allprovs[emp_allprovs$Province!="Phnom Penh",], aes(x=propPrimSec, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# This plot shows that there is no effect at the province level

# remove PP and free axis
ggplot(emp_allprovs[emp_allprovs$Province!="Phnom Penh",], aes(x=propPrimSec, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Yep - no effect at all 





### this function spits out a dataframe with a range of propPrimSec values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.emp <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(propPrimSec = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(propPrimSec = seq(min(dat$propPrimSec[dat$Province==province]),
                                       max(dat$propPrimSec[dat$Province==province]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of propPrimSec and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("propPrimSec","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of M6_24_sch)  
    mean.df <- compred %>% group_by(propPrimSec) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.emp(dat1, "Stung Treng", emp.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.emp(province=provs[i], model=emp.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
emp_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
emp_means <- emp_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
emp.m1.provs <- ggplot(NULL, aes(x=propPrimSec, y=pred))+
                    geom_line(data=emp_allprovs[emp_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=emp_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Proportion of population employed in the primary sector (centerd and scaled)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

# no effect at all in any province

      # propPrimSec effects between PA and non-PA communes ####

## now I want to have a look at differences between groups of communes. Specifically I want to look at communes in and around PAs. This is to see if there is a difference in effect of propPrimSec on the more rural/remote areas with high forest cover and PAs. 

# I need to do the same as above - split the communes into groups, predict for each commune within a group, and then get the mean, using the quantiles to show variation.  I can adapt my function from above

# I can use the PA variable in dat1 to split the groups


PAmean.emp <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(propPrimSec = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(propPrimSec = seq(min(dat$propPrimSec[dat$PA==pa]),
                                       max(dat$propPrimSec[dat$PA==pa]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(emp.m2, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of M6_24_sch and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("propPrimSec","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
  mean.df <- compred %>% group_by(propPrimSec) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(propPrimSec = compred_wide$propPrimSec,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="propPrimSec")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.emp(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.emp(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
ggplot(pa_all, aes(x=propPrimSec, y=pred, group=PA, colour=PA, fill=PA))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.3, colour=NA)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))
  #xlim(-0.15,0.2)
  #ylim(0,7500)
# no effect at all for either group



### I am not bothering to extract the individual commune prediction lines as I did for some of the above because there are no effects visible for any commune in the above section

  ## Economic security ####

# Les_1_R_Land & pig_fam

    ## FORESTED COMMUNES ####
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
    ## ALL COMMUNES ####
      # econ.m1 & econ.m2 ####

# model with both variables. I don't have any a priori hypothesis about an interaction, and so I will not test one
econ.m1 <- glmer(ForPix ~ Les1_R_Land + pig_fam + offset(log(areaKM)) + (year|Province/Provcomm),
                 family="poisson", data=dat1)

summary(econ.m1)
# Tiny effect sizes and large approx p values.

# quick plot
plot_model(econ.m1, type="pred")

# try removing the smallest effect (Les1_R_Land)
econ.m2 <- glmer(ForPix ~ pig_fam + offset(log(areaKM)) + (year|Province/Provcomm),
                 family="poisson", data=dat1)

summary(econ.m2)
# no change in effect size

anova(econ.m1, econ.m2)

# Simpler model is better. I will take pig_fam forward for the global model

AIC(econ.m1)
AIC(econ.m2)


# predict and plot global effect
m2.econ.newdat <- data.frame(pig_fam = seq(min(dat1$pig_fam),max(dat1$pig_fam), length.out = 100),
                            areaKM = mean(dat1$areaKM))
m2.econ.newdat$pred <- as.vector(predict(econ.m2, newdata=m2.econ.newdat, type="response", re.form=NA))

# plot 
econ.m1.glob <- ggplot(m2.econ.newdat, aes(x=pig_fam, y=pred))+
              geom_line(size=1)+
              ylim(0,20000)+
              theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
              ylab("Predicted number of forest pixels")+
              xlab("Proportion of families with pigs")+
              theme(axis.title = element_text(size=20))+
              theme(axis.text = element_text(size=17))


      # Les1_R_Land & pig_fam effects between provinces ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


          # Les1_R_Land - quantiles ####

# this function spits out a dataframe with a range of Les1_R_Land values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.econ <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(Les1_R_Land = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(Les1_R_Land = seq(min(dat$Les1_R_Land[dat$Province==province]),
                                       max(dat$Les1_R_Land[dat$Province==province]), length.out = 100), 
                         pig_fam = mean(dat$pig_fam[dat$Province==province]),
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of Les1_R_Land and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("Les1_R_Land","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
mean.df <- compred %>% group_by(Les1_R_Land) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(Les1_R_Land = compred_wide$Les1_R_Land,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="Les1_R_Land")

return(mean.df)
  
}
 
test <- ProvMean.econ(dat1,"Stung Treng",econ.m1)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.econ(province=provs[i], model=econ.m1)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
econ_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and free axis
ggplot(econ_allprovs, aes(x=Les1_R_Land, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(econ_allprovs[emp_allprovs$Province!="Phnom Penh",], aes(x=Les1_R_Land, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# This plot shows that there is no effect at the province level

# remove PP and free axis
ggplot(econ_allprovs[emp_allprovs$Province!="Phnom Penh",], aes(x=Les1_R_Land, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Yep - no effect at all 




          # Les1_R_Land - lines ####

### this function spits out a dataframe with a range of Les1_R_Land values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.econ1 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(Les1_R_Land = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(Les1_R_Land = seq(min(dat$Les1_R_Land[dat$Province==province]),
                                       max(dat$Les1_R_Land[dat$Province==province]), length.out = 100), # range in province
                         pig_fam = mean(dat$pig_fam[dat$Province==province]),
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of Les1_R_Land and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("Les1_R_Land","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of Les1_R_Land)  
    mean.df <- compred %>% group_by(Les1_R_Land) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.econ1(dat1, "Stung Treng", econ.m1)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.econ1(province=provs[i], model=econ.m1)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
econ1_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
econ1_means <- econ1_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
econ.m1.provs <- ggplot(NULL, aes(x=Les1_R_Land, y=pred))+
                    geom_line(data=econ1_allprovs[econ1_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=econ1_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Proportion of population with < 1ha of farmland")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

# all flat lines - no effect



          # pig_fam - quantiles ####

# this function spits out a dataframe with a range of pig_fam values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.econ2 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(pig_fam = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pig_fam = seq(min(dat$pig_fam[dat$Province==province]),
                                       max(dat$pig_fam[dat$Province==province]), length.out = 100), 
                         Les1_R_Land = mean(dat$Les1_R_Land[dat$Province==province]),
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pig_fam and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("pig_fam","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of pig_fam)  
mean.df <- compred %>% group_by(pig_fam) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(pig_fam = compred_wide$pig_fam,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="pig_fam")

return(mean.df)
  
}
 
test <- ProvMean.econ2(dat1,"Stung Treng",econ.m1)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.econ2(province=provs[i], model=econ.m1)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
econ_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and no free axis
ggplot(econ_allprovs, aes(x=pig_fam, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(econ_allprovs[emp_allprovs$Province!="Phnom Penh",], aes(x=pig_fam, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# This plot shows that there is no effect at the province level

# remove PP and free axis
ggplot(econ_allprovs[emp_allprovs$Province!="Phnom Penh",], aes(x=pig_fam, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Yep - no effect at all 

          # pig_fam - lines ####

### this function spits out a dataframe with a range of pig_fam values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.econ2 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(pig_fam = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pig_fam = seq(min(dat$pig_fam[dat$Province==province]),
                                       max(dat$pig_fam[dat$Province==province]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pig_fam and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("pig_fam","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pig_fam)  
    mean.df <- compred %>% group_by(pig_fam) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.econ2(dat1, "Stung Treng", econ.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.econ2(province=provs[i], model=econ.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
econ2_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
econ2_means <- econ2_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
econ.m2.provs <- ggplot(NULL, aes(x=pig_fam, y=pred))+
                    geom_line(data=econ2_allprovs[econ2_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=econ2_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Proportion of population with pigs")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

# all flat lines - except maybe Stung Treng and Pursat

# plot stung treng
ggplot(NULL, aes(x=pig_fam, y=pred))+
  geom_line(data=econ2_allprovs[econ2_allprovs$province=="Stung Treng",], 
            aes(group=commune),  col="grey", size=0.5)+
  geom_line(data=econ2_means[econ2_means$province=="Stung Treng",], col="black", size=1)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  ylim(0,26000)+
  xlab("Proportion of population with pigs")+
  ylab("Predicted number of forest pixels")+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=13))
# nope, nothing of interest


      # Les1_R_Land & pig_fam effects between PA and non_PA communes ####

# note I am not bothering to output the lines as I did above, as there is no evidence of commune-level effects

## now I want to have a look at differences between groups of communes. Specifically I want to look at communes in and around PAs. This is to see if there is a difference in effect of Les1_R_Land & pig_fam on the more rural/remote areas with high forest cover and PAs. 

# I need to do the same as above - split the communes into groups, predict for each commune within a group, and then get the mean, using the quantiles to show variation.  I can adapt my function from above

# I can use the PA variable in dat1 to split the groups


### Les1_R_Land

PAmean.econ <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(Les1_R_Land = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(Les1_R_Land = seq(min(dat$Les1_R_Land[dat$PA==pa]),
                                       max(dat$Les1_R_Land[dat$PA==pa]), length.out = 100), # range in province
                         pig_fam =  mean(dat$pig_fam[dat$PA==pa]),
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(econ.m1, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of Les1_R_Land and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("Les1_R_Land","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of Les1_R_Land)  
  mean.df <- compred %>% group_by(Les1_R_Land) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(Les1_R_Land = compred_wide$Les1_R_Land,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="Les1_R_Land")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.econ(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.econ(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
ggplot(pa_all, aes(x=Les1_R_Land, y=pred, group=PA, colour=PA, fill=PA))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.3, colour=NA)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))
  #xlim(-0.15,0.2)
  #ylim(0,7500)
# no effect at all for either group





# pig_fam

PAmean.econ <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(pig_fam = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pig_fam = seq(min(dat$pig_fam[dat$PA==pa]),
                                       max(dat$pig_fam[dat$PA==pa]), length.out = 100), # range in province
                         Les1_R_Land =  mean(dat$Les1_R_Land[dat$PA==pa]),
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(econ.m1, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pig_fam and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("pig_fam","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pig_fam)  
  mean.df <- compred %>% group_by(pig_fam) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(pig_fam = compred_wide$pig_fam,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="pig_fam")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.econ(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.econ(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
ggplot(pa_all, aes(x=pig_fam, y=pred, group=PA, colour=PA, fill=PA))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.3, colour=NA)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))
  #xlim(-0.15,0.2)
  #ylim(0,7500)
# no effect at all for either group


  ## Access to services ####

# dist_sch, garbage, KM_Comm

    ## FORESTED COMMUNES ####
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


anova(acc.m1,acc.m2,test="Chisq")
# simpler model is better

anova(acc.m2,acc.m3,test="Chisq")
# simpler model is better
      
anova(acc.m2,acc.m4,test="Chisq")
# simpler model is better

anova(acc.m1,acc.m5,test="Chisq")
# simpler model is better

# compare m3-m5 using AICc
acc.AIC <- data.frame(AICc = c(AICc(acc.m3),AICc(acc.m4),AICc(acc.m5)))
acc.AIC$dAICc <- acc.AIC$AICc - min(acc.AIC$AICc)
acc.AIC <- arrange(acc.AIC,dAICc)

# acc.m3 - acc.m5 are all similar in terms of AICc

#
    ## ALL COMMUNES ####
      # acc.m1, acc.m2, acc.m3 ####

acc.m1 <- glmer(ForPix ~ dist_sch + garbage + KM_Comm + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(acc.m1)
# tiny effect sizes and large approx p values

# quick plot
plot_model(acc.m1, type="pred")

# remove weakest effect (KM_Comm)
acc.m2 <- glmer(ForPix ~ dist_sch + garbage + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(acc.m2)
# no change in effect size

# anova
anova(acc.m1,acc.m2)
# simpler model is better

# remove next smallest effect (dist_sch)
acc.m3 <- glmer(ForPix ~ garbage + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(acc.m3)

# anova
anova(acc.m2,acc.m3)

# the simpler model is the best for use in the Province/Pa no PA analysis.


# I will take two strongest effects forward for the global model - dist_sch and garbage


# predict and plot global effect
# dist_sch
m2.dist_sch.newdat <- data.frame(dist_sch = seq(min(dat1$dist_sch),max(dat1$dist_sch), length.out = 100),
                            garbage = mean(dat1$garbage),
                             areaKM = mean(dat1$areaKM))
m2.dist_sch.newdat$pred <- as.vector(predict(acc.m2, newdata=m2.dist_sch.newdat, type="response", re.form=NA))

# plot 
dist_sch.m1.glob <- ggplot(m2.dist_sch.newdat, aes(x=dist_sch, y=pred))+
                  geom_line(size=1)+
                  ylim(0,20000)+
                  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                  ylab("Predicted number of forest pixels")+
                  xlab("Median distance to school (km)")+
                  theme(axis.title = element_text(size=20))+
                  theme(axis.text = element_text(size=17))

# garbage
m2.garbage.newdat <- data.frame(garbage = seq(min(dat1$garbage),max(dat1$garbage), length.out = 100),
                                 dist_sch = mean(dat1$dist_sch),
                                 areaKM = mean(dat1$areaKM))
m2.garbage.newdat$pred <- as.vector(predict(acc.m2, newdata=m2.garbage.newdat, type="response", re.form=NA))
m2.garbage.newdat$ucl <- m2.garbage.newdat$pred+(2*)

# plot 
garbage.m1.glob <- ggplot(m2.garbage.newdat, aes(x=garbage, y=pred))+
                  geom_line(size=1)+
                  ylim(0,20000)+
                  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                  ylab("Predicted number of forest pixels")+
                  xlab("Proportion of households with access to waste collection")+
                  theme(axis.title = element_text(size=20))+
                  theme(axis.text = element_text(size=17))



      # garbage effects between provinces ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


# this function spits out a dataframe with a range of garbage values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.acc <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(garbage = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(garbage = seq(min(dat$garbage[dat$Province==province]),
                                       max(dat$garbage[dat$Province==province]), length.out = 100), 
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of garbage and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("garbage","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
mean.df <- compred %>% group_by(garbage) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(garbage = compred_wide$garbage,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="garbage")

return(mean.df)
  
}
 
test <- ProvMean.acc(dat1,"Stung Treng",acc.m3)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.acc(province=provs[i], model=acc.m3)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
acc_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and no free axis
ggplot(acc_allprovs, aes(x=garbage, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(acc_allprovs[acc_allprovs$Province!="Phnom Penh",], aes(x=garbage, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# This plot shows that there is no effect at the province level

# remove PP and free axis
ggplot(acc_allprovs[acc_allprovs$Province!="Phnom Penh",], aes(x=garbage, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Yep - no effect at all 





### this function spits out a dataframe with a range of garbage values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.acc <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(garbage = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(garbage = seq(min(dat$garbage[dat$Province==province]),
                                       max(dat$garbage[dat$Province==province]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of garbage and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("garbage","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of garbage)  
    mean.df <- compred %>% group_by(garbage) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.acc(dat1, "Stung Treng", acc.m3)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.acc(province=provs[i], model=acc.m3)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
acc_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
acc_means <- acc_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
acc.m3.provs <- ggplot(NULL, aes(x=garbage, y=pred))+
                    geom_line(data=acc_allprovs[acc_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=acc_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Proportion of population with access to waste collection")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))
# nothing of interest.


      # garbage effects between PA and non_PA communes ####

## now I want to have a look at differences between groups of communes. Specifically I want to look at communes in and around PAs. This is to see if there is a difference in effect of garbage on the more rural/remote areas with high forest cover and PAs. 

# I need to do the same as above - split the communes into groups, predict for each commune within a group, and then get the mean, using the quantiles to show variation.  I can adapt my function from above

# I can use the PA variable in dat1 to split the groups


### garbage

PAmean.acc <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(garbage = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(garbage = seq(min(dat$garbage[dat$PA==pa]),
                                       max(dat$garbage[dat$PA==pa]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(acc.m3, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of garbage and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("garbage","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of garbage)  
  mean.df <- compred %>% group_by(garbage) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(garbage = compred_wide$garbage,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="garbage")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.acc(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.acc(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
ggplot(pa_all, aes(x=garbage, y=pred, group=PA, colour=PA, fill=PA))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.3, colour=NA)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))
  #xlim(-0.15,0.2)
  #ylim(0,7500)
# no effect at all for either group







  ## Social justice ####

# crim_case, land_confl

    ## FORESTED COMMUNES ####
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

anova(jus.m1,jus.m2, test="Chisq")
# simpler model is better


#
    ## ALL COMMUNES ####
      # jus.m1, jus.2 ####

# no a priori reason to think there is an interaction and so I won't test
jus.m1 <- glmer(ForPix ~ crim_case + land_confl + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(jus.m1)
# tiny effect sizes and large approx p values.

# quick plot
#plot_model(jus.m1, type="pred")

# remove weakest effect (land_confl)
jus.m2 <- glmer(ForPix ~ crim_case + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(jus.m2)
# no real change in effect

# anova
anova(jus.m1,jus.m2)
# simpler model is better

# I will tkae crim_case forward


# predict and plot global effect
m2.crim_case.newdat <- data.frame(crim_case = seq(min(dat1$crim_case),max(dat1$crim_case), length.out = 100),
                                 areaKM = mean(dat1$areaKM))
m2.crim_case.newdat$pred <- as.vector(predict(jus.m2, newdata=m2.crim_case.newdat, type="response", re.form=NA))

# plot 
crim_case.m1.glob <- ggplot(m2.crim_case.newdat, aes(x=crim_case, y=pred))+
                  geom_line(size=1)+
                  ylim(0,20000)+
                  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                  ylab("Predicted number of forest pixels")+
                  xlab("Criminal cases per capita")+
                  theme(axis.title = element_text(size=20))+
                  theme(axis.text = element_text(size=17))


      # crim_case effects between provinces ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### crim_case

# this function spits out a dataframe with a range of crim_case values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.jus <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(crim_case = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(crim_case = seq(min(dat$crim_case[dat$Province==province]),
                                       max(dat$crim_case[dat$Province==province]), length.out = 100), 
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of crim_case and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("crim_case","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of crim_case)  
mean.df <- compred %>% group_by(crim_case) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(crim_case = compred_wide$crim_case,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="crim_case")

return(mean.df)
  
}
 
test <- ProvMean.jus(dat1,"Stung Treng",jus.m2)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.jus(province=provs[i], model=jus.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
jus_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and no free axis
ggplot(jus_allprovs, aes(x=crim_case, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(jus_allprovs[jus_allprovs$Province!="Phnom Penh",], aes(x=crim_case, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# This plot shows that there is no effect at the province level

# remove PP and free axis
ggplot(jus_allprovs[jus_allprovs$Province!="Phnom Penh",], aes(x=crim_case, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Yep - no effect at all 





### this function spits out a dataframe with a range of garbage values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.jus <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(crim_case = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(crim_case = seq(min(dat$crim_case[dat$Province==province]),
                                       max(dat$crim_case[dat$Province==province]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of crim_case and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("crim_case","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of crim_case)  
    mean.df <- compred %>% group_by(crim_case) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.jus(dat1, "Stung Treng", jus.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.jus(province=provs[i], model=jus.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
jus_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
jus_means <- jus_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
jus.m2.provs <- ggplot(NULL, aes(x=crim_case, y=pred))+
                    geom_line(data=jus_allprovs[jus_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=jus_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Criminal cases per capita")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))
# nothing of interest.



      # crim_case effects between PA and non_PA communes ####

## now I want to have a look at differences between groups of communes. Specifically I want to look at communes in and around PAs. This is to see if there is a difference in effect of crim_case on the more rural/remote areas with high forest cover and PAs. 

# I need to do the same as above - split the communes into groups, predict for each commune within a group, and then get the mean, using the quantiles to show variation.  I can adapt my function from above

# I can use the PA variable in dat1 to split the groups


### garbage

PAmean.jus <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(crim_case = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(crim_case = seq(min(dat$crim_case[dat$PA==pa]),
                                       max(dat$crim_case[dat$PA==pa]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(jus.m2, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of garbage and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("crim_case","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of garbage)  
  mean.df <- compred %>% group_by(crim_case) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(crim_case = compred_wide$crim_case,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="crim_case")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.jus(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.jus(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
ggplot(pa_all, aes(x=crim_case, y=pred, group=PA, colour=PA, fill=PA))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.3, colour=NA)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))
  #xlim(-0.15,0.2)
  #ylim(0,7500)
# no effect at all for either group




  ## migration ####
    ## FORESTED COMMUNES ####
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

anova(mig.m1,mig.m2,test="Chisq")
# simpler model is better

plot_model(mig.m2, type="pred")

#
    ## ALL COMMUNES ####
      # mig.m1, mig.2, mig.3, mig.4 ####

# Here I think there is a possible cause to test interactions. Migration in an out of a commune, and what this means for socioeconomics and forest cover is potentially quite complex. 
mig.m1 <- glmer(ForPix ~ Pax_migt_in*Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m1)
# tiny effect sizes and large approx p values

# remove smallest effect (interaction)
mig.m2 <- glmer(ForPix ~ Pax_migt_in + Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m2)
# effect size of migt_in gone down and migt_out gone up

# anova
anova(mig.m1,mig.m2)
# simpler model is better


# remove smallest effect (migt_in)
mig.m3 <- glmer(ForPix ~ Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m3)
# effect size down a tiny bit

# anova
anova(mig.m2,mig.m3)
# simpler model is better

# try just migt_in
mig.m4 <- glmer(ForPix ~ Pax_migt_in + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m4)
# much smaller effect size than migt_out

AICc(mig.m3)
AICc(mig.m4)

# mig.m3 will be used for the province/PA no-PA analysis below

# pax_migt_out will be taken forward


# predict and plot global effect
m2.migt_out.newdat <- data.frame(Pax_migt_out = seq(min(dat1$Pax_migt_out),max(dat1$Pax_migt_out), 
                                                    length.out = 100),
                                  areaKM = mean(dat1$areaKM))
m2.migt_out.newdat$pred <- as.vector(predict(mig.m3, newdata=m2.migt_out.newdat, type="response", re.form=NA))

# plot 
migt_out.m1.glob <- ggplot(m2.migt_out.newdat, aes(x=Pax_migt_out, y=pred))+
  geom_line(size=1)+
  ylim(0,20000)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  ylab("Predicted number of forest pixels")+
  xlab("Number of out-migrants")+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=17))

      # Pax_migt_out effects between provinces ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### Pax_migt_out

# this function spits out a dataframe with a range of Pax_migt_out values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.mig <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(Pax_migt_out = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(Pax_migt_out = seq(min(dat$Pax_migt_out[dat$Province==province]),
                                       max(dat$Pax_migt_out[dat$Province==province]), length.out = 100), 
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of Pax_migt_out and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("Pax_migt_out","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of Pax_migt_out)  
mean.df <- compred %>% group_by(Pax_migt_out) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(Pax_migt_out = compred_wide$Pax_migt_out,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="Pax_migt_out")

return(mean.df)
  
}
 
test <- ProvMean.mig(dat1,"Stung Treng",mig.m3)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.mig(province=provs[i], model=mig.m3)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
mig_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and no free axis
ggplot(mig_allprovs, aes(x=Pax_migt_out, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(mig_allprovs[mig_allprovs$Province!="Phnom Penh",], aes(x=Pax_migt_out, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# This plot shows that there is no effect at the province level

# remove PP and free axis
ggplot(mig_allprovs[mig_allprovs$Province!="Phnom Penh",], aes(x=Pax_migt_out, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Yep - no effect at all 





### this function spits out a dataframe with a range of Pax_migt_out values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.mig <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(Pax_migt_out = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(Pax_migt_out = seq(min(dat$Pax_migt_out[dat$Province==province]),
                                       max(dat$Pax_migt_out[dat$Province==province]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of crim_case and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("Pax_migt_out","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of Pax_migt_out)  
    mean.df <- compred %>% group_by(Pax_migt_out) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.mig(dat1, "Stung Treng", mig.m3)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.mig(province=provs[i], model=mig.m3)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
mig_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
mig_means <- mig_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
mig.m3.provs <- ggplot(NULL, aes(x=Pax_migt_out, y=pred))+
                    geom_line(data=mig_allprovs[mig_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=mig_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Number of out-migrants")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))
# nothing of interest.

      # Pax_migt_out effects between PA and non_PA communes ####

## now I want to have a look at differences between groups of communes. Specifically I want to look at communes in and around PAs. This is to see if there is a difference in effect of Pax_migt_out on the more rural/remote areas with high forest cover and PAs. 

# I need to do the same as above - split the communes into groups, predict for each commune within a group, and then get the mean, using the quantiles to show variation.  I can adapt my function from above

# I can use the PA variable in dat1 to split the groups


### garbage

PAmean.mig <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(Pax_migt_out = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(Pax_migt_out = seq(min(dat$Pax_migt_out[dat$PA==pa]),
                                       max(dat$Pax_migt_out[dat$PA==pa]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(mig.m3, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of garbage and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("Pax_migt_out","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of garbage)  
  mean.df <- compred %>% group_by(Pax_migt_out) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(Pax_migt_out = compred_wide$Pax_migt_out,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="Pax_migt_out")
  
  return(mean.df)
  
}

# run function for communes with PAs
pa_mean <- PAmean.mig(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean <- PAmean.mig(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all <- rbind(pa_mean,nopa_mean)

# plot
ggplot(pa_all, aes(x=Pax_migt_out, y=pred, group=PA, colour=PA, fill=PA))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5), alpha=0.3, colour=NA)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))
  #xlim(-0.15,0.2)
  #ylim(0,7500)

# VERY slight positive effect for both groups, but barely. The PA group does appear to have a very slightly larger positive effect. 




  ## Environmental vars ####
    ## FORESTED COMMUNES ####
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
    ## ALL COMMUNES ####
      # env.m1 ####

env.m1 <- glmer(ForPix ~ mean_elev + offset(log(areaKM)) + (year|Province/Provcomm),
                family = "poisson", data=dat1)

summary(env.m1)
# decent positive effect, very small approx p value

# variable taken forward


# predict and plot global effect
m2.elev.newdat <- data.frame(mean_elev = seq(min(dat1$mean_elev),max(dat1$mean_elev), 
                                                    length.out = 100),
                                 areaKM = mean(dat1$areaKM))
m2.elev.newdat$pred <- as.vector(predict(env.m1, newdata=m2.elev.newdat, type="response", re.form=NA))

# plot 
elev.m1.glob <- ggplot(m2.elev.newdat, aes(x=mean_elev, y=pred))+
  geom_line(size=1)+
  ylim(0,20000)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  ylab("Predicted number of forest pixels")+
  xlab("Mean elevation (scaled)")+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=17))


      # diagnostics env.m1 ####


# copy data
env.m1.diag <- dat1
env.m1.diag$Provcomm <- as.factor(env.m1.diag$Provcomm)

# attach residuals
env.m1.diag$m1res <- resid(env.m1)

# attach conditional predictions
env.m1.diag$m1pred <- as.vector(predict(env.m1, type="response"))

# plot predicted vs observed
diag.env.m1 <- plot(env.m1.diag$m1pred, env.m1.diag$ForPix)
# good. Not sure why then the predictions in the section below are so high?

# residuals vs fitted
plot(env.m1.diag$m1pred, env.m1.diag$m1res)
# heteroskedasicity - particularly bad a low predicted forest cover. Similar to the popdem models.



      # elevation effects between provinces ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### mean_elev

# this function spits out a dataframe with a range of mean_elev values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.elev <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(mean_elev = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(mean_elev = seq(min(dat$mean_elev[dat$Province==province]),
                                       max(dat$mean_elev[dat$Province==province]), length.out = 100), 
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of mean_elev and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("mean_elev","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of elevation)  
mean.df <- compred %>% group_by(mean_elev) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(mean_elev = compred_wide$mean_elev,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="mean_elev")

return(mean.df)
  
}
 
test <- ProvMean.elev(dat1,"Stung Treng",env.m1)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.elev(province=provs[i], model=env.m1)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
elev_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP, free axis
ggplot(elev_allprovs, aes(x=mean_elev, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(elev_allprovs[elev_allprovs$Province!="Phnom Penh",], aes(x=mean_elev, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  ylim(0,90000)+
  facet_wrap(~Province, nrow=6)
  
# 

# remove PP and free axis
ggplot(elev_allprovs[elev_allprovs$Province!="Phnom Penh",], aes(x=mean_elev, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
# Here we see that there is an effect of elevation, and this effect varies between provinces.





### this function spits out a dataframe with a range of Pax_migt_out values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.elev <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(mean_elev = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(mean_elev = seq(min(dat$mean_elev[dat$Province==province]),
                                       max(dat$mean_elev[dat$Province==province]), length.out = 100), # range in province
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of mean_elev and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("mean_elev","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of mean_elev)  
    mean.df <- compred %>% group_by(mean_elev) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.elev(dat1, "Stung Treng", env.m1)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.elev(province=provs[i], model=env.m1)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
env_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
env_means <- env_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
env.m1.provs <- ggplot(NULL, aes(x=mean_elev, y=pred))+
                    geom_line(data=env_allprovs[env_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=env_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,100000)+
                    xlab("Mean elevation (m)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/Elevation/env.m1.lines.png",env.m1.provs,
       width = 30, height = 30, units="cm", dpi=300)


  ## Human additional variables ####
    ## FORESTED COMMUNES ####
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
anova(hum.m1,hum.m2)
# More complex model is not better

anova(hum.m2,hum.m3)
# complex model not better

anova(hum.m3, hum.m4)
# complex model not better

anova(hum.m4, hum.m5)
# complex model not better

anova(hum.m5,hum.m6)
# complex model is better - i.e worth keeping dist_border in
      

# compare with AIC
aic.comp <- data.frame(model = c("m1","m2","m3","m4","m5","m6"),
                       AICc = c(AICc(hum.m1),AICc(hum.m2),AICc(hum.m3),AICc(hum.m4),
                                AICc(hum.m5),AICc(hum.m6)))
aic.comp$dAICc <- aic.comp$AICc - min(aic.comp$AICc)
aic.comp <- arrange(aic.comp, dAICc)
# based on AICc hum.m2, hum.m4, and hum.m5 all have some support (dAICc < 3). Therefore there is cause to investigate dist_border, dist_provCap, elc, and PA. I will progress with hum.m2 

      # diagnostics hum.m2 ####

# copy data for diagnostics
hum.diag.dat <- dat1

# residuals
hum.diag.dat$m2res <- resid(hum.m2)

# conditional predictions
hum.diag.dat$m2.pred <- as.vector(predict(hum.m2, type="response", re.form=NA))

# predicted vs observed
plot(hum.diag.dat$m2.pred, hum.diag.dat$ForPix)
# not great - much worse than the previous model sets. It appears the model is underpredicting by quite a long way

# residuals vs predicted
plot(hum.diag.dat$m2.pred, hum.diag.dat$m2res)
# this doesn't look great. Some outlier large predictions which have small residuals, but a lot of heterogeneity at smaller predicted values. There's an odd line of residuals jsut below 2000 (x axis) which suggests there's one predicted value that is appearing quite a few times?


# further look at residuals by predictor
par(mfrow=c(3,2))
plot(hum.diag.dat$dist_border, hum.diag.dat$m2res, ylab = "residuals", xlab = "distance to border")
plot(hum.diag.dat$dist_provCap, hum.diag.dat$m2res, ylab = "residuals", xlab = "distance to Prov Cap")
plot(hum.diag.dat$PA, hum.diag.dat$m2res, ylab = "residuals", xlab = "PA presence")
plot(hum.diag.dat$elc, hum.diag.dat$m2res, ylab = "residuals", xlab = "Land concession presence")
boxplot(m2res ~ factor(Province), data = hum.diag.dat, outline = T, xlab = "Province", 
        ylab = "Residuals w/i Province")
boxplot(m2res ~ factor(Provcomm), data = hum.diag.dat, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")
# Based on the first two plots, it looks like there's only a relatively small number of communes that have really large residuals (and there seems to be patterns in these)

# zoom in on the y axis for the first two plots to get a better look at the majority of residuals
par(mfrow=c(2,1))
plot(hum.diag.dat$dist_border, hum.diag.dat$m2res, ylim=c(-3,3),
     ylab = "residuals", xlab = "distance to border")
plot(hum.diag.dat$dist_provCap, hum.diag.dat$m2res, ylim=c(-3,3),
     ylab = "residuals", xlab = "distance to Prov Cap")
# when you zoom in they look better!  The slightly odd patterns are smaller residuals between 0 and 1 dist_border, and between probably 0 and 0.3 for dist_provCap. 


## lets have a look at which provinces have the larger residuals and see if they match with the problem provinces from the previous model sets
levels(hum.diag.dat$Province)
# Battambang, Kampong Cham, Kampong Chhnanhg, Kampong Thom, Koh Kong, Kracheh, Mondul Kiri, Otdar Meanchey, Pursat, Ratanak Kiri, Siem Reap, Stung Treng. 
# These are the same provinces that are causing issues in the other model sets.


# I think it is because they are where forest is being lost over time. Lets try and check this using diffPix
diffPix <- dat1 %>% group_by(Provcomm) %>% summarise(sum = sum(diffPix))
provs <- unlist(strsplit(diffPix$Provcomm, "_"))
provs1 <- provs[seq(1, length(provs), 2)]
diffPix$Province <- provs1

unique(diffPix$Province[diffPix$sum > 0])
# the provinces where sone forest is lost are Kampong Cham, Kampong Speu, Kampot, Koh Kong, Kracheh, Mondul Kiri, Otdar Meanchey, Preah Sihanouk, Preah Vihear, Ratanak Kiri, Stung Treng.
# So this may go some way towards explaining the issues. However, there are still Provinces that lose no forest but are still causing issues, such as Kampong Chhnang and Kampong Thom. 


## lets try and look at the individual communes that have very large residuals.
par(mfrow=c(1,1))
boxplot(m2res ~ factor(Provcomm), data = hum.diag.dat, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")

# extract communes
prob.coms <- hum.diag.dat[hum.diag.dat$m2res > 1 | hum.diag.dat$m2res < -1,]
prob.coms$type <- "problem"
other.coms <- hum.diag.dat %>% filter(!Provcomm %in% prob.coms$Provcomm)
other.coms$type <- "other"
all.coms <- rbind(prob.coms, other.coms)


plot(prob.coms$m2.pred, prob.coms$m2res)

unique(prob.coms$Province)

# plot ForPix between the two sets
ggplot(all.coms, aes(x=Provcomm, y=ForPix, colour=type))+
  geom_point()
# this doen't show an obvious pattern to me - and in fact here the loss of forest over time doesn't look unique to the problem communes, as it did in the previous sets

# plot area between the sets
ggplot(all.coms, aes(x=Provcomm, y=areaKM, colour=type))+
  geom_point()
# no obvious pattern here

# plot dist_border between sets
ggplot(all.coms, aes(x=Provcomm, y=dist_border, colour=type))+
  geom_point()+
  theme(element_blank())
# no obvious pattern

# plot dist_provCap between sets
ggplot(all.coms, aes(x=Provcomm, y=dist_provCap, colour=type))+
  geom_point()+
  theme(element_blank())
# no obvious pattern

# compare count of communes with PA presence 
nrow(prob.coms[prob.coms$PA=="1",])
nrow(other.coms[other.coms$PA=="1",])

# compare count of communes with ELC presence 
nrow(prob.coms[prob.coms$elc=="1",])
nrow(other.coms[other.coms$elc=="1",])

### SO am not sure exactly what the other reasons are for the large residuals in those communes. It may become clearer when I look at the predictions between the global model and the commune-specific models

### There didn't appear to be any obvious issues with the residual spread for PA / ELC presence

#
      # predict main effects hum.m2 ####

### dist_border

# create new data
dist_border_newdat <- expand.grid(dist_border = seq(min(dat1$dist_border), max(dat1$dist_border), 
                                                    length.out = 100),
                                  dist_provCap = mean(dat1$dist_provCap),
                                  PA = levels(dat1$PA),
                                  elc = levels(dat1$elc),
                                  areaKM = mean(dat1$areaKM))

# predict
dist_border_newdat$pred <- as.vector(predict(hum.m2, type="response", newdata=dist_border_newdat, re.form=NA))

# plot panel grid with elc and PA
ggplot(dist_border_newdat, aes(x=dist_border, y=pred))+
  geom_line(size=1)+
  facet_grid(PA ~ elc, labeller = label_both)+
  theme(element_blank())+
  xlab("Distance to international border (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")
# PA and ELC appear to make no differnce to the effect of dist_border.  As distance to border increases, so does predicrted forest cover




### dist_provCap

# create new data
dist_provCap_newdat <- expand.grid(dist_provCap = seq(min(dat1$dist_provCap), max(dat1$dist_provCap), 
                                                    length.out = 100),
                                  dist_border = mean(dat1$dist_border),
                                  PA = levels(dat1$PA),
                                  elc = levels(dat1$elc),
                                  areaKM = mean(dat1$areaKM))

# predict
dist_provCap_newdat$pred <- as.vector(predict(hum.m2, type="response", newdata=dist_provCap_newdat, re.form=NA))

# plot
ggplot(dist_provCap_newdat, aes(x=dist_provCap, y=pred))+
  geom_line(size=1)+
  facet_grid(PA ~ elc, labeller = label_both)+
  theme(element_blank())+
  xlab("Distance to provincial capital (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")
# very similar slope - as distance to provCap increases, so does predicted forest cover. There does appear to be slight differences in the shapes.  Hard to see, but I think in communes where there is a PA and NO ELC, predicted forest cover increases more sharply than in communes with PAs AND an ELC.  I think this is also the case in communes where there is no PA - if there is an ELC, predicted forest cover increases less steeply. This is good because it makes sense!

# plot with larger y axis
ggplot(dist_provCap_newdat, aes(x=dist_provCap, y=pred))+
  geom_line(size=1)+
  facet_grid(PA ~ elc, labeller = label_both)+
  theme(element_blank())+
  ylim(0,1000)+
  xlab("Distance to provincial capital (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")
# the differences in the effects all but disappears when a more realistic y-axis limit is used - i.e. the effect, and the differences in effect, are pretty small


### PA + ELC

# create new data
PA_elc_newdat <- expand.grid(PA = levels(dat1$PA),
                             elc = levels(dat1$elc),
                             dist_border = mean(dat1$dist_border),
                             dist_provCap = mean(dat1$dist_provCap),
                             areaKM = mean(dat1$areaKM))

# predict
PA_elc_newdat$pred <- as.vector(predict(hum.m2, type="response", newdata=PA_elc_newdat, re.form=NA))


# plot PA / elc
ggplot(PA_elc_newdat, aes(x=PA,y=pred))+
  geom_point(size=2)+
  facet_grid(PA ~ elc, labeller = label_both)+
  theme(element_blank())
# So here we see that if a commune has a PA, it is predicted to have more forest, but only by a really tiny amount (~4 pixels...).  The presence of an ELC appears to make no difference


      # predict for sets of communes ####

# here I want to plot grids of different communes with the overall predicted effects, plus the commune-specific effect. I want to do this for communes with commune-level intercepts close to the mean, and communes with commune-level intercpets at the extremes. Because of the presence of 2 categorical predictors, I will need 4 plots per commune

# save the hum.m2 commune-level random effects
hum.m2.com <- ranef(hum.m2)[[1]]
plot_model(hum.m2, type="re")

# re-order
hum.m2.com <- hum.m2.com[order(hum.m2.com[ ,"(Intercept)"], decreasing = TRUE),]
head(hum.m2.com,4)

# the 4 communes closest to 0 are:
# Kracheh_Preaek Saman:Kracheh, 
# Preah Vihear_Kuleaen Tboung:Preah Vihear, 
# Preah Vihear_Chrach:Preah Vihear, 
# Kampong Cham_Kampoan:Kampong Cham

# the 4 communes furthest above 0 are:
# Kampot_Preaek Tnaot:Kampot
# Kampot_Kaoh Touch:Kampot
# Kampong Chhnang_Chieb:Kampong Chhnang
# Kampong Cham_Pongro:Kampong Cham

# the 4 communes furthest below 0 are:
# Kampong Thom_Chaeung Daeung:Kampong Thom
# Kracheh_Han Chey:Kracheh
# Preah Vihear_Reaksmei:Preah Vihear
# Siem Reap_Nokor Pheas:Siem Reap


# which communes
coms <- c("Kracheh_Preaek Saman","Preah Vihear_Kuleaen Tboung","Preah Vihear_Chrach","Kampong Cham_Kampoan")

# which provinces
provs <- c("Kracheh","Preah Vihear","Preah Vihear","Kampong Cham")


### I am making commune-specific predictions, so should customise the range of dist_border and dist_provCom I am predicting for, on the basis of each commune.

###  define the range of dist_border to predict for. Min/max per commune:
dist_border_min <- tapply(dat1$dist_border, dat1$Provcomm, min)
dist_border_max <- tapply(dat1$dist_border, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_border_min <- min(dist_border_min[names(dist_border_min) %in% coms])
dist_border_max <- max(dist_border_max[names(dist_border_max) %in% coms])

min(dat1$dist_border)
max(dat1$dist_border)


###  define the range of dist_provCap to predict for. Min/max per commune:
dist_provCap_min <- tapply(dat1$dist_provCap, dat1$Provcomm, min)
dist_provCap_max <- tapply(dat1$dist_provCap, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_provCap_min <- min(dist_provCap_min[names(dist_provCap_min) %in% coms])
dist_provCap_max <- max(dist_provCap_max[names(dist_provCap_max) %in% coms])

min(dat1$dist_provCap)
max(dat1$dist_provCap)


# create new prediction grid for specific communes with varying dist_border
hum_m2_newdat_bord <- expand.grid(Provcomm = coms,
                                dist_border = seq(dist_border_min, dist_border_max, length.out = 100),
                                dist_provCap = mean(dat1$dist_provCap),
                                year = mean(dat1$year),
                                PA = levels(dat1$PA),
                                elc = levels(dat1$elc))

# add province
hum_m2_newdat_bord$Provcomm <- as.character(hum_m2_newdat_bord$Provcomm)
provs <- unlist(strsplit(hum_m2_newdat_bord$Provcomm, "_"))
provs1 <- provs[seq(1, length(provs), 2)]
hum_m2_newdat_bord$Province <- provs1
hum_m2_newdat_bord$Provcomm <- as.factor(hum_m2_newdat_bord$Provcomm)


# add commune-specific areaKM offset                         
hum_m2_newdat_bord$areaKM <-  dat1$areaKM[match(hum_m2_newdat_bord$Provcomm, dat1$Provcomm)]

# re-order levels so they plot in the correct sets
hum_m2_newdat_bord$Provcomm <- factor(hum_m2_newdat_bord$Provcomm, 
                                     levels = c("Kracheh_Preaek Saman","Preah Vihear_Kuleaen Tboung",
                                                "Preah Vihear_Chrach","Kampong Cham_Kampoan"))


# attach commune-specific predictions
hum_m2_newdat_bord$pred.com <- as.vector(predict(hum.m2, type="response", newdata=hum_m2_newdat_bord, 
                                                re.form=~(year|Province/Provcomm)))


# attach global predictions. need to alter areaKM below so that the global model has only the mean areakM rather than the commune-specific one. This is fine as pred.com is already done
hum_m2_newdat_bord <- hum_m2_newdat_bord %>% rename(areaKM_com = areaKM)
hum_m2_newdat_bord$areaKM <- mean(dat1$areaKM)
hum_m2_newdat_bord$pred.glo <- as.vector(predict(hum.m2,type="response",newdata=hum_m2_newdat_bord,re.form=NA))



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
provcomm_lvls <- levels(hum_m2_newdat_bord$Provcomm) 
par(mfrow = c(4,4))
### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### population density range across all communes, so we need to do this overall:
ylo <- min(hum_m2_newdat_bord$pred.com)*0.9
yhi <- max(hum_m2_newdat_bord$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(hum_m2_newdat_bord$Provcomm),"dist_border"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(hum_m2_newdat_bord$Provcomm),"dist_border"])
### Iterate through the communes (levels in hum_m2_newdat_bord$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- hum_m2_newdat_bord[hum_m2_newdat_bord$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
   #if(i == 1) {
  # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$dist_border,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Distance to intl border (scaled & standardised)",
       ylab = "Predicted forest cover (forest pixel count)",
       main = unique(preddat_i$Provcomm))
   #} else {
  lines(preddat_i$dist_border,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$dist_border,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$dist_border, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

# can't face figuring out how to plot all the different lines on each plot (technically 8 lines, for each combination of the categorical variables for both the commune-specific and global models), so I am donig it in ggplot (sorry Jeroen!)
ggplot(hum_m2_newdat_bord, aes(x=dist_border))+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==1,], 
            aes(y=pred.com), colour = "red")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==0,], 
            aes(y=pred.com), colour = "red", linetype="dashed")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==1,], 
            aes(y=pred.com), colour = "blue")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==0,], 
            aes(y=pred.com), colour = "blue", linetype="dashed")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==1,],
            aes(y=pred.glo), colour = "black", linetype="solid")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==0,],
            aes(y=pred.glo), colour = "black", linetype="dashed")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==1,],
            aes(y=pred.glo), colour = "black", linetype="dotted")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==0,],
            aes(y=pred.glo), colour = "black", linetype="dotdash")+
  
  facet_wrap(~Provcomm)+
  theme(element_blank())
  
 
### Do the above again for the 4 communes with the largest intercept 

# the 4 communes furthest above 0 are:
# Kampot_Preaek Tnaot:Kampot
# Kampot_Kaoh Touch:Kampot
# Kampong Chhnang_Chieb:Kampong Chhnang
# Kampong Cham_Pongro:Kampong Cham


# which communes
coms <- c("Kampot_Preaek Tnaot","Kampot_Kaoh Touch","Kampong Chhnang_Chieb","Kampong Cham_Pongro")

# which provinces
provs <- c("Kampot","Kampot","Kampong Chhnang","Kampong Cham")


### I am making commune-specific predictions, so should customise the range of dist_border and dist_provCom I am predicting for, on the basis of each commune.

###  define the range of dist_border to predict for. Min/max per commune:
dist_border_min <- tapply(dat1$dist_border, dat1$Provcomm, min)
dist_border_max <- tapply(dat1$dist_border, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_border_min <- min(dist_border_min[names(dist_border_min) %in% coms])
dist_border_max <- max(dist_border_max[names(dist_border_max) %in% coms])

min(dat1$dist_border)
max(dat1$dist_border)


###  define the range of dist_provCap to predict for. Min/max per commune:
dist_provCap_min <- tapply(dat1$dist_provCap, dat1$Provcomm, min)
dist_provCap_max <- tapply(dat1$dist_provCap, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_provCap_min <- min(dist_provCap_min[names(dist_provCap_min) %in% coms])
dist_provCap_max <- max(dist_provCap_max[names(dist_provCap_max) %in% coms])

min(dat1$dist_provCap)
max(dat1$dist_provCap)


# create new prediction grid for specific communes with varying dist_border
hum_m2_newdat_bord <- expand.grid(Provcomm = coms,
                                  dist_border = seq(dist_border_min, dist_border_max, length.out = 100),
                                  dist_provCap = mean(dat1$dist_provCap),
                                  year = mean(dat1$year),
                                  PA = levels(dat1$PA),
                                  elc = levels(dat1$elc))

# add province
hum_m2_newdat_bord$Provcomm <- as.character(hum_m2_newdat_bord$Provcomm)
provs <- unlist(strsplit(hum_m2_newdat_bord$Provcomm, "_"))
provs1 <- provs[seq(1, length(provs), 2)]
hum_m2_newdat_bord$Province <- provs1
hum_m2_newdat_bord$Provcomm <- as.factor(hum_m2_newdat_bord$Provcomm)


# add commune-specific areaKM offset                         
hum_m2_newdat_bord$areaKM <-  dat1$areaKM[match(hum_m2_newdat_bord$Provcomm, dat1$Provcomm)]


# attach commune-specific predictions
hum_m2_newdat_bord$pred.com <- as.vector(predict(hum.m2, type="response", newdata=hum_m2_newdat_bord, 
                                                 re.form=~(year|Province/Provcomm)))


# attach global predictions. need to alter areaKM below so that the global model has only the mean areakM rather than the commune-specific one. This is fine as pred.com is already done
hum_m2_newdat_bord <- hum_m2_newdat_bord %>% rename(areaKM_com = areaKM)
hum_m2_newdat_bord$areaKM <- mean(dat1$areaKM)
hum_m2_newdat_bord$pred.glo <- as.vector(predict(hum.m2,type="response",newdata=hum_m2_newdat_bord,re.form=NA))

# plot
ggplot(hum_m2_newdat_bord, aes(x=dist_border))+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==1,], 
            aes(y=pred.com), colour = "red")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==0,], 
            aes(y=pred.com), colour = "red", linetype="dashed")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==1,], 
            aes(y=pred.com), colour = "blue")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==0,], 
            aes(y=pred.com), colour = "blue", linetype="dashed")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==1,],
            aes(y=pred.glo), colour = "black", linetype="solid")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==1 & hum_m2_newdat_bord$elc==0,],
            aes(y=pred.glo), colour = "black", linetype="dashed")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==1,],
            aes(y=pred.glo), colour = "black", linetype="dotted")+
  
  geom_line(data=hum_m2_newdat_bord[hum_m2_newdat_bord$PA==0 & hum_m2_newdat_bord$elc==0,],
            aes(y=pred.glo), colour = "black", linetype="dotdash")+
  
  facet_wrap(~Provcomm)+
  theme(element_blank())


### elc is clearly doing nothing (well, PA isn't exactly doing a lot, but more than elc).  If you look back at the AIC table, m5 and m4 were almost the same, and m2 - which had elc - had a dAICc > 2.  I wanted to see if it did anything, but clearly not. Therefore I need to check the model predictions for m4 (dist_border, dist_provCap, PA)

#
      # diagnostics hum.m4 ####

summary(hum.m4)

plot_model(hum.m4, type="pred")

# I firs ran predictions and plotted them, see sectino below


      # predict main effects hum.m4 ####

# create new data
dist_border_newdat <- expand.grid(dist_border = seq(min(dat1$dist_border), max(dat1$dist_border), 
                                                    length.out = 100),
                                  dist_provCap = mean(dat1$dist_provCap),
                                  PA = levels(dat1$PA),
                                  areaKM = mean(dat1$areaKM))

# predict
dist_border_newdat$pred <- as.vector(predict(hum.m4, type="response", newdata=dist_border_newdat, re.form=NA))

# plot
ggplot(dist_border_newdat, aes(x=dist_border, y=pred, group=PA, colour=PA))+
  geom_line(size=1)+
  theme(element_blank())+
  xlab("Distance to international border (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")
# PA appears to make no difference to the effect of dist_border.  As distance to border increases, so does predicrted forest cover


### dist_provCap

# create new data
dist_provCap_newdat <- expand.grid(dist_provCap = seq(min(dat1$dist_provCap), max(dat1$dist_provCap), 
                                                      length.out = 100),
                                   dist_border = mean(dat1$dist_border),
                                   PA = levels(dat1$PA),
                                   areaKM = mean(dat1$areaKM))

# predict
dist_provCap_newdat$pred <- as.vector(predict(hum.m4, type="response", newdata=dist_provCap_newdat, re.form=NA))

# plot
ggplot(dist_provCap_newdat, aes(x=dist_provCap, y=pred, group=PA, colour=PA))+
  geom_line(size=1)+
  theme(element_blank())+
  xlab("Distance to provincial capital (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")


# PA just doesn't do that much. I think I will continue with hum.m5 (dist_border and dist_provCap only)

#
      # diagnostics hum.m5 ####

# copy data for diagnostics
hum.diag.dat <- dat1

# residuals
hum.diag.dat$m5res <- resid(hum.m5)

# conditional predictions
hum.diag.dat$m5.pred <- as.vector(predict(hum.m5, type="response", re.form=NA))

# predicted vs observed
plot(hum.diag.dat$m5.pred, hum.diag.dat$ForPix)
# not great - much worse than the previous model sets. It appears the model is underpredicting by quite a long way

# residuals vs predicted
plot(hum.diag.dat$m5.pred, hum.diag.dat$m5res)
# this doesn't look great. Some outlier large predictions which have small residuals, but a lot of heterogeneity at smaller predicted values. There's an odd line of residuals jsut below 2000 (x axis) which suggests there's one predicted value that is appearing quite a few times?


# further look at residuals by predictor
par(mfrow=c(2,2))
plot(hum.diag.dat$dist_border, hum.diag.dat$m5res, ylab = "residuals", xlab = "distance to border")
plot(hum.diag.dat$dist_provCap, hum.diag.dat$m5res, ylab = "residuals", xlab = "distance to Prov Cap")
boxplot(m5res ~ factor(Province), data = hum.diag.dat, outline = T, xlab = "Province", 
        ylab = "Residuals w/i Province")
boxplot(m5res ~ factor(Provcomm), data = hum.diag.dat, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")
# Based on the first two plots, it looks like there's only a relatively small number of communes that have really large residuals (and there seems to be patterns in these)

# zoom in on the y axis for the first two plots to get a better look at the majority of residuals
par(mfrow=c(2,1))
plot(hum.diag.dat$dist_border, hum.diag.dat$m5res, ylim=c(-3,3),
     ylab = "residuals", xlab = "distance to border")
plot(hum.diag.dat$dist_provCap, hum.diag.dat$m5res, ylim=c(-3,3),
     ylab = "residuals", xlab = "distance to Prov Cap")
# when you zoom in they look better!  The slightly odd patterns are smaller residuals between 0 and 1 dist_border, and between probably 0 and 0.3 for dist_provCap. 


## lets have a look at which provinces have the larger residuals and see if they match with the problem provinces from the previous model sets
levels(hum.diag.dat$Province)
# Battambang, Kampong Cham, Kampong Chhnanhg, Kampong Thom, Koh Kong, Kracheh, Mondul Kiri, Otdar Meanchey, Pursat, Ratanak Kiri, Siem Reap, Stung Treng. 
# These are the same provinces that are causing issues in the other model sets.


# I think it is because they are where forest is being lost over time. Lets try and check this using diffPix
diffPix <- dat1 %>% group_by(Provcomm) %>% summarise(sum = sum(diffPix))
provs <- unlist(strsplit(diffPix$Provcomm, "_"))
provs1 <- provs[seq(1, length(provs), 2)]
diffPix$Province <- provs1

unique(diffPix$Province[diffPix$sum > 0])
# the provinces where sone forest is lost are Kampong Cham, Kampong Speu, Kampot, Koh Kong, Kracheh, Mondul Kiri, Otdar Meanchey, Preah Sihanouk, Preah Vihear, Ratanak Kiri, Stung Treng.
# So this may go some way towards explaining the issues. However, there are still Provinces that lose no forest but are still causing issues, such as Kampong Chhnang and Kampong Thom. 


## lets try and look at the individual communes that have very large residuals.
par(mfrow=c(1,1))
boxplot(m5res ~ factor(Provcomm), data = hum.diag.dat, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")

# extract communes
prob.coms <- hum.diag.dat[hum.diag.dat$m5res > 1 | hum.diag.dat$m5res < -1,]
prob.coms$type <- "problem"
other.coms <- hum.diag.dat %>% filter(!Provcomm %in% prob.coms$Provcomm)
other.coms$type <- "other"
all.coms <- rbind(prob.coms, other.coms)


plot(prob.coms$m5.pred, prob.coms$m5res)

unique(prob.coms$Province)

# plot ForPix between the two sets
ggplot(all.coms, aes(x=Provcomm, y=ForPix, colour=type))+
  geom_point()
# this doen't show an obvious pattern to me - and in fact here the loss of forest over time doesn't look unique to the problem communes, as it did in the previous sets

# plot area between the sets
ggplot(all.coms, aes(x=Provcomm, y=areaKM, colour=type))+
  geom_point()
# no obvious pattern here

# plot dist_border between sets
ggplot(all.coms, aes(x=Provcomm, y=dist_border, colour=type))+
  geom_point()+
  theme(element_blank())
# no obvious pattern

# plot dist_provCap between sets
ggplot(all.coms, aes(x=Provcomm, y=dist_provCap, colour=type))+
  geom_point()+
  theme(element_blank())
# no obvious pattern


### SO am not sure exactly what the other reasons are for the large residuals in those communes. It may become clearer when I look at the predictions between the global model and the commune-specific models



      # predict main effects hum.m5 ####

# create new data
dist_border_newdat <- expand.grid(dist_border = seq(min(dat1$dist_border), max(dat1$dist_border), 
                                                    length.out = 100),
                                  dist_provCap = mean(dat1$dist_provCap),
                                  areaKM = mean(dat1$areaKM))

# predict
dist_border_newdat$pred <- as.vector(predict(hum.m5, type="response", newdata=dist_border_newdat, re.form=NA))

# plot
ggplot(dist_border_newdat, aes(x=dist_border, y=pred))+
  geom_line(size=1)+
  theme(element_blank())+
  xlab("Distance to international border (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")
# As distance to border increases, so does predicrted forest cover


### dist_provCap

# create new data
dist_provCap_newdat <- expand.grid(dist_provCap = seq(min(dat1$dist_provCap), max(dat1$dist_provCap), 
                                                      length.out = 100),
                                   dist_border = mean(dat1$dist_border),
                                   areaKM = mean(dat1$areaKM))

# predict
dist_provCap_newdat$pred <- as.vector(predict(hum.m5, type="response", newdata=dist_provCap_newdat, 
                                              re.form=NA))

# plot
ggplot(dist_provCap_newdat, aes(x=dist_provCap, y=pred))+
  geom_line(size=1)+
  theme(element_blank())+
  xlab("Distance to provincial capital (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")
# as distance to provincial capital increases, so does predicted forest cover. This appears to be a stronger effect than dist_border.


      # predict for commune sets ####


# here I want to plot grids of different communes with the overall predicted effects, plus the commune-specific effect. I want to do this for communes with commune-level intercepts close to the mean, and communes with commune-level intercpets at the extremes. Because of the presence of 2 categorical predictors, I will need 4 plots per commune

# save the hum.m2 commune-level random effects
hum.m5.com <- ranef(hum.m5)[[1]]

# re-order
hum.m5.com <- hum.m5.com[order(hum.m5.com[ ,"(Intercept)"], decreasing = TRUE),]
head(hum.m5.com,4)

# the 4 communes closest to 0 are:
# Stung Treng_Kbal Romeas:Stung Treng
# Preah Vihear_Chhaeb Pir:Preah Vihear
# Kampong Cham_Kampoan:Kampong Cham 
# Preah Vihear_Kuleaen Tboung:Preah Vihear

# the 4 communes furthest above 0 are:
# Kampot_Preaek Tnaot:Kampot
# Kampot_Kaoh Touch:Kampot
# Kampong Chhnang_Chieb:Kampong Chhnang
# Kampong Cham_Pongro:Kampong Cham

# the 4 communes furthest below 0 are:
# Kampong Thom_Chaeung Daeung:Kampong Thom
# Kracheh_Han Chey:Kracheh
# Preah Vihear_Reaksmei:Preah Vihear
# Siem Reap_Nokor Pheas:Siem Reap


# which communes
coms <- c("Stung Treng_Kbal Romeas","Preah Vihear_Chhaeb Pir","Kampong Cham_Kampoan","Preah Vihear_Kuleaen Tboung",
          "Kampot_Preaek Tnaot","Kampot_Kaoh Touch","Kampong Chhnang_Chieb","Kampong Cham_Pongro",
          "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Preah Vihear_Reaksmei","Siem Reap_Nokor Pheas")

# which provinces
provs <- c("Stung Treng","Preah Vihear","Kampong Cham","Preah Vihear",
           "Kampot","Kampot","Kampong Chhnang","Kampong Cham",
           "Kampong Thom","Kracheh","Preah Vihear","Siem Reap")


### I am making commune-specific predictions, so should customise the range of dist_border and dist_provCom I am predicting for, on the basis of each commune.

###  define the range of dist_border to predict for. Min/max per commune:
dist_border_min <- tapply(dat1$dist_border, dat1$Provcomm, min)
dist_border_max <- tapply(dat1$dist_border, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_border_min <- min(dist_border_min[names(dist_border_min) %in% coms])
dist_border_max <- max(dist_border_max[names(dist_border_max) %in% coms])

min(dat1$dist_border)
max(dat1$dist_border)


# create new prediction grid for specific communes with varying dist_border
hum_m5_newdat_bord <- data.frame(Provcomm = rep(coms, each=100),
                                 Province = rep(provs, each=100),
                                  dist_border = seq(dist_border_min, dist_border_max, length.out = 100),
                                  dist_provCap = dat1$dist_provCap[match(hum_m5_newdat_bord$Provcomm, dat1$Provcomm)],
                                  year = mean(dat1$year))



# add commune-specific areaKM offset                         
hum_m5_newdat_bord$areaKM <-  dat1$areaKM[match(hum_m5_newdat_bord$Provcomm, dat1$Provcomm)]

# re-order levels so they plot in the correct sets
hum_m5_newdat_bord$Provcomm <- factor(hum_m5_newdat_bord$Provcomm, 
                                      levels = c("Stung Treng_Kbal Romeas","Preah Vihear_Chhaeb Pir",
                                                 "Kampong Cham_Kampoan","Preah Vihear_Kuleaen Tboung",
                                                 "Kampot_Preaek Tnaot","Kampot_Kaoh Touch","Kampong Chhnang_Chieb",
                                                 "Kampong Cham_Pongro","Kampong Thom_Chaeung Daeung",
                                                 "Kracheh_Han Chey","Preah Vihear_Reaksmei","Siem Reap_Nokor Pheas"))


# attach commune-specific predictions
hum_m5_newdat_bord$pred.com <- as.vector(predict(hum.m5, type="response", newdata=hum_m5_newdat_bord, 
                                                 re.form=~(year|Province/Provcomm)))


# attach global predictions. need to alter areaKM below so that the global model has only the mean areakM rather than the commune-specific one. This is fine as pred.com is already done
hum_m5_newdat_bord <- hum_m5_newdat_bord %>% rename(areaKM_com = areaKM)
hum_m5_newdat_bord$areaKM <- mean(dat1$areaKM)
hum_m5_newdat_bord$pred.glo <- as.vector(predict(hum.m5,type="response",newdata=hum_m5_newdat_bord,re.form=NA))



### The following plot can either "overlay" the observed ForPix count against observed dist_borders for the communes, or I can split by commune. comment out the if(i==1) statement and set par() if you want a grid

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
provcomm_lvls <- levels(hum_m5_newdat_bord$Provcomm) 
par(mfrow = c(3,4))
### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### dist_border range across all communes, so we need to do this overall:
ylo <- min(hum_m5_newdat_bord$pred.com)*0.9
yhi <- max(hum_m5_newdat_bord$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_bord$Provcomm),"dist_border"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_bord$Provcomm),"dist_border"])
### Iterate through the communes (levels in hum_m5_newdat_bord$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- hum_m5_newdat_bord[hum_m5_newdat_bord$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  #if(i == 1) {
  # Plot predicted ForPix as function of dist_border; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$dist_border,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Distance to intl border (scaled & standardised)",
       ylab = "Predicted forest cover (forest pixel count)",
       main = unique(preddat_i$Provcomm))
  #} else {
  lines(preddat_i$dist_border,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$dist_border,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$dist_border, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

### These plots suggest the global model fits poorly for communes with higher forest cover, but fits better for communes with low forest cover 



### predict for dist_provCap


###  define the range of dist_provCap to predict for. Min/max per commune:
dist_provCap_min <- tapply(dat1$dist_provCap, dat1$Provcomm, min)
dist_provCap_max <- tapply(dat1$dist_provCap, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_provCap_min <- min(dist_provCap_min[names(dist_provCap_min) %in% coms])
dist_provCap_max <- max(dist_provCap_max[names(dist_provCap_max) %in% coms])

min(dat1$dist_provCap)
max(dat1$dist_provCap)

# create new prediction grid for specific communes with varying dist_border
hum_m5_newdat_provCap <- data.frame(Provcomm = rep(coms, each=100),
                                 Province = rep(provs, each=100),
                                 dist_provCap = seq(dist_provCap_min, dist_provCap_max, length.out = 100),
                                 dist_border = dat1$dist_border[match(hum_m5_newdat_bord$Provcomm, dat1$Provcomm)],
                                 year = mean(dat1$year))



# add commune-specific areaKM offset                         
hum_m5_newdat_provCap$areaKM <-  dat1$areaKM[match(hum_m5_newdat_provCap$Provcomm, dat1$Provcomm)]

# re-order levels so they plot in the correct sets
hum_m5_newdat_provCap$Provcomm <- factor(hum_m5_newdat_provCap$Provcomm, 
                                      levels = c("Stung Treng_Kbal Romeas","Preah Vihear_Chhaeb Pir",
                                                 "Kampong Cham_Kampoan","Preah Vihear_Kuleaen Tboung",
                                                 "Kampot_Preaek Tnaot","Kampot_Kaoh Touch","Kampong Chhnang_Chieb",
                                                 "Kampong Cham_Pongro","Kampong Thom_Chaeung Daeung",
                                                 "Kracheh_Han Chey","Preah Vihear_Reaksmei","Siem Reap_Nokor Pheas"))


# attach commune-specific predictions
hum_m5_newdat_provCap$pred.com <- as.vector(predict(hum.m5, type="response", newdata=hum_m5_newdat_provCap, 
                                                 re.form=~(year|Province/Provcomm)))


# attach global predictions. need to alter areaKM below so that the global model has only the mean areakM rather than the commune-specific one. This is fine as pred.com is already done
hum_m5_newdat_provCap <- hum_m5_newdat_provCap %>% rename(areaKM_com = areaKM)
hum_m5_newdat_provCap$areaKM <- mean(dat1$areaKM)
hum_m5_newdat_provCap$pred.glo <- as.vector(predict(hum.m5,type="response",newdata=hum_m5_newdat_provCap,re.form=NA))



### The following plot can either "overlay" the observed ForPix count against observed dist_provCap for the communes, or I can split by commune. comment out the if(i==1) statement and set par() if you want a grid

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
provcomm_lvls <- levels(hum_m5_newdat_provCap$Provcomm) 
par(mfrow = c(3,4))
### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### dist_provCap range across all communes, so we need to do this overall:
ylo <- min(hum_m5_newdat_provCap$pred.com)*0.9
yhi <- max(hum_m5_newdat_provCap$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_provCap$Provcomm),"dist_border"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_provCap$Provcomm),"dist_border"])
### Iterate through the communes (levels in hum_m5_newdat_bord$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- hum_m5_newdat_provCap[hum_m5_newdat_provCap$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  #if(i == 1) {
  # Plot predicted ForPix as function of dist_border; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$dist_provCap,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Distance to provincial capital (scaled & standardised)",
       ylab = "Predicted forest cover (forest pixel count)",
       main = unique(preddat_i$Provcomm))
  #} else {
  lines(preddat_i$dist_provCap,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$dist_provCap,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$dist_provCap, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

### similar conclusions to the dist_border model

#
    ## ALL COMMUNES ####
      # hum.m1 & hum.m2 ####


# there are 5 variables in this set - distance to border (dist_border), distance to the provincial captial (dist_provCap), presence of economic land concession (elc), presence of any protected area (PA), and the protecte area category (PA_cat, includes "none")

# attempt saturated model
hum.m1 <- glmer(ForPix ~ dist_border+dist_provCap+elc+PA+PA_cat+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m1)
# dist_provCap has decent effect size and p value. dist_border potentially. Rank deficiency warning. Based on previous model selection above, I will remove PA_cat as I think that is the one that is causing the warning. There are probably too many categories

# remove PA_Cat
hum.m2 <- glmer(ForPix ~ dist_border+dist_provCap+elc+PA+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m2)
# dist_provCap still good. Effect size of dist_border (and maybe PA) suggest potentially some interest.

# Although elc doesn't appear to hav a strong effect, I want to take it forward because theoretically it is important to control for this in the models. I will be criticised by lots of people if I don't!

# will take dist_provCap, dist_border, PA, and elc forward.



# predict and plot global effect
# dist_provCap
m2.provCap.newdat <- data.frame(dist_provCap = seq(min(dat1$dist_provCap),max(dat1$dist_provCap), 
                                             length.out = 100),
                                dist_border = mean(dat1$dist_border),
                                
                             areaKM = mean(dat1$areaKM))
m2.elev.newdat$pred <- as.vector(predict(env.m1, newdata=m2.elev.newdat, type="response", re.form=NA))

# plot 
elev.m1.glob <- ggplot(m2.elev.newdat, aes(x=mean_elev, y=pred))+
  geom_line(size=1)+
  ylim(0,20000)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  ylab("Predicted number of forest pixels")+
  xlab("Mean elevation (scaled)")+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=17))


      # dist_border effects between provinces ####
        # PA == 1 ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### dist_border when elc=0 and PA=1

# this function spits out a dataframe with a range of dist_border values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.bord1 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_border = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_border = seq(min(dat$dist_border[dat$Province==province]),
                                       max(dat$dist_border[dat$Province==province]), length.out = 100),
                         dist_provCap = mean(dat1$dist_provCap[dat1$Province==province]),
                         elc = "0",
                         PA = "1",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_border and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_border","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of dist_border)  
mean.df <- compred %>% group_by(dist_border) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(dist_border = compred_wide$dist_border,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="dist_border")

return(mean.df)
  
}
 
test <- ProvMean.bord1(dat1,"Stung Treng",hum.m2)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.bord1(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
bord_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and no free axis
ggplot(bord_allprovs, aes(x=dist_border, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(bord_allprovs[bord_allprovs$Province!="Phnom Penh",], aes(x=dist_border, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# 

# remove PP and free axis
ggplot(mig_allprovs[mig_allprovs$Province!="Phnom Penh",], aes(x=Pax_migt_out, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
#





### this function spits out a dataframe with a range of Pax_migt_out values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.bord1 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_border = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_border = seq(min(dat$dist_border[dat$Province==province]),
                                       max(dat$dist_border[dat$Province==province]), length.out = 100),
                         dist_provCap = mean(dat1$dist_provCap[dat1$Province==province]),
                         elc = "0",
                         PA = "1",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_border and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_border","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of dist_border)  
    mean.df <- compred %>% group_by(dist_border) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.bord1(dat1, "Stung Treng", hum.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.bord1(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
bordLines_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
bord_means <- bordLines_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
hum.m2.bordLines <- ggplot(NULL, aes(x=dist_border, y=pred))+
                    geom_line(data=bordLines_allprovs[bordLines_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=bord_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Distance to international border (scaled)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/dist_border/hum.m2.border.pa1.lines.png",hum.m2.bordLines,
       width=30, height=30, units="cm", dpi=300)


        # PA == 0 ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### dist_border when elc=0 and PA=0

# this function spits out a dataframe with a range of dist_border values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.bord1 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_border = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_border = seq(min(dat$dist_border[dat$Province==province]),
                                       max(dat$dist_border[dat$Province==province]), length.out = 100),
                         dist_provCap = mean(dat1$dist_provCap[dat1$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_border and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_border","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of dist_border)  
mean.df <- compred %>% group_by(dist_border) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(dist_border = compred_wide$dist_border,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="dist_border")

return(mean.df)
  
}
 
test <- ProvMean.bord1(dat1,"Stung Treng",hum.m2)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.bord1(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
bord_allprovs2 <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and no free axis
ggplot(bord_allprovs2, aes(x=dist_border, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(bord_allprovs2[bord_allprovs2$Province!="Phnom Penh",], aes(x=dist_border, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# 

# remove PP and free axis
ggplot(mig_allprovs[mig_allprovs$Province!="Phnom Penh",], aes(x=Pax_migt_out, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")
#





### this function spits out a dataframe with a range of dist_border values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.bord2 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_border = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_border = seq(min(dat$dist_border[dat$Province==province]),
                                       max(dat$dist_border[dat$Province==province]), length.out = 100),
                         dist_provCap = mean(dat1$dist_provCap[dat1$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_border and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_border","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of dist_border)  
    mean.df <- compred %>% group_by(dist_border) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.bord2(dat1, "Stung Treng", hum.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.bord2(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
bordLines_allprovs2 <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
bord_means2 <- bordLines_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
hum.m2.bordLines2 <- ggplot(NULL, aes(x=dist_border, y=pred))+
                    geom_line(data=bordLines_allprovs[bordLines_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=bord_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Distance to international border (scaled)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/dist_border/hum.m2.border.pa0.lines.png",hum.m2.bordLines,
       width=30, height=30, units="cm", dpi=300)


      # dist_provCap effects between provinces ####
        # PA == 1 ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### dist_provCap when elc=0 and PA=1

# this function spits out a dataframe with a range of dist_provCap values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.provCap1 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_provCap = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_provCap = seq(min(dat$dist_provCap[dat$Province==province]),
                                       max(dat$dist_provCap[dat$Province==province]), length.out = 100),
                         dist_border = mean(dat1$dist_border[dat1$Province==province]),
                         elc = "0",
                         PA = "1",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_provCap and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_provCap","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of dist_provCap)  
mean.df <- compred %>% group_by(dist_provCap) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(dist_provCap = compred_wide$dist_provCap,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="dist_provCap")

return(mean.df)
  
}
 
test <- ProvMean.provCap1(dat1,"Stung Treng",hum.m2)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.provCap1(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
provCap_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and free axis
ggplot(provCap_allprovs, aes(x=dist_provCap, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(provCap_allprovs[provCap_allprovs$Province!="Phnom Penh",], aes(x=dist_provCap, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# 





### this function spits out a dataframe with a range of dist_provCap values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.provCap1 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_provCap = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_provCap = seq(min(dat$dist_provCap[dat$Province==province]),
                                       max(dat$dist_provCap[dat$Province==province]), length.out = 100),
                         dist_border = mean(dat1$dist_border[dat1$Province==province]),
                         elc = "0",
                         PA = "1",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_provCap and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_provCap","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of dist_provCap)  
    mean.df <- compred %>% group_by(dist_provCap) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.provCap1(dat1, "Stung Treng", hum.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.provCap1(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
provCapLines_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
provCap_means <- provCapLines_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
hum.m2.provCapLines <- ggplot(NULL, aes(x=dist_provCap, y=pred))+
                    geom_line(data=provCapLines_allprovs[provCapLines_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=provCap_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Distance to Provincial Capital (scaled)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/dist_provCap/hum.m2.provCap.pa1.lines.png",hum.m2.provCapLines,
       width=30, height=30, units="cm", dpi=300)





        # PA == 0 ####

# in order to get a provincial "mean" I am going to do the following: predict for each commune within a given province, and then take the mean of those predictions to form the provincial mean. I can then use the commune predictions to show CIs or the "variation" around the mean 


### dist_provCap when elc=0 and PA=0

# this function spits out a dataframe with a range of dist_provCap values (length=100), the mean prediction for the province, the province name, and the 2.5 and 97.5 quantiles around the mean
ProvMean.provCap2 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_provCap = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_provCap = seq(min(dat$dist_provCap[dat$Province==province]),
                                       max(dat$dist_provCap[dat$Province==province]), length.out = 100),
                         dist_border = mean(dat1$dist_border[dat1$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_provCap and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_provCap","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of dist_provCap)  
mean.df <- compred %>% group_by(dist_provCap) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(dist_provCap = compred_wide$dist_provCap,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="dist_provCap")

return(mean.df)
  
}
 
test <- ProvMean.provCap2(dat1,"Stung Treng",hum.m2)


## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.provCap1(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
provCap2_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and free axis
ggplot(provCap2_allprovs, aes(x=dist_provCap, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6, scales = "free")


# remove PP and no free axis
ggplot(provCap_allprovs[provCap_allprovs$Province!="Phnom Penh",], aes(x=dist_provCap, y=pred, group=Province))+
  geom_line()+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
  facet_wrap(~Province, nrow=6)
# 





### this function spits out a dataframe with a range of dist_provCap values (length=100), the mean prediction for the province, the province name, and the predictions for all communes within that province
ProvMeanLine.provCap2 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_provCap = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_provCap = seq(min(dat$dist_provCap[dat$Province==province]),
                                       max(dat$dist_provCap[dat$Province==province]), length.out = 100),
                         dist_border = mean(dat1$dist_border[dat1$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_provCap and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_provCap","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of dist_provCap)  
    mean.df <- compred %>% group_by(dist_provCap) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.provCap2(dat1, "Stung Treng", hum.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.provCap2(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
provCapLines2_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
provCap2_means <- provCapLines_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
hum.m2.provCapLines <- ggplot(NULL, aes(x=dist_provCap, y=pred))+
                    geom_line(data=provCapLines2_allprovs[provCapLines2_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=0.5)+
                    geom_line(data=provCap2_means, col="black", size=1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    ylim(0,26000)+
                    xlab("Distance to Provincial Capital (scaled)")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/dist_provCap/hum.m2.provCap.pa1.lines.png",hum.m2.provCapLines,
       width=30, height=30, units="cm", dpi=300)



      # elc effects between provinces ####
        # PA == 1 ####


# this function outputs a df with two mean predictions for each province, elc=1 and elc=0. It also outputs the 95% quantile intervals which are taken from the commune-level predictions 
ProvMean.elc1 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(elc = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(elc = c("1","0"),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         PA = "1",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of elc and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("elc","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of elc)  
mean.df <- compred %>% group_by(elc) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(elc = compred_wide$elc,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="elc")

return(mean.df)
  
}
 
test <- ProvMean.elc1(dat1,"Stung Treng",hum.m2)



## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.elc1(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
elc_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and free axis
hu.m2.elc.quants <- ggplot(elc_allprovs, aes(x=elc, y=pred, group=elc))+
                    geom_point(size=2)+
                    geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=0.1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~Province, nrow=6)+
                    ylim(0,26000)+
                    xlab("Presence of economic land concessions")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))
ggsave("Results/Socioeconomics/Plots/elc/hu.m2.elc.quants.png",hu.m2.elc.quants,
       width=30, height=30, units="cm", dpi=300)




### this function does the same as above but instead of quantiles it outputs all the commune predictions
ProvMeanLine.elc <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(elc = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(elc = c("1","0"),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         PA = "1",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of elc and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("elc","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of elc)  
    mean.df <- compred %>% group_by(elc) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine.elc(dat1, "Stung Treng", hum.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine.elc(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
elcLines_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
elc_means <- elcLines_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
hum.m2.elcLines <- ggplot(NULL, aes(x=elc, y=pred))+
                    geom_point(data=elcLines_allprovs[elcLines_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=1)+
                    geom_point(data=elc_means, col="black", size=1.5)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    #ylim(0,26000)+
                    xlab("Presence of economic land concessions")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))

ggsave("Results/Socioeconomics/Plots/dist_provCap/hum.m2.provCap.pa1.lines.png",hum.m2.provCapLines,
       width=30, height=30, units="cm", dpi=300)



# check if there are any communes with large differences between predictions when elc=1 and elc=0
comname <- unique(elcLines_allprovs$commune)
diff_df <- data.frame(commune = NULL, diff=NULL)

for(i in 1:length(comname)){
  df   <- elcLines_allprovs %>% filter(commune==comname[i])
  diff <- df$pred[df$elc=="0"] - df$pred[df$elc=="1"]
  df2  <- data.frame(commune = comname[i],
                    diff = diff)
  diff_df <- rbind(diff_df,df2)
    
}

summary(diff_df)
hist(diff_df$diff)
# the maximum difference in predicted forest cover between elc=1 and elc=0 (and PA==1) within any commune is 6.5. In other words, there is no effect at all really

#
        # PA == 0 ####

# this function outputs a df with two mean predictions for each province, elc=1 and elc=0. It also outputs the 95% quantile intervals which are taken from the commune-level predictions 
ProvMean.elc2 <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(elc = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(elc = c("1","0"),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of elc and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("elc","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of elc)  
mean.df <- compred %>% group_by(elc) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(elc = compred_wide$elc,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="elc")

return(mean.df)
  
}
 
test <- ProvMean.elc2(dat1,"Stung Treng",hum.m2)



## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.elc2(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
elc2_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and free axis
hu.m2.elc2.quants <- ggplot(elc2_allprovs, aes(x=elc, y=pred, group=elc))+
                    geom_point(size=2)+
                    geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=0.1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~Province, nrow=6)+
                    ylim(0,26000)+
                    xlab("Presence of economic land concessions")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))
ggsave("Results/Socioeconomics/Plots/elc/hu.m2.elc2.quants.png",hu.m2.elc2.quants,
       width=30, height=30, units="cm", dpi=300)





### this function does the same as above but instead of quantiles it outputs all the commune predictions
ProvMeanLine2.elc <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(elc = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(elc = c("1","0"),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of elc and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("elc","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of elc)  
    mean.df <- compred %>% group_by(elc) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}

test <- ProvMeanLine2.elc(dat1, "Stung Treng", hum.m2)

## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMeanLine2.elc(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
elcLines2_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# extract mean predictions
elc2_means <- elcLines_allprovs %>% filter(commune=="mean")


# no PP, free axis, separate dataframes
hum.m2.elcLines2 <- ggplot(NULL, aes(x=elc, y=pred))+
                    geom_point(data=elcLines2_allprovs[elcLines_allprovs$province!="Phnom Penh",], 
                              aes(group=commune),  col="grey", size=1)+
                    geom_point(data=elc2_means, col="black", size=1.5)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    #ylim(0,26000)+
                    xlab("Presence of economic land concessions")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))



# check if there are any communes with large differences between predictions when elc=1 and elc=0
comname <- unique(elcLines2_allprovs$commune)
diff_df <- data.frame(commune = NULL, diff=NULL)

for(i in 1:length(comname)){
  df   <- elcLines2_allprovs %>% filter(commune==comname[i])
  diff <- df$pred[df$elc=="0"] - df$pred[df$elc=="1"]
  df2  <- data.frame(commune = comname[i],
                    diff = diff)
  diff_df <- rbind(diff_df,df2)
    
}

summary(diff_df)
hist(diff_df$diff)
# the maximum difference in predicted forest cover between elc=1 and elc=0 (and PA==0) within any commune is 6.5. In other words, there is no effect at all really


      # elc effects between PA/no-PA ####

# I know I have looked at PA above, but I want to double check using a different approach. I will use the approach I used in the sections above, an split the communes in to the two groups (PA/no-PA)

PA1mean.elc <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(elc = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(elc = c("1","0"),
                         PA = "1",
                         dist_border = mean(dat$dist_border[dat$PA==pa]),
                         dist_provCap = mean(dat$dist_provCap[dat$PA==pa]),
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(hum.m2, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of elc and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("elc","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of elc)  
  mean.df <- compred %>% group_by(elc) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(elc = compred_wide$elc,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="elc")
  
  return(mean.df)
  
} # PA==1 in newdata

PA0mean.elc <- function(dat=dat1,pa){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$PA==pa])
  
  # Initialise empty dataframe
  compred <- data.frame(elc = NULL,
                        pred = NULL,
                        commune = NULL,
                        PA = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(elc = c("1","0"),
                         PA = "0",
                         dist_border = mean(dat$dist_border[dat$PA==pa]),
                         dist_provCap = mean(dat$dist_provCap[dat$PA==pa]),
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$PA==pa]),
                         Province = dat$Province[dat$Provcomm==communes[i]][1],
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(hum.m2, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of elc and the predictions, and attach commune name and PA status. 
    df <- newdat[ ,c("elc","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    df$commune <- comname 
    df$PA <- pa
    compred <- rbind(compred,df)
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of elc)  
  mean.df <- compred %>% group_by(elc) %>% summarise_at(vars(pred),mean) %>% 
    mutate(PA = pa)
  
  # get the 2.5 and 97.5 quantiles. I have to create unique identifier from the row names first, because there are duplicate rows in the data so pivot_wider gets grumpy and spits out something weird
  compred_wide <- compred %>% 
                  pivot_wider(., names_from = commune, values_from = pred, values_fn = list(pred=mean))  
  lnth <- ncol(compred_wide)
  quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))
  
  quants.vec <- data.frame(elc = compred_wide$elc,
                           Q2.5 = as.numeric(quants[1,]),
                           Q97.5 = as.numeric(quants[2,]))
  
  # join together
  mean.df <- left_join(mean.df, quants.vec, by="elc")
  
  return(mean.df)
  
} # PA==0 in newdata

# now run function for both groups
pa_mean_elc <- PA1mean.elc(pa="1")

# run function for communes with no PAs. Remove PP for the noPA group
nopa_mean_elc <- PA0mean.elc(dat=dat1[dat1$Province!="Phnom Penh",], pa="0")
pa_all_elc <- rbind(pa_mean_elc,nopa_mean_elc)

# plot
ggplot(pa_all_elc, aes(x=PA, y=pred, group=elc, color=elc))+
  geom_point(size=3, position = position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=0.2, position = position_dodge(width=0.3))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"))
# There is no difference in effect of ELC between communes with PA and communes without


#
      # PA effects between provinces ####

# this function outputs a df with two mean predictions for each province, PA=1 and PA=0. It also outputs the 95% quantile intervals which are taken from the commune-level predictions 
ProvMean.PA <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(PA = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(PA = c("1","0"),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of PA and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("PA","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of PA)  
mean.df <- compred %>% group_by(PA) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(PA = compred_wide$PA,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="PA")

return(mean.df)
  
}
 
test <- ProvMean.PA(dat1,"Stung Treng",hum.m2)



## now use the function to get the mean effects for all provinces

# create list of province names
provs <- as.character(unique(dat1$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- ProvMean.PA(province=provs[i], model=hum.m2)
  output.list[[i]] <- df
}



# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, globalenv())

# rbind
PA_allprovs <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

# plot with PP and free axis
hum.m2.PA.quants <- ggplot(PA_allprovs, aes(x=PA, y=pred, group=PA))+
                    geom_point(size=2)+
                    geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=0.1)+
                    theme(panel.background = element_blank(),axis.line = element_line(colour = "grey20"))+
                    facet_wrap(~Province, nrow=6)+
                    ylim(0,26000)+
                    xlab("Presence of Protected Areas")+
                    ylab("Predicted number of forest pixels")+
                    theme(axis.title = element_text(size=20))+
                    theme(axis.text = element_text(size=13))
# PA doesn't really predict any difference in forest cover within provinces






  ## Plotting all vars not in final model ####

# these vars were the ones that were not in the final model

nonsgVars_plot <- edu.m1.glob + emp.m1.glob +
                  econ.m1.glob + dist_sch.m1.glob +
                  garbage.m1.glob + crim_case.m1.glob +
                  migt_out.m1.glob

# remove x axes labels 
nonsgVars_plot[[1]] <- nonsgVars_plot[[1]] + theme(axis.title.x = element_blank())
nonsgVars_plot[[2]] <- nonsgVars_plot[[2]] + theme(axis.title.x = element_blank())
nonsgVars_plot[[3]] <- nonsgVars_plot[[3]] + theme(axis.title.x = element_blank())
nonsgVars_plot[[4]] <- nonsgVars_plot[[4]] + theme(axis.title.x = element_blank())
nonsgVars_plot[[5]] <- nonsgVars_plot[[5]] + theme(axis.title.x = element_blank())
nonsgVars_plot[[6]] <- nonsgVars_plot[[6]] + theme(axis.title.x = element_blank())
nonsgVars_plot[[7]] <- nonsgVars_plot[[7]] + theme(axis.title.x = element_blank())

# remove y axis labels for all except 4 and change margin for 4
nonsgVars_plot[[1]] <- nonsgVars_plot[[1]] + theme(axis.title.y = element_blank())
nonsgVars_plot[[2]] <- nonsgVars_plot[[2]] + theme(axis.title.y = element_blank())
nonsgVars_plot[[3]] <- nonsgVars_plot[[3]] + theme(axis.title.y = element_blank())
nonsgVars_plot[[5]] <- nonsgVars_plot[[5]] + theme(axis.title.y = element_blank())
nonsgVars_plot[[6]] <- nonsgVars_plot[[6]] + theme(axis.title.y = element_blank())
nonsgVars_plot[[7]] <- nonsgVars_plot[[7]] + theme(axis.title.y = element_blank())

nonsgVars_plot[[4]] <- nonsgVars_plot[[4]] + theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), 
                                                                               size=20, vjust = 0.1))

# remove y axis ticks for 2, 3, 5, 6
nonsgVars_plot[[2]] <- nonsgVars_plot[[2]] + theme(axis.text.y = element_blank())
nonsgVars_plot[[3]] <- nonsgVars_plot[[3]] + theme(axis.text.y = element_blank())
nonsgVars_plot[[5]] <- nonsgVars_plot[[5]] + theme(axis.text.y = element_blank())
nonsgVars_plot[[6]] <- nonsgVars_plot[[6]] + theme(axis.text.y = element_blank())

# add titles
nonsgVars_plot[[1]] <- nonsgVars_plot[[1]] + ggtitle("a") + theme(plot.title = element_text(hjust=0.1, size=20))
nonsgVars_plot[[2]] <- nonsgVars_plot[[2]] + ggtitle("b") + theme(plot.title = element_text(hjust=0.1, size=20))
nonsgVars_plot[[3]] <- nonsgVars_plot[[3]] + ggtitle("c") + theme(plot.title = element_text(hjust=0.1, size=20))
nonsgVars_plot[[4]] <- nonsgVars_plot[[4]] + ggtitle("d") + theme(plot.title = element_text(hjust=0.1, size=20))
nonsgVars_plot[[5]] <- nonsgVars_plot[[5]] + ggtitle("e") + theme(plot.title = element_text(hjust=0.1, size=20))
nonsgVars_plot[[6]] <- nonsgVars_plot[[6]] + ggtitle("f") + theme(plot.title = element_text(hjust=0.1, size=20))
nonsgVars_plot[[7]] <- nonsgVars_plot[[7]] + ggtitle("g") + theme(plot.title = element_text(hjust=0.1, size=20))

ggsave("Results/socioeconomics/Plots/non_sig_vars/nonsigVars_plotgrid.png", nonsgVars_plot,
       width = 30, height = 30, unit="cm", dpi=300)

## Models with multiple sets ####

# Above I have only modelled variables within a single set together. There is argument however that variables from different sets may inflence each other. Therefore I think it is worth exploring models that have pre-selected (i.e. selected because of a priori hypotheses) variables from different sets together.

# Population density is the only variable with some power, and so this will form the basis of the models.

# hypotheses:

# pop_den * education. As pop_den increases, if there are more people yet fewer males in school, then there could be more young men in agriculture/forest clearance.

# pop_den * employment. As pop_den increases, if there is a higher proportion of people in the primary sector then there are more people exerting pressure on natural resources in the same area. 

# pop_den * Les_1_R_land. If there are more people in the same area, and there are fewer people with llitte or no rice land, then that might suggest that agricultural land is expanding.

# pop_den * land_confl.  As population density increases, if land conflict also increases, there may be more land grabbing

# pop_den * pax_migt_in.  Population density may well be influenced by in-migration. If a commune's population is made up of more and more migrants, there may be more land grabbing, and traditional land management may be less influential i.e. if khmer people migrate into indigenous areas.

  ## FORESTED COMMUNES ####
   # pop_den * education ####

popdem.edu.m1 <- glmer(ForPix ~ pop_den *  M6_24_sch + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")

summary(popdem.edu.m1)

plot_model(popdem.edu.m1, type = "int")
# There doesn't appear to be an interaction. M6_24_sch does nothing

   # pop_den * employment ####

popdem.emp.m1 <- glmer(ForPix ~ pop_den *  propPrimSec + offset(log(areaKM)) + (year|Province/Provcomm), 
                       data = dat1, family = "poisson")

summary(popdem.emp.m1)
# propPrimSec is nearly sig, and the interaction term is.

plot_model(popdem.emp.m1, type = "int")
# the interaction is what I would have expected. When the population density increases, if the proportion of people engaged in the primary sector is low, forest cover decreases slower. If the proportion of people engaged in the primary sector is high, forest cover decreases quicker.

   # pop_den * economic security ####

popdem.ecsec.m1 <- glmer(ForPix ~ pop_den * Les1_R_Land + offset(log(areaKM)) + (year|Province/Provcomm), 
                       data = dat1, family = "poisson")

summary(popdem.ecsec.m1)
# No signifcant interaction and Les1_R_Land is not important

plot_model(popdem.ecsec.m1, type = "int")

   # pop_den * land conflict ####

popdem.lconf.m1 <- glmer(ForPix ~ pop_den * land_confl + offset(log(areaKM)) + (year|Province/Provcomm), 
                         data = dat1, family = "poisson", 
                         glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

summary(popdem.lconf.m1)
# although the land_confl term and the interaction term don't have signifncant approximate p values, the plot of the interaction suggests that something is going on. When population desnity increases but land conflicts are low, forest cover decreases less quickly. But when population density increases and land conflict are high, forest cover decreases more quickly.

plot_model(popdem.lconf.m1, type = "int")

   # pop_den * migration ####

popdem.mig.m1 <- glmer(ForPix ~ pop_den * Pax_migt_in + offset(log(areaKM)) + (year|Province/Provcomm), 
                         data = dat1, family = "poisson", 
                         glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

summary(popdem.mig.m1)
# migration term and interaction term don't appear sig

plot_model(popdem.mig.m1, type = "int")
# some evidence of an interaction in the plot but not hugely convincing.


   # Multiple vars ####

# a couple of the models above showed some interesting interactions - pop_den*propPrimSec and pop_den*land_confl

# try a model with both
multi.mod.1 <- glmer(ForPix ~ pop_den*propPrimSec + pop_den*land_confl + offset(log(areaKM)) +
                              (year|Province/Provcomm), 
                     data = dat1, family = "poisson")
summary(multi.mod.1)


# variance component analysis
print(VarCorr(multi.mod.1),comp="Variance") 
vars <- data.frame(term = c("Commune","year/com", "Province", "year/Prov"),
                   variance = c(1.49,0.004,1.63,0.0007))
vars$relative.contrib <- vars$variance/sum(vars$variance)

# marginal and conditional r2
r.squaredGLMM(multi.mod.1)
# marginal r2 (fixed effects) is extremely low 0.008, and the conditional (fixed + random) is 0.99.  This basically means that the random effects are explaing essentially all the variance.


# remove land_confl and see if the model is improved
multi.mod.2 <- glmer(ForPix ~ pop_den*propPrimSec + offset(log(areaKM)) +
                              (year|Province/Provcomm), 
                     data = dat1, family = "poisson")
summary(multi.mod.2)

# compare models
anova(multi.mod.1,multi.mod.2)
# the simpler model is better


# what if we added some of the control variables to the model with pop_den*propPrimSec
# mean_elev, dist_border, dist_provCap
multi.mod.3 <- glmer(ForPix ~ pop_den*propPrimSec + dist_border + dist_provCap + offset(log(areaKM)) +
                              (year|Province/Provcomm), 
                     data = dat1, family = "poisson")
summary(multi.mod.3)
# all terms appear to have some support which is good - I was worried the control vars would destroy any effect of pop_den and propPrimSec. 

# variance component analysis
print(VarCorr(multi.mod.3),comp="Variance") 
vars <- data.frame(term = c("Commune","year/com", "Province", "year/Prov"),
                   variance = c(1.36,0.004,1.58,0.0007))
vars$relative.contrib <- vars$variance/sum(vars$variance)
# variance has gone down a bit for the random effects

# marginal and conditional r2
r.squaredGLMM(multi.mod.3)
# marginal r2 has increased by an order of magnitude compared to multi.mod.1

# quick plots
plot_model(multi.mod.3, type="pred")
plot_model(multi.mod.3, type="int")


# test models with interactions between pop_den and the human control vars
multi.mod.4 <- glmer(ForPix ~ pop_den*propPrimSec + pop_den*PA + offset(log(areaKM)) +
                              (year|Province/Provcomm), 
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                     data = dat1, family = "poisson")
summary(multi.mod.4)
plot_model(multi.mod.4, type="int")
# Turns out there is a fairly decent looking interaction between pop_den and PA presence. And it makes sense!  As pop_den increases, if there is a PA in the commune then forest cover decreases at a slower rate, compared to a commune with no PA. 


# test model with PA_cat
multi.mod.5 <- glmer(ForPix ~ pop_den*propPrimSec + pop_den*PA_cat + offset(log(areaKM)) +
                              (year|Province/Provcomm), 
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                     data = dat1, family = "poisson")
summary(multi.mod.5)
plot_model(multi.mod.5, type="int")
# I think there are too many levels and not enough data here.


    # Plotting multiple variable model ####

# The best, and I guess final model is




#
  ## ALL COMMUNES ####
    # Global model - Null hyp testing ####

# first try saturated model and see if it runs
sat1 <- glmer(ForPix ~ pop_den + M6_24_sch + propPrimSec + pig_fam + dist_sch + garbage + crim_case + 
                Pax_migt_out + mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
                data=dat1, family="poisson")

summary(sat1)
# pop_den, mean_elev, dist_border, and dist_provCap all have decent effect sizes and p values. None of the others have much going for them.


## stepwise removal of terms. I will not remove elc or PA for conceptual reasons - I want to control for those two factors, even if they have small effect sizes

# remove weakest term (crim_case)
sat2 <- glmer(ForPix ~ pop_den + M6_24_sch + propPrimSec + pig_fam + dist_sch + garbage +
                Pax_migt_out + mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat2)

# anova
anova(sat1, sat2)
# simpler model is better

# remove next weakest term (M6_24_sch)
sat3 <- glmer(ForPix ~ pop_den + propPrimSec + pig_fam + dist_sch + garbage +
                Pax_migt_out + mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat3)

# anova
anova(sat2, sat3)
# simpler model is better

# remove next weakest (propPrimSec)
sat4 <- glmer(ForPix ~ pop_den + pig_fam + dist_sch + garbage + Pax_migt_out + 
                       mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat4)

# anova
anova(sat3, sat4)
# simpler model is better

# remove dist_sch
sat5 <- glmer(ForPix ~ pop_den + pig_fam + garbage + Pax_migt_out + 
                mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat5)
# garbage now is looking more interesting. half-decent effect size and decent approx p value. 

# anova
anova(sat4, sat5)
# anova suggests that the simpler model (sat5) is worse than sat4. 

AIC(sat4)
AIC(sat5)
# sat 4 is better.

# instead of removing dist_sch, remove pig_fam (the next weakest in sat4 after dist_sch)
sat6 <- glmer(ForPix ~ pop_den + dist_sch + garbage + Pax_migt_out + 
                mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat6)

# anova
anova(sat4,sat6)
# the simpler model is better

# test whether sat6 is better than sat5
anova(sat5,sat6)
# the model with dist_sch (sat6) is better 

# remove garbage
sat7 <- glmer(ForPix ~ pop_den + dist_sch + Pax_migt_out + 
                mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat7)

# compare with sat6
anova(sat6,sat7)
# simpler model (sat7) is better

# remove dist_sch again as it is the weaker term
sat8 <- glmer(ForPix ~ pop_den + Pax_migt_out + 
                mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat8)

# anova
anova(sat7,sat8)
# simpler model (sat8) is better

# remove Pax_migt_out
sat9 <- glmer(ForPix ~ pop_den + 
                mean_elev + dist_border+dist_provCap+elc+PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")

summary(sat9)

# anova
anova(sat8,sat9)
# simpler model (sat9) is better


# try remove elc and PA and see if convergence warning goes away
sat10 <- glmer(ForPix ~ pop_den + 
                mean_elev + dist_border+dist_provCap+PA+
                offset(log(areaKM)) + (year|Province/Provcomm),
              data=dat1, family="poisson")
# it doesn't


# compare with AICc
aic.comp <- data.frame(model = c("sat1","sat2","sat3","sat4","sat5","sat6","sat7","sat8","sat9"),
                       AICc = c(AICc(sat1),AICc(sat2),AICc(sat3),AICc(sat4),
                                AICc(sat5),AICc(sat6), AICc(sat7), AICc(sat8), AICc(sat9)))
aic.comp$dAICc <- aic.comp$AICc - min(aic.comp$AICc)
aic.comp <- arrange(aic.comp, dAICc)

# check if AIC agrees
aic.comp$AIC <- c(AIC(sat1),AIC(sat2),AIC(sat3),AIC(sat4),AIC(sat5),AIC(sat6),AIC(sat7),AIC(sat8),AIC(sat9))
aic.comp$dAIC <- aic.comp$AIC - min(aic.comp$AIC)
aic.comp <- arrange(aic.comp, dAIC)

# AIC suggests that models with dist_sch an pax_migt_out have some support. But based on the effect sizes and some quick plots, both of those variables are not important.

# therefore sat9 is the best and final model.  This is supported by all of the individual model sets I ran above - the only variables that have any effect are in the model sat9 (as well as elc and PA for conceptual reasons).

# there is a convergence warning for sat9, which I will tr and deal with below

# run sat 9 with all available optimisers following the guidance in help("convergence")
sat9.all <- allFit(sat9)
ss <- summary(sat9.all)
ss$fixef
# most of the parameter estimates are very similar. The optimizer "optimx.L-BFGS-B" seems to be different to the rest 
ss$llik
# log likelihoods are all very similar, which is good. 
ss$sdcor
# not sure what I'm looking for here
ss$which.OK
# so apparently they all worked. Below I will try and run them all and see if the warning goes away

# use default bobyqa but with more iterations
sat9a <- glmer(ForPix ~ pop_den + mean_elev + dist_border + dist_provCap + elc + PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
                data=dat1, family="poisson", 
                control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
# still has warning

# Nelder-Mead
sat9b <- glmer(ForPix ~ pop_den + mean_elev + dist_border + dist_provCap + elc + PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
                data=dat1, family="poisson", 
                control=glmerControl(optimizer="Nelder_Mead",  optCtrl=list(maxfun=100000)))
# still has warning


# nlminbwrap
sat9c <- glmer(ForPix ~ pop_den + mean_elev + dist_border + dist_provCap + elc + PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
                data=dat1, family="poisson", 
                control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))
summary(sat9c)
# No warning!!


#
    # Global model - Information Theoretic approcah ####


# Instead of the null hypothesis testing appraoch used above (i.e. step-wise removal of terms) I will build a candidate set of models and model select/average

m1 <- glmer(ForPix ~ pop_den + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m2 <-  glmer(ForPix ~ M6_24_sch + mean_elev + dist_border+dist_provCap+elc+PA + 
               offset(log(areaKM)) + (year|Province/Provcomm),
             data=dat1, family="poisson",
             control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m3 <- glmer(ForPix ~ propPrimSec + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m4 <- glmer(ForPix ~ pig_fam + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m5 <- glmer(ForPix ~ dist_sch + garbage + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m6 <- glmer(ForPix ~ crim_case + garbage + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m7 <- glmer(ForPix ~ Pax_migt_out + garbage + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m8 <- glmer(ForPix ~ M6_24_sch + dist_sch + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m9 <- glmer(ForPix ~ propPrimSec + Pax_migt_out + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m10 <- glmer(ForPix ~ pop_den + M6_24_sch + propPrimSec + pig_fam + dist_sch + garbage + crim_case + 
               Pax_migt_out + mean_elev + dist_border+dist_provCap+elc+PA + 
              offset(log(areaKM)) + (year|Province/Provcomm),
            data=dat1, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
# m1 is the top model by a long way

summary(m1)

    # Diagnostics sat9c ####

# copy data
sat9c.diag.dat <- dat1
sat9c.diag.dat$Provcomm <- as.factor(sat9c.diag.dat$Provcomm)

### Make "fitted" predictions, i.e. fully conditional:
sat9c.diag.dat$pred <- predict(sat9c, type = "response")

### Plot predicted against observed:
plot(sat9c.diag.dat$ForPix, sat9c.diag.dat$pred, ylab = "Predicted ForPix", xlab = "Observed ForPix")
### Nice!

### Extract model residuals:
sat9c.diag.dat$resid <- resid(sat9c)

### Plot residuals against fitted values:
plot(sat9c.diag.dat$pred, sat9c.diag.dat$resid, xlab="Predicted values", ylab="Model residuals")
# lots of heterogeneity at lower predicted values of ForPix

## QQ plot of random effects (using sjPlot)
plot_model(sat9c,type="diag")

# plot residuals of model
hist(resid(sat9c))


print(VarCorr(sat9c),comp="Variance") 
vars <- data.frame(term = c("Commune","year/com", "Province", "year/Prov"),
                   variance = c(10.4,0.0046,6.7,0.00048))
vars$relative.contrib <- vars$variance/sum(vars$variance)
# Commune is making up the majority of the variance (~60%), followed by Province (~39%), with year making up the negligible rest. 

# marginal and conditional r2
r.squaredGLMM(sat9c)
# Marginal r2 (fixed effects) is relatively high 0.78, and the conditional (fixed + random) is 1.  This means that the fixed effects are actually accounting for most of the variance. 

plot(sat9c)


      # global predictions ####

# sat9c is the model I will be using to predict all partial effects

# I will start by running global predictions (i.e. for the whole country).  Here I am expecting relatively small effects to be shown.  I then expect to see a much wider range of effects when I predict at the province level


        # pop_den ####

## plotting on the response scale

# create new data. I will set mean_elev, dist_border, dist_provCap at their national means, and I will set elc and PA to 0.
pop_den_newdat <- data.frame(pop_den = seq(min(dat1$pop_den), max(dat1$pop_den), length.out = 200),
                             mean_elev = mean(dat1$mean_elev),
                             dist_border = mean(dat1$dist_border),
                             dist_provCap = mean(dat1$dist_provCap),
                             elc = "0",
                             PA = "0",
                             areaKM = median(dat1$areaKM))
pop_den_newdat$pred <- as.vector(predict(sat9c, type="response", newdata=pop_den_newdat, re.form=NA))
popden_SE <- 1.127
pop_den_newdat$U_SE <- pop_den_newdat$pred + (2*popden_SE)
pop_den_newdat$L_SE <- pop_den_newdat$pred - (2*popden_SE)

# plot
pop_den_plot <- ggplot()+
                geom_point(data=dat1, aes(x=pop_den, y=ForPix))+
                geom_line(data=pop_den_newdat, aes(x=pop_den, y=pred), size=2, color="red")+
                geom_ribbon(data=pop_den_newdat, aes(x=pop_den, y=pred,ymin=L_SE, ymax=U_SE),
                            alpha=0.4, fill="red")+
                xlim(-0.17,1)+
                ylim(0,100)+
                theme(panel.background = element_blank(),
                      axis.line = element_line(colour = "grey20"),
                      axis.title = element_text(size=17),
                      axis.text = element_text(size=15))+
                ylab("Predicted forest pixels per unit area (km2)")+
                xlab("Population density (scaled)")



### plotting on the log scale

# create new data. I will set mean_elev, dist_border, dist_provCap at their national means, and I will set elc and PA to 0.
pop_den_newdat_ln <- data.frame(pop_den = seq(min(dat1$pop_den), max(dat1$pop_den), length.out = 200),
                             mean_elev = mean(dat1$mean_elev),
                             dist_border = mean(dat1$dist_border),
                             dist_provCap = mean(dat1$dist_provCap),
                             elc = "0",
                             PA = "0",
                             areaKM = median(dat1$areaKM))
pop_den_newdat_ln$pred <- as.vector(predict(m1, type="link", newdata=pop_den_newdat_ln, re.form=NA))
pop_den_newdat_ln$pop_den2 <- pop_den_newdat_ln$pop_den + 0.17
pop_den_newdat_ln$pop_den_log <- log(pop_den_newdat_ln$pop_den2)

# Take raw values and log them
pop_den_raw <- data.frame(pop_den = dat1$pop_den,
                          ForPix = dat1$ForPix)

pop_den_raw$pop_den2 <- pop_den_raw$pop_den + 0.17
pop_den_raw$pop_den_log <- log(pop_den_raw$pop_den2)
pop_den_raw$ForPix_log <- log(pop_den_raw$ForPix)

ggplot()+
  geom_point(data=pop_den_raw, aes(x=pop_den_log, y=ForPix_log))+
  geom_line(data=pop_den_newdat_ln, aes(x=pop_den_log, y=pred))
  


#
        # mean_elev ####

# create new data. I will set pop_den, dist_border, dist_provCap at their national means, and I will set elc and PA to 0.
mean_elev_newdat <- data.frame(mean_elev = seq(min(dat1$mean_elev), max(dat1$mean_elev), length.out = 200),
                             pop_den = mean(dat1$pop_den),
                             dist_border = mean(dat1$dist_border),
                             dist_provCap = mean(dat1$dist_provCap),
                             elc = "0",
                             PA = "0",
                             areaKM = mean(dat1$areaKM))
mean_elev_newdat$pred <- as.vector(predict(sat9c, type="response", newdata=mean_elev_newdat, re.form=NA))


# plot
ggplot()+
    geom_point(data=dat1, aes(x=mean_elev, y=ForPix))+
    geom_line(data=mean_elev_newdat, aes(x=mean_elev, y=pred), size=2, color="red")+
    #xlim(-0.18,5)+
    ylim(0,26000)+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "grey20"),
          axis.title = element_text(size=17),
          axis.text = element_text(size=15))+
    ylab("Predicted forest pixels per unit area (km2)")+
    xlab("Elevation (scaled)")
   

## Need to check with Jeroen about what the pred actually is.  Is it predicted forest pixels per unit area?  Becuase if so, these estimates don't make sense - you can't have 10K pixels per km2. In theory you can only have 11.11 pixels per km2 becuase each pixel is 0.09km2
# After chatting with Jeroen - yes the predictions should be per unit area



## Jeroen pointed out that the distribution of areaKM is quite skewed, and so the mean is probably not the best value to be predicting from.  Below I will try using the median

# create new data. I will set pop_den, dist_border, dist_provCap at their national means, and I will set elc and PA to 0.
mean_elev_newdat2 <- data.frame(mean_elev = seq(min(dat1$mean_elev), max(dat1$mean_elev), length.out = 200),
                             pop_den = mean(dat1$pop_den),
                             dist_border = mean(dat1$dist_border),
                             dist_provCap = mean(dat1$dist_provCap),
                             elc = "0",
                             PA = "0",
                             areaKM = median(dat1$areaKM))
mean_elev_newdat2$pred <- as.vector(predict(sat9c, type="response", newdata=mean_elev_newdat2, re.form=NA))
meanelev_SE <- 0.122
mean_elev_newdat2$U_SE <- mean_elev_newdat2$pred + (2*meanelev_SE)
mean_elev_newdat2$L_SE <- mean_elev_newdat2$pred - (2*meanelev_SE)


# plot
mean_elev_plot <- ggplot()+
                  geom_point(data=dat1, aes(x=mean_elev, y=ForPix))+
                  geom_line(data=mean_elev_newdat2, aes(x=mean_elev, y=pred), size=2, color="red")+
                  geom_ribbon(data=mean_elev_newdat2, aes(x=mean_elev, y=pred, ymin=L_SE, ymax=U_SE),
                              alpha=0.4, fill="red")+
                  #xlim(-0.18,5)+
                  ylim(0,26000)+
                  theme(panel.background = element_blank(),
                        axis.line = element_line(colour = "grey20"),
                        axis.title = element_text(size=17),
                        axis.text = element_text(size=15))+
                  ylab("Predicted forest pixels per unit area (km2)")+
                  xlab("Mean elevation (masl, scaled)")
            


        # dist_border ####


# create new data. I will set pop_den, mean_elev, dist_provCap at their national means, and I will set elc and PA to 0.
dist_border_newdat <- data.frame(dist_border = seq(min(dat1$dist_border), max(dat1$dist_border), 
                                                   length.out = 200),
                             pop_den = mean(dat1$pop_den),
                             mean_elev = mean(dat1$mean_elev),
                             dist_provCap = mean(dat1$dist_provCap),
                             elc = "0",
                             PA = "0",
                             areaKM = median(dat1$areaKM))
dist_border_newdat$pred <- as.vector(predict(sat9c, type="response", newdata=dist_border_newdat, re.form=NA))

# plot
dist_border_plot <- ggplot()+
                    geom_point(data=dat1, aes(x=dist_border, y=ForPix))+
                    geom_line(data=dist_border_newdat, aes(x=dist_border, y=pred), size=2, color="red")+
                    #xlim(-0.18,5)+
                    #ylim(0,26000)+
                    theme(panel.background = element_blank(),
                          axis.line = element_line(colour = "grey20"),
                          axis.title = element_text(size=17),
                          axis.text = element_text(size=15))+
                    ylab("Predicted forest pixels per unit area (km2)")+
                    xlab("Distance to international border (KM, scaled)")
                    



        # dist_provCap ####

# create new data. I will set pop_den, mean_elev, dist_border at their national means, and I will set elc and PA to 0.
dist_provCap_newdat <- data.frame(dist_provCap = seq(min(dat1$dist_provCap), max(dat1$dist_provCap), 
                                                     length.out = 200),
                             pop_den = mean(dat1$pop_den),
                             mean_elev = mean(dat1$mean_elev),
                             dist_border = mean(dat1$dist_border),
                             elc = "0",
                             PA = "0",
                             areaKM = median(dat1$areaKM))
dist_provCap_newdat$pred <- as.vector(predict(sat9c, type="response", newdata=dist_provCap_newdat, re.form=NA))

# plot
dist_provCap_plot <- ggplot()+
                    geom_point(data=dat1, aes(x=dist_provCap, y=ForPix))+
                    geom_line(data=dist_provCap_newdat, aes(x=dist_provCap, y=pred),size=2, color="red")+
                    #xlim(-0.18,5)+
                    #ylim(0,26000)+
                    theme(panel.background = element_blank(),
                          axis.line = element_line(colour = "grey20"),
                          axis.title = element_text(size=17),
                          axis.text = element_text(size=15))+
                    ylab("Predicted forest pixels per unit area (km2)")+
                    xlab("Distance to provincial capital (KM, scaled)")
                   



        # elc ####

# create new data. I will set pop_den, mean_elev, dist_border, dist_proCap at their national means, and I will set PA to 0.
elc_newdat <- data.frame(elc = c("1","0"),
                        pop_den = mean(dat1$pop_den),
                        mean_elev = mean(dat1$mean_elev),
                        dist_border = mean(dat1$dist_border),
                        dist_provCap = mean(dat1$dist_provCap),
                        PA = "0",
                        areaKM = mean(dat1$areaKM))
elc_newdat$pred <- as.vector(predict(sat9c, type="response", newdata=elc_newdat, re.form=NA))

# plot
elc_plot <- ggplot(elc_newdat, aes(x=elc, y=pred))+
            geom_point(size=3)+
            #xlim(-0.18,5)+
            ylim(0,5)+
            theme(panel.background = element_blank(),
                  axis.line = element_line(colour = "grey20"),
                  axis.title = element_text(size=17),
                  axis.text = element_text(size=15))+
            ylab("Predicted forest pixels per unit area (km2)")+
            xlab("Presence of economic land concessions")
           


        # PA ####

# create new data. I will set pop_den, mean_elev, dist_border, dist_proCap at their national means, and I will set elc to 0.
PA_newdat <- data.frame(PA = c("1","0"),
                        pop_den = mean(dat1$pop_den),
                        mean_elev = mean(dat1$mean_elev),
                        dist_border = mean(dat1$dist_border),
                        dist_provCap = mean(dat1$dist_provCap),
                        elc = "0",
                        areaKM = mean(dat1$areaKM))
PA_newdat$pred <- as.vector(predict(sat9c, type="response", newdata=PA_newdat, re.form=NA))

# plot
pa_plot <- ggplot(PA_newdat, aes(x=PA, y=pred))+
            geom_point(size=3)+
            #xlim(-0.18,5)+
            ylim(0,5)+
            theme(panel.background = element_blank(),
                  axis.line = element_line(colour = "grey20"),
                  axis.title = element_text(size=17),
                  axis.text = element_text(size=15))+
            ylab("Predicted forest pixels per unit area (km2)")+
            xlab("Presence of protected areas")

        # Point estimate predictions (for results section) ####

# following from Nils' advice, for describing the effects of the predictors, I am going to predict for a couple of point estimates for each predictor. Then I can compare them in the text of the results section. This is because the coefficients are on the log scale, and so it is challenging to simply talk about effect sies as you would with a normal lm. 

# First identiy the two point estimates for each variable. Nils suggested points that are of interest. So I'll need to explore the range of predictors a bit.

# load data that is already aggregated to the Province level
dat2 <- read.csv("Data/commune/dat2.csv", header=T, stringsAsFactors = T)

# copy data
pt_est_dat <- dat2

plot(pt_est_dat$pop_den, pt_est_dat$Province)

# get mean for each province for vars of interest
pt_est_dat_mean <- pt_est_dat %>% 
                    select(Province,pop_den,mean_elev,dist_border,dist_provCap,elc,PA,areaKM) %>% 
                    group_by(Province) %>% 
                    summarise_at(vars(pop_den,mean_elev,dist_border,dist_provCap,areaKM), mean)

plot(pt_est_dat_mean$pop_den, pt_est_dat_mean$Province)
summary(pt_est_dat_mean$pop_den)

# all of the predictions are basically identical so I think I should use the most extreme values (max, min)

## I think I want to do two sets of point estimate predictions for each variable. 1) the minimum and maximum values for that variable using the global model (re.form=NA), and then 2) the min and max of that variable but using the random effects, so taking into account the province and the commune. This will hopefully show that the global model is shite, but that when you take the random effects into account there are bigger differences in the predictions. 

# remove Phnom Penh from data
dat3 <- dat1[dat1$Province != "Phnom Penh", ]


## create dataframes for predicting point estimates  

## pop_den (RE)

# find the province, commune, and area for the min and max values of pop_den
pop_den_details <- dat3 %>% select(Province,Commune,areaKM,pop_den) %>% 
                            filter(pop_den == min(pop_den) | pop_den == max(pop_den))

# Dataframe using all of the commune-specific values. I don't think this is what I want, as this is not reflecting anything about pop_den. I need all of the other variables to be at their mean, with only pop_den varying. See dataframe below this
#RE_pop_den <- data.frame(pop_den = c(min(dat3$pop_den), max(dat3$pop_den)),
 #                        mean_elev = c(dat3$mean_elev[dat3$Commune=="Ta Tey Leu"][1],dat3$mean_elev[dat3$Commune=="Preaek Ruessei"][1]),
  #                       dist_border = c(dat3$dist_border[dat3$Commune=="Ta Tey Leu"][1],dat3$dist_border[dat3$Commune=="Preaek Ruessei"][1]),
   #                      dist_provCap = c(dat3$dist_provCap[dat3$Commune=="Ta Tey Leu"][1],dat3$dist_provCap[dat3$Commune=="Preaek Ruessei"][1]),
    #                     elc = factor(c(as.character(dat3$elc[dat3$Commune=="Ta Tey Leu"][1]),
     #                                 as.character(dat3$elc[dat3$Commune=="Preaek Ruessei"][1]))),
      #                   PA =  factor(c(as.character(dat3$PA[dat3$Commune=="Ta Tey Leu"][1]),
       #                                 as.character(dat3$PA[dat3$Commune=="Preaek Ruessei"][1]))),
        #                 areaKM = mean(dat3$areaKM),
         #                Province = as.factor(c("Koh Kong","Kandal")),
          #               Provcomm = as.factor(c("Koh Kong_Ta Tey Leu","Kandal_Preaek Ruessei")),
           #              year = mean(dat3$year))

#RE_pop_den$pred <- as.vector(predict(m1,newdata=RE_pop_den, type="response",re.form=~year|Province/Provcomm))

# dataframe where only pop_den (and commune/province) vary
RE_pop_den <- data.frame(pop_den = c(min(dat3$pop_den), max(dat3$pop_den)),
                         mean_elev = mean(dat3$mean_elev),
                         dist_border = mean(dat3$dist_border),
                         dist_provCap = mean(dat3$dist_provCap),
                         elc = "0",
                         PA = "0",
                         areaKM = c(pop_den_details[2,3], pop_den_details[1,3]),
                         Province = as.factor(c("Koh Kong","Kandal")),
                         Provcomm = as.factor(c("Koh Kong_Ta Tey Leu","Kandal_Preaek Ruessei")),
                         year = mean(dat3$year))

RE_pop_den$pred <- as.vector(predict(m1,newdata=RE_pop_den, type="response",re.form=~year|Province/Provcomm))


## pop_den (global)

# dataframe for global model predictions on varying pop_den
Glob_pop_den <- data.frame(pop_den = c(min(dat3$pop_den), max(dat3$pop_den)),
                         mean_elev = mean(dat3$mean_elev),
                         dist_border = mean(dat3$dist_border),
                         dist_provCap = mean(dat3$dist_provCap),
                         elc = "0",
                         PA = "0",
                         areaKM = mean(dat3$areaKM))

Glob_pop_den$pred <- as.vector(predict(m1,newdata=Glob_pop_den, type="response",re.form=NA))


## mean_elev (RE)

# find the province, commune, and area for the min and max values of mean_elev
elev_details <- dat3 %>% select(Province,Commune,areaKM,mean_elev) %>% 
  filter(mean_elev == min(mean_elev) | mean_elev == max(mean_elev))

# dataframe where only mean_elev (and commune/province) vary
RE_mean_elev <- data.frame(mean_elev = c(min(dat3$mean_elev), max(dat3$mean_elev)),
                         pop_den = mean(dat3$pop_den),
                         dist_border = mean(dat3$dist_border),
                         dist_provCap = mean(dat3$dist_provCap),
                         elc = "0",
                         PA = "0",
                         areaKM = c(elev_details[1,3], elev_details[5,3]),
                         Province = as.factor(c("Svay Rieng","Mondul Kiri")),
                         Provcomm = as.factor(c("Svay Rieng_Thmei","Mondul Kiri_Dak Dam")),
                         year = mean(dat3$year))

RE_mean_elev$pred <- as.vector(predict(m1,newdata=RE_mean_elev, type="response",re.form=~year|Province/Provcomm))


## mean_elev (global)

# dataframe for global model predictions on varying mean_elev
glob_elev <- data.frame(mean_elev = c(min(dat3$mean_elev), max(dat3$mean_elev)),
                        pop_den = mean(dat3$pop_den),
                        dist_border = mean(dat3$dist_border),
                        dist_provCap = mean(dat3$dist_provCap),
                        elc = "0",
                        PA = "0",
                        areaKM = mean(dat3$areaKM))

glob_elev$pred <- as.vector(predict(m1,newdata=glob_elev, type="response",re.form=NA))



## dist_border (RE)

# find the province, commune, and area for the min and max values of dist_border
border_details <- dat3 %>% select(Province,Commune,areaKM,dist_border,Provcomm,elc,PA) %>% 
  filter(dist_border == min(dist_border) | dist_border == max(dist_border))

# dataframe where only dist_border (and commune/province) vary
RE_border <- data.frame(dist_border = c(min(dat3$dist_border), max(dat3$dist_border)),
                           pop_den = mean(dat3$pop_den),
                           mean_elev = mean(dat3$mean_elev),
                           dist_provCap = mean(dat3$dist_provCap),
                           elc = "0",
                           PA = "0",
                           areaKM = c(border_details[1,3], border_details[6,3]),
                           Province = as.factor(c("Kampong Cham","Kampong Thom")),
                           Provcomm = as.factor(c("Kampong Cham_Rung","Kampong Thom_Phat Sanday")),
                           year = mean(dat3$year))

RE_border$pred <- as.vector(predict(m1,newdata=RE_border, type="response",re.form=~year|Province/Provcomm))


## dist_border (global)

# dataframe for global model predictions on varying mean_elev
glob_border <- data.frame(dist_border = c(min(dat3$dist_border), max(dat3$dist_border)),
                        pop_den = mean(dat3$pop_den),
                        mean_elev = mean(dat3$mean_elev),
                        dist_provCap = mean(dat3$dist_provCap),
                        elc = "0",
                        PA = "0",
                        areaKM = mean(dat3$areaKM))

glob_border$pred <- as.vector(predict(m1,newdata=glob_border, type="response",re.form=NA))



#
        # All global prediction plots ####

all_plot <- pop_den_plot + mean_elev_plot + dist_border_plot + dist_provCap_plot + elc_plot + pa_plot

# remove y axis titles except for 1 and 4
all_plot[[2]] <- all_plot[[2]] + theme(axis.title.y = element_blank()) 
all_plot[[3]] <- all_plot[[3]] + theme(axis.title.y = element_blank())
all_plot[[5]] <- all_plot[[5]] + theme(axis.title.y = element_blank())
all_plot[[6]] <- all_plot[[6]] + theme(axis.title.y = element_blank())

# change size of y axis titles
all_plot[[1]] <- all_plot[[1]] + theme(axis.title.y = element_text(size=15)) 
all_plot[[4]] <- all_plot[[4]] + theme(axis.title.y = element_text(size=15)) 

# remove x axis titles
all_plot[[1]] <- all_plot[[1]] + theme(axis.title.x = element_blank()) 
all_plot[[2]] <- all_plot[[2]] + theme(axis.title.x = element_blank()) 
all_plot[[3]] <- all_plot[[3]] + theme(axis.title.x = element_blank())
all_plot[[4]] <- all_plot[[4]] + theme(axis.title.x = element_blank()) 
all_plot[[5]] <- all_plot[[5]] + theme(axis.title.x = element_blank())
all_plot[[6]] <- all_plot[[6]] + theme(axis.title.x = element_blank())

ggsave("Results/Socioeconomics/Plots/global_model_global_predictions/glob_mod_plots.png",all_plot,
       width = 45, height = 35, unit="cm", dpi=300)


      # Province-level predictions ####
        # Functions ####

### commune-specific predictions and provincial means

## output dataframe with province mean and 95% quantiles
ProvMean.popden  <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(pop_den = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pop_den = seq(min(dat$pop_den[dat$Province==province]),
                                       max(dat$pop_den[dat$Province==province]), length.out = 100), # range in province
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("pop_den","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
mean.df <- compred %>% group_by(pop_den) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(pop_den = compred_wide$pop_den,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="pop_den")

return(mean.df)
  
}
ProvMean.elev    <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(mean_elev = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(mean_elev = seq(min(dat$mean_elev[dat$Province==province]),
                                       max(dat$mean_elev[dat$Province==province]), length.out = 100), # range in province
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of mean_elev and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("mean_elev","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of mean_elev)  
mean.df <- compred %>% group_by(mean_elev) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(mean_elev = compred_wide$mean_elev,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="mean_elev")

return(mean.df)
  
}
ProvMean.border  <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_border = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_border = seq(min(dat$dist_border[dat$Province==province]),
                                       max(dat$dist_border[dat$Province==province]), length.out = 100), # range in province
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_border and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_border","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of dist_border)  
mean.df <- compred %>% group_by(dist_border) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(dist_border = compred_wide$dist_border,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="dist_border")

return(mean.df)
  
}
ProvMean.provCap <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_provCap = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_provCap = seq(min(dat$dist_provCap[dat$Province==province]),
                                       max(dat$dist_provCap[dat$Province==province]), length.out = 100), # range in province
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_provCap and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_provCap","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of dist_provCap)  
mean.df <- compred %>% group_by(dist_provCap) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(dist_provCap = compred_wide$dist_provCap,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="dist_provCap")

return(mean.df)
  
}
ProvMean.elc     <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(elc = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(elc = c("1","0"),
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of elc and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("elc","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of elc)  
mean.df <- compred %>% group_by(elc) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(elc = compred_wide$elc,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="elc")

return(mean.df)
  
}
ProvMean.PA      <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(PA = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(PA = c("1","0"),
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of PA and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("PA","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
  }
  
# get the mean prediction for the province (i.e. mean of all communes for a given value of PA)  
mean.df <- compred %>% group_by(PA) %>% summarise_at(vars(pred),mean) %>% 
            mutate(Province = province)

# get the 2.5 and 97.5 quantiles
compred_wide <- pivot_wider(compred, names_from = commune, values_from = pred) 
lnth <- ncol(compred_wide)
quants <- data.frame(apply(compred_wide[ ,3:lnth], 1, quantile, probs=c(0.025,0.975)))

quants.vec <- data.frame(PA = compred_wide$PA,
                         Q2.5 = as.numeric(quants[1,]),
                         Q97.5 = as.numeric(quants[2,]))

# join together
mean.df <- left_join(mean.df, quants.vec, by="PA")

return(mean.df)
  
}

## output dataframe with province mean and all commune predictions
ProvMeanLine.popden  <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(pop_den = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(pop_den = seq(min(dat$pop_den[dat$Province==province]),
                                       max(dat$pop_den[dat$Province==province]), length.out = 100), # range in province
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of pop_den and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("pop_den","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of pop_den)  
    mean.df <- compred %>% group_by(pop_den) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}
ProvMeanLine.elev    <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(mean_elev = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(mean_elev = seq(min(dat$mean_elev[dat$Province==province]),
                                       max(dat$mean_elev[dat$Province==province]), length.out = 100), # range in province
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of mean_elev and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("mean_elev","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of mean_elev)  
    mean.df <- compred %>% group_by(mean_elev) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}
ProvMeanLine.border  <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_border = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_border = seq(min(dat$dist_border[dat$Province==province]),
                                       max(dat$dist_border[dat$Province==province]), length.out = 100), # range in province
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_border and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_border","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of dist_border)  
    mean.df <- compred %>% group_by(dist_border) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}
ProvMeanLine.provCap <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(dist_provCap = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    newdat <- data.frame(dist_provCap = seq(min(dat$dist_provCap[dat$Province==province]),
                                       max(dat$dist_provCap[dat$Province==province]), length.out = 100), # range in province
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         mean_elev = mean(dat$mean_elev[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of dist_provCap and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("dist_provCap","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of dist_provCap)  
    mean.df <- compred %>% group_by(dist_provCap) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}
# I don't need functions for elc and PA because the above qunatile functions output are better for plotting (use the quantiles to plot error bars, rather than lots of faded dots)


### Function to run the above functions (quantiles and lines) for all provinces
RunFun <- function(dat,fun,model){
  
  
# create list of province names
provs <- as.character(unique(dat$Province))

# initialise list
output.list <- list()

# loop through list of provinces, applying the function to each one
for(i in 1:length(provs)){
  
  df <- fun(province=provs[i], model=model)
  output.list[[i]] <- df
}

# name list elements
provname <- sub(" ","_", provs)
names(output.list) <- provname

# extract list elements
list2env(output.list, envir = environment())


# rbind
output.df <- rbind(Banteay_Meanchey,Battambang,Kampong_Cham,Kampong_Chhnang,Kampong_Speu,Kampong_Thom,
                         Kampot,Kandal,Koh_Kong,Kracheh,Mondul_Kiri,Phnom_Penh,
                         Preah_Vihear,Prey_Veng,Pursat,Ratanak_Kiri,Siem_Reap,Preah_Sihanouk,
                         Stung_Treng,Svay_Rieng,Takeo,Otdar_Meanchey,Kep,Pailin)

return(output.df)

}



### Plotting functions

## for line plots with quantile error ribbons. Scales can be either "free" or "fixed"
plotFunQuants <- function(dat,x,group,scales,xlab){
  
ggplot(dat, aes(x=x, y=pred, group=group))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5),fill="grey60", alpha=0.3)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"),
        axis.text = element_text(size=12),
        axis.title = element_text(size=15))+
  facet_wrap(~Province, nrow=6, scales = scales)+
  xlab(xlab)+
  ylab("Predicted forest cover (pixels)")
}

## for point plots with quantile error bars. Scales can be either "free" or "fixed"
plotFunQuants2 <- function(dat,x,group,scales,xlab){

  ggplot(dat, aes(x=x, y=pred, group=group))+
        geom_point(size=2)+
        geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5), width=0.1)+
        theme(panel.background = element_blank(),
              axis.line = element_line(colour = "grey20"),
              axis.title = element_text(size=20),
              axis.text = element_text(size=13))+
        facet_wrap(~Province, nrow=6, scales=scales)+
        xlab(xlab)+
        ylab("Predicted number of forest pixels")
                    
}

## For mean line with faded lines for each communes - NEITHER FUNCTION BELOW IS WORKING
plotFunLines <- function(datmeans,datlines,x,group,scales,xlab,ylab){
  
ggplot(NULL, aes(x=x, y=pred))+
  geom_line(data=datlines[datlines$province!="Phnom Penh",], aes(group=group),  col="grey", size=0.5)+
  geom_line(data=datmeans, col="black", size=1)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"),
        axis.title = element_text(size=20),
        axis.text = element_text(size=13))+
  facet_wrap(~province, nrow=6, scales = scales)+
  #ylim(ylim)+
  xlab(xlab)+
  ylab(ylab)
    
}
plotFunLines <- function(dat,x,group,scales,xlab,ylab){
  
ggplot(NULL)+
  geom_line(data=dat[dat$commune!="mean",], aes(x=x, y=pred, group=group), col="grey", size=0.5)+
  geom_line(data=dat[dat$commune=="mean",], aes(x=x, y=pred, group=group), col="black", size=1)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"),
        axis.title = element_text(size=20),
        axis.text = element_text(size=13))+
  facet_wrap(~province, nrow=6, scales = scales)+
  #ylim(ylim)+
  xlab(xlab)+
  ylab(ylab)
    
}
# Can't get these to work so will just plot manually

pop_den_plot <- plotFunQuants(pop_den_quants, pop_den_quants$pop_den, pop_den_quants$Province, "free",
                              "Population density (scaled)","Predicted forest cover (pixels)")





ggplot(pop_den_lines, aes(x=pop_den, y=pred, group=commune))+
  geom_line(data=pop_den_lines[pop_den_lines$commune!="mean",],  col="grey", size=0.5)+
  geom_line(data=pop_den_lines[pop_den_lines$commune=="mean",], col="black", size=1)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"),
        axis.title = element_text(size=20),
        axis.text = element_text(size=13))+
  facet_wrap(~province, nrow=6, scales = "free")
  #ylim(ylim)+
  #xlab(xlab)+
  #ylab(ylab)


#
        # Predictions ####

# Below I will produce the predictions and save them all as .csv's so that I don't need to repeatedly run them

## population density 
pop_den_quants <- RunFun(dat1,ProvMean.popden,sat9c)
pop_den_lines  <- RunFun(dat1, ProvMeanLine.popden, sat9c)
write.csv(pop_den_quants,"Results/Socioeconomics/sat9c_predictions/Province/pop_den_quants.csv")
write.csv(pop_den_lines,"Results/Socioeconomics/sat9c_predictions/Province/pop_den_lines.csv")

## mean elevation
mean_elev_quants <- RunFun(dat1, ProvMean.elev, sat9c)
mean_elev_lines  <- RunFun(dat1, ProvMeanLine.elev, sat9c)
write.csv(mean_elev_quants,"Results/Socioeconomics/sat9c_predictions/Province/mean_elev_quants.csv")
write.csv(mean_elev_lines,"Results/Socioeconomics/sat9c_predictions/Province/mean_elev_lines.csv")

## distance to border
dist_border_quants <- RunFun(dat1, ProvMean.border, sat9c)
dist_border_lines  <- RunFun(dat1, ProvMeanLine.border, sat9c)
write.csv(dist_border_quants,"Results/Socioeconomics/sat9c_predictions/Province/dist_border_quants.csv")
write.csv(dist_border_lines,"Results/Socioeconomics/sat9c_predictions/Province/dist_border_lines.csv")

## distance to provincial capital
dist_provCap_quants <- RunFun(dat1, ProvMean.provCap, sat9c)
dist_provCap_lines  <- RunFun(dat1, ProvMeanLine.provCap, sat9c)
write.csv(dist_provCap_quants,"Results/Socioeconomics/sat9c_predictions/Province/dist_provCap_quants.csv")
write.csv(dist_provCap_lines,"Results/Socioeconomics/sat9c_predictions/Province/dist_provCap_lines.csv")

## ELC
elc_quants <- RunFun(dat1, ProvMean.elc, sat9c)
write.csv(elc_quants,"Results/Socioeconomics/sat9c_predictions/Province/elc_quants.csv")

## PA
PA_quants <- RunFun(dat1, ProvMean.PA, sat9c)
write.csv(PA_quants,"Results/Socioeconomics/sat9c_predictions/Province/PA_quants.csv")




#
        # Plots ####

### Load dataframes
pop_den_quants      <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/pop_den_quants.csv")
pop_den_lines       <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/pop_den_lines.csv")
mean_elev_quants    <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/mean_elev_quants.csv")
mean_elev_lines     <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/mean_elev_lines.csv")
dist_border_quants  <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/dist_border_quants.csv")
dist_border_lines   <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/dist_border_lines.csv")
dist_provCap_quants <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/dist_provCap_quants.csv")
dist_provCap_lines  <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/dist_provCap_lines.csv")
elc_quants          <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/elc_quants.csv")
elc_quants$elc      <- as.factor(elc_quants$elc) 
PA_quants           <- read.csv("Results/Socioeconomics/sat9c_predictions/Province/PA_quants.csv")
PA_quants$PA        <- as.factor(PA_quants$PA)

### Mean and quantile plots 
pop_den_quants_p      <- plotFunQuants(pop_den_quants, pop_den_quants$pop_den, "Province", "free",
                                  "Population density (scaled)")
ggsave("Results/Socioeconomics/Plots/population_density/m1_popdenProv_free.png", pop_den_quants_p,
       width = 30, height = 30, unit="cm", dpi=300)


mean_elev_quants_p    <- plotFunQuants(mean_elev_quants, mean_elev_quants$mean_elev, "Province", "free",
                                    "Mean elevation (scaled)")

dist_border_quants_p  <- plotFunQuants(dist_border_quants,dist_border_quants$dist_border,"Province","free",
                                      "Distance to international border (scaled)")

dist_provCap_quants_p <- plotFunQuants(dist_provCap_quants, dist_provCap_quants$dist_provCap, "Province",
                                       "free","Distance to international border (scaled)")

elc_quants_p          <- plotFunQuants2(elc_quants, elc_quants$elc, "Province", "free",
                                      "Presence of economic land concessions")

ggsave("Results/Socioeconomics/Plots/elc/sat9c_elc_quants_free.png",elc_quants_p,
       width = 25, height=25, units="cm", dpi=300)


PA_quants_p           <- plotFunQuants2(PA_quants, PA_quants$PA, "Province", "free",
                                      "Presence of protected areas")

ggsave("Results/Socioeconomics/Plots/PA/sat9c_PA_quants_free.png",PA_quants_p,
       width = 25, height=25, units="cm", dpi=300)


### mean and commune line plots
pop_den_lines_p <- ggplot(data=NULL,aes(x=pop_den, y=pred, group=commune))+
                    geom_line(data=pop_den_lines[pop_den_lines$commune!="mean",],col="grey", size=0.5)+
                    geom_line(data=pop_den_lines[pop_den_lines$commune=="mean",],col="black",size=1)+
                    theme(panel.background = element_blank(),
                          axis.line = element_line(colour = "grey20"),
                          axis.title = element_text(size=17),
                          axis.text = element_text(size=13))+
                    facet_wrap(~province, nrow=6, scales = "free")+
                    #ylim(0,26000)+
                    xlab("Population density (Centerd and scaled)")+
                    ylab("Predicted number of forest pixels")

ggsave("Results/Socioeconomics/Plots/population_density/m1.popdenProv_lines_free.png",pop_den_lines_p,
       width = 30, height = 30, unit="cm", dpi=300)

mean_elev_lines_p <- ggplot(data=NULL,aes(x=mean_elev, y=pred, group=commune))+
                      geom_line(data=mean_elev_lines[mean_elev_lines$commune!="mean",],col="grey", size=0.5)+
                      geom_line(data=mean_elev_lines[mean_elev_lines$commune=="mean",],col="black",size=1)+
                      theme(panel.background = element_blank(),
                            axis.line = element_line(colour = "grey20"),
                            axis.title = element_text(size=17),
                            axis.text = element_text(size=13))+
                      facet_wrap(~province, nrow=6, scales = "free")+
                      #ylim(0,26000)+
                      xlab("Mean elevation (scaled)")+
                      ylab("Predicted number of forest pixels")

dist_border_lines_p <- ggplot(data=NULL,aes(x=dist_border, y=pred, group=commune))+
                        geom_line(data=dist_border_lines[dist_border_lines$commune!="mean",],col="grey", size=0.5)+
                        geom_line(data=dist_border_lines[dist_border_lines$commune=="mean",],col="black",size=1)+
                        theme(panel.background = element_blank(),
                              axis.line = element_line(colour = "grey20"),
                              axis.title = element_text(size=17),
                              axis.text = element_text(size=13))+
                        facet_wrap(~province, nrow=6, scales = "free")+
                        #ylim(0,26000)+
                        xlab("Distance to international border (scaled)")+
                        ylab("Predicted number of forest pixels")

dist_provCap_lines_p <- ggplot(data=NULL,aes(x=dist_provCap, y=pred, group=commune))+
                        geom_line(data=dist_provCap_lines[dist_provCap_lines$commune!="mean",],col="grey", size=0.5)+
                        geom_line(data=dist_provCap_lines[dist_provCap_lines$commune=="mean",],col="black",size=1)+
                        theme(panel.background = element_blank(),
                              axis.line = element_line(colour = "grey20"),
                              axis.title = element_text(size=17),
                              axis.text = element_text(size=13))+
                        facet_wrap(~province, nrow=6, scales = "free")+
                        #ylim(0,26000)+
                        xlab("Distance to provincial capital (scaled)")+
                        ylab("Predicted number of forest pixels")





### Here I am just exploring the variation in individual communes

# plot pop_den effects for just Koh Kong
Koh_kong <- pop_den_lines %>% filter(province=="Koh Kong")

ggplot(data=NULL,aes(x=pop_den, y=pred, group=commune))+
                    geom_line(data=Koh_kong[Koh_kong$commune!="mean",],col="grey", size=0.5)+
                    geom_line(data=Koh_kong[Koh_kong$commune=="mean",],col="black",size=1)+
                    theme(panel.background = element_blank(),
                          axis.line = element_line(colour = "grey20"),
                          axis.title = element_text(size=17),
                          axis.text = element_text(size=13))+
                    #facet_wrap(~province, nrow=6, scales = "free")+
                    #ylim(0,26000)+
                    xlab("Population density (Centerd and scaled)")+
                    ylab("Predicted number of forest pixels")

# identify communes in Koh Kong with largest intercepts
sat9c.ranef <- ranef(sat9c)[[1]]
sat9c.ranef <- sat9c.ranef[order(sat9c.ranef[,"(Intercept)"], decreasing = TRUE), ]
head(sat9c.ranef)
sat9c.ranef$Provcomm <- row.names(sat9c.ranef)
sat9c.ranef <- sat9c.ranef %>% separate(col=Provcomm, into=c("Provcomm","Province"), sep=":")
KohKong.ranef <- sat9c.ranef %>% filter(Province=="Koh Kong")
KohKong.ranef <- KohKong.ranef %>% separate(Provcomm, c("province","commune"), sep="_")
KohKong.ranef <- KohKong.ranef[ ,-5]
KK_top10 <- KohKong.ranef[1:10,] 

KK_top10_plot <- Koh_kong %>% filter(commune %in% KK_top10$commune)

# plot the 10 communes in Koh Kong with the largest intercepts
ggplot(data=NULL,aes(x=pop_den, y=pred, group=commune))+
                    geom_line(data=KK_top10_plot,col="grey", size=0.5)+
                    #geom_line(data=Koh_kong[Koh_kong$commune=="mean",],col="black",size=1)+
                    theme(panel.background = element_blank(),
                          axis.line = element_line(colour = "grey20"),
                          axis.title = element_text(size=17),
                          axis.text = element_text(size=13))+
                    #facet_wrap(~province, nrow=6, scales = "free")+
                    #ylim(0,26000)+
                    xlab("Population density (Centerd and scaled)")+
                    ylab("Predicted number of forest pixels")





### Here I am following an idea from Jeroen - to add the observed data onto the plots of provincial predictions

# extract ForPix and elevation data to be added to the plot
elev_pts <- dat1 %>% select(Province,Commune,ForPix,mean_elev) %>% 
              rename(commune=Commune,pred=ForPix,province=Province)

# plot
ggplot(data=NULL,aes(x=mean_elev, y=pred, group=commune))+
  geom_point(data=elev_pts, shape=1)+
  geom_line(data=mean_elev_lines[mean_elev_lines$commune!="mean",],col="grey", size=0.5)+
  geom_line(data=mean_elev_lines[mean_elev_lines$commune=="mean",],col="black",size=1)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"),
        axis.title = element_text(size=17),
        axis.text = element_text(size=13))+
  facet_wrap(~province, nrow=6, scales = "free")+
  #ylim(0,26000)+
  xlab("Mean elevation (scaled)")+
  ylab("Predicted number of forest pixels")

# That plot shows how poorly the model is predicting for communes.  Basically, because the random effects estimates are taken from ALL communes and provinces, and they are used in each commune-specific prediction, the predictions are not particularly good for specific communes.

# Another suggestion from Jeroen was to reduce the x axis, so to reduce the "extremes" in the predictors. This will mean the model only has to predict for predictor values that are closer to the "global mean", and thus should make predictions more accurate.  

# I will take the centre 50% of mean_elev (i.e. cut off the smallest 25% and largest 25%)
elev_quants25 <- quantile(dat1$mean_elev,probs=(0.25))
elev_quants75 <- quantile(dat1$mean_elev,probs=(0.75))

# quantiles incorporated into the function (as need to do for each province, within the loop)
ProvMeanLine.elev.red    <- function(dat=dat1,province, model){
  
  # extract list of communes 
  communes <- unique(dat$Provcomm[dat$Province==province])
  
  # Initialise empty dataframe
  compred <- data.frame(mean_elev = NULL,
                        pred = NULL,
                        commune = NULL,
                        province = NULL)
  
  # loop through list of communes and predict for each one, and attach results into dataframe
  for(i in 1:length(communes)){
    
    # get 25 and 75% quantiles for mean_elev from that Province
    quant25 <- quantile(dat$mean_elev[dat$Province==province], probs = 0.25)
    quant75 <- quantile(dat$mean_elev[dat$Province==province], probs = 0.75)
    
    # newdata
    newdat <- data.frame(mean_elev = seq(quant25,quant75, length.out = 100), # centre 50% range in province
                         pop_den = mean(dat$pop_den[dat$Province==province]),
                         dist_border = mean(dat$dist_border[dat$Province==province]),
                         dist_provCap = mean(dat$dist_provCap[dat$Province==province]),
                         elc = "0",
                         PA = "0",
                         areaKM = dat$areaKM[dat$Provcomm==communes[i]][1],
                         year = mean(dat$year[dat$Province==province]),
                         Province = province,
                         Provcomm = communes[i])
    newdat$pred <- as.vector(predict(model, type="response",newdata=newdat, re.form=~(year|Province/Provcomm)))
    
    # pull out values of mean_elev and the predictions, and attach commune and province name. 
    df <- newdat[ ,c("mean_elev","pred")]
    split <- colsplit(newdat$Provcomm, pattern="_", names=c("Province", "Commune"))
    comname <- split[1,2]
    provname <- split[1,1]
    df$commune <- comname 
    df$province <- provname
    compred <- rbind(compred,df)
    
    
    
  }
  
  # get the mean prediction for the province (i.e. mean of all communes for a given value of mean_elev)  
    mean.df <- compred %>% group_by(mean_elev) %>% summarise_at(vars(pred),mean) %>% 
                mutate(commune = "mean")  %>% mutate(province = province) 
    
    # attach mean to commune df
    compred <- rbind(compred,mean.df)
    
    return(compred)
}


mean_elev_red <- RunFun(dat1, ProvMeanLine.elev.red, sat9c)


ggplot(data=NULL,aes(x=mean_elev, y=pred, group=commune))+
  geom_point(data=elev_pts, shape=1)+
  geom_line(data=mean_elev_red[mean_elev_red$commune!="mean",],col="grey", size=0.5)+
  geom_line(data=mean_elev_red[mean_elev_red$commune=="mean",],col="black",size=1)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"),
        axis.title = element_text(size=17),
        axis.text = element_text(size=13))+
  facet_wrap(~province, nrow=6, scales = "free")+
  #ylim(0,26000)+
  xlab("Mean elevation (scaled)")+
  ylab("Predicted number of forest pixels")



#
### Analysis at Provincial level ####
  ## Prepare data ####


# load dat2 - aggregated to the province level and scaled
dat2 <- read.csv("Data/commune/dat2.csv", header=T, stringsAsFactors = T)
str(dat2)
dat2 <- dat2[ ,-1]


# no need to do the below, just load dat2 above


# load raw data (dat rather than dat1)
dat <- read.csv("Data/commune/dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

str(dat)

## I am going to aggregate all data up to the provinicial level

## The following variables need to be summed:
  # areaKM, ForPix, diffPix, tot_pop, land_confl, Pax_migt_in, Pax_migt_out

## The following variables need to be averaged:
  # prop_ind, pop_den, M6_24_sch, propPrimSec, propSecSec, Les1_R_Land, pig_fam, dist_sch, garbage, KM_Comm, 
  # crim_case, mean_elev, dist_border, dist_provCap


# sum
dat_sum <- dat %>% 
           select(year,Province,areaKM,ForPix,diffPix,tot_pop,land_confl,Pax_migt_in,Pax_migt_out) %>%  
           group_by(Province, year) %>% 
           summarise_at(c("areaKM", "ForPix", "diffPix", "tot_pop", "land_confl", "Pax_migt_in",
                          "Pax_migt_out"), sum) 
                   
# mean
dat_mean <- dat %>% 
            select(year,Province, prop_ind, pop_den, M6_24_sch, propPrimSec, propSecSec, Les1_R_Land, 
                   pig_fam, dist_sch, garbage, KM_Comm, crim_case, mean_elev, dist_border, dist_provCap) %>% 
            group_by(Province,year) %>% 
            summarise_at(c("prop_ind", "pop_den", "M6_24_sch", "propPrimSec", "propSecSec", "Les1_R_Land",
                           "pig_fam", "dist_sch", "garbage", "KM_Comm", "crim_case", "mean_elev", 
                           "dist_border", "dist_provCap"), mean)


# join
dat2 <- full_join(dat_sum, dat_mean, by=c("Province","year"))

# save unscaled provincial-level data
write.csv(dat2, file="Data/commune/dat_prov.csv")


# scale
dat2 <- dat2 %>% mutate_at(c("year","tot_pop", "land_confl", "Pax_migt_in","Pax_migt_out","prop_ind", 
                             "pop_den", "M6_24_sch", "propPrimSec", "propSecSec","Les1_R_Land", 
                             "pig_fam", "dist_sch", "garbage", "KM_Comm", "crim_case", "mean_elev", 
                             "dist_border", "dist_provCap"), ~(scale(.) %>% as.vector))

# elc
elc_prov <- unique(dat$Province[dat$elc=="1"])
dat2$elc <- ifelse(dat2$Province %in% elc_prov, "1", "0")
unique(dat2$Province[dat2$elc=="1"])

# PA
PA_prov <- unique(dat$Province[dat$PA=="1"])
dat2$PA <- ifelse(dat2$Province %in% PA_prov, "1", "0")
unique(dat2$Province[dat2$PA=="1"])

# deal with NaNs (produced when trying to scale vectors of 0). Prop_ind is the only var with the problem
sum(is.nan(dat2$prop_ind))

dat2$prop_ind <- ifelse(is.nan(dat2$prop_ind), 0, dat2$prop_ind)


# save
write.csv(dat2, file="Data/commune/dat2.csv")

#
  ## Exploratory plots ####


ggplot(dat2, aes(x=tot_pop, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=land_confl, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=Pax_migt_in, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=Pax_migt_out, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=prop_ind, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=pop_den, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=M6_24_sch, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=propPrimSec, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=propSecSec, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=Les1_R_Land, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=pig_fam, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=dist_sch, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=garbage, y=ForPix))+
  geom_point

ggplot(dat2, aes(x=KM_Comm, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=crim_case, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=mean_elev, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=dist_border, y=ForPix))+
  geom_point()

ggplot(dat2, aes(x=dist_provCap, y=ForPix))+
  geom_point()

par(mfrow=c(3,3))
hist(dat2$tot_pop)
hist(dat2$land_confl)
hist(dat2$Pax_migt_in)
hist(dat2$Pax_migt_out)
hist(dat2$prop_ind)
hist(dat2$pop_den)
hist(dat2$M6_24_sch)
hist(dat2$propPrimSec)
hist(dat2$propSecSec)

hist(dat2$Les1_R_Land)
hist(dat2$pig_fam)
hist(dat2$dist_sch)
hist(dat2$garbage)
hist(dat2$KM_Comm)
hist(dat2$crim_case)
hist(dat2$mean_elev)
hist(dat2$dist_border)
hist(dat2$dist_provCap)

# Looking at the histograms, most of the variables appear to have two peaks.  This suggests that perhaps there are two "groups" of provinces for each variable. Perhaps binomial models would be a better approach? 


#
  ## Models ####
    # Continous ####

# First I will try models with a Poisson distribution like I did above for the commune level models. Not sure they will work that well based on the histograms above, but worth trying.

# first I will just run models in the individual sets and do global predictions for each model. Then I will plot them with the raw point, as suggested by Jeroen

popdem.m1 <- glmer(ForPix ~ prop_ind + pop_den + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

edu.m1 <- glmer(ForPix ~ M6_24_sch + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

emp.m1 <- glmer(ForPix ~ propPrimSec + propSecSec + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

econ.m1 <- glmer(ForPix ~ Les1_R_Land + pig_fam + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

acc.m1 <- glmer(ForPix ~ dist_sch + garbage + KM_Comm + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

soc.m1 <- glmer(ForPix ~ crim_case + land_confl + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

mig.m1 <- glmer(ForPix ~ Pax_migt_in + Pax_migt_out + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

elev.m1 <- glmer(ForPix ~ mean_elev + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")

hum.m1 <- glmer(ForPix ~ dist_border + dist_provCap + elc + PA + offset(log(areaKM)) + (year|Province), 
                   data=dat2, family="poisson")


### predicting

# pop_den
pop_den.pred <- data.frame(pop_den = seq(min(dat2$pop_den), max(dat2$pop_den), length.out=100),
                           prop_ind = mean(dat2$prop_ind, na.rm=TRUE),
                           areaKM = mean(dat2$areaKM))
pop_den.pred$pred <- as.vector(predict(popdem.m1, type="response", newdata=pop_den.pred, re.form=NA))

pop_den.plot <- ggplot(pop_den.pred, aes(x=pop_den, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=pop_den, y=ForPix))+
                theme_classic()
                

# prop_ind
prop_ind.pred <- data.frame(prop_ind = seq(min(dat2$prop_ind), max(dat2$prop_ind), length.out=100),
                           pop_den = mean(dat2$pop_den, na.rm=TRUE),
                           areaKM = mean(dat2$areaKM))
prop_ind.pred$pred <- as.vector(predict(popdem.m1, type="response", newdata=prop_ind.pred, re.form=NA))

prop_ind.plot <- ggplot(prop_ind.pred, aes(x=prop_ind, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=prop_ind, y=ForPix))+
                theme_classic()


# M6_24_sch
edu.pred <- data.frame(M6_24_sch = seq(min(dat2$M6_24_sch), max(dat2$M6_24_sch), length.out=100),
                           areaKM = mean(dat2$areaKM))
edu.pred$pred <- as.vector(predict(edu.m1, type="response", newdata=edu.pred, re.form=NA))

edu.plot <- ggplot(edu.pred, aes(x=M6_24_sch, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=M6_24_sch, y=ForPix))+
                theme_classic()


# propPrimSec
primSec.pred <- data.frame(propPrimSec = seq(min(dat2$propPrimSec), max(dat2$propPrimSec), length.out=100),
                           propSecSec = mean(dat2$propSecSec, na.rm=TRUE),
                           areaKM = mean(dat2$areaKM))
primSec.pred$pred <- as.vector(predict(emp.m1, type="response", newdata=primSec.pred, re.form=NA))

primSec.plot <- ggplot(primSec.pred, aes(x=propPrimSec, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=propPrimSec, y=ForPix))+
                theme_classic()


# propSecSec
secSec.pred <- data.frame(propSecSec = seq(min(dat2$propSecSec), max(dat2$propSecSec), length.out=100),
                           propPrimSec = mean(dat2$propPrimSec, na.rm=TRUE),
                           areaKM = mean(dat2$areaKM))
secSec.pred$pred <- as.vector(predict(emp.m1, type="response", newdata=secSec.pred, re.form=NA))

secSec.plot <- ggplot(secSec.pred, aes(x=propSecSec, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=propSecSec, y=ForPix))+
                theme_classic()

# Les1_R_Land
rice.pred <- data.frame(Les1_R_Land = seq(min(dat2$Les1_R_Land), max(dat2$Les1_R_Land), length.out=100),
                           pig_fam = mean(dat2$pig_fam, na.rm=TRUE),
                           areaKM = mean(dat2$areaKM))
rice.pred$pred <- as.vector(predict(econ.m1, type="response", newdata=rice.pred, re.form=NA))

rice.plot <- ggplot(rice.pred, aes(x=Les1_R_Land, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=Les1_R_Land, y=ForPix))+
                theme_classic()


# pig_fam
pig.pred <- data.frame(pig_fam = seq(min(dat2$pig_fam), max(dat2$pig_fam), length.out=100),
                           Les1_R_Land = mean(dat2$Les1_R_Land, na.rm=TRUE),
                           areaKM = mean(dat2$areaKM))
pig.pred$pred <- as.vector(predict(econ.m1, type="response", newdata=pig.pred, re.form=NA))

pig.plot <- ggplot(pig.pred, aes(x=pig_fam, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=pig_fam, y=ForPix))+
                theme_classic()



# dist_sch
distSch.pred <- data.frame(dist_sch = seq(min(dat2$dist_sch), max(dat2$dist_sch), length.out=100),
                       KM_Comm = mean(dat2$KM_Comm, na.rm=TRUE),
                       garbage = mean(dat2$garbage),      
                       areaKM = mean(dat2$areaKM))
distSch.pred$pred <- as.vector(predict(acc.m1, type="response", newdata=distSch.pred, re.form=NA))

distSch.plot <- ggplot(distSch.pred, aes(x=dist_sch, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=dist_sch, y=ForPix))+
                theme_classic()

# KM_Comm
KMcomm.pred <- data.frame(KM_Comm = seq(min(dat2$KM_Comm), max(dat2$KM_Comm), length.out=100),
                       dist_sch = mean(dat2$dist_sch, na.rm=TRUE),
                       garbage = mean(dat2$garbage),      
                       areaKM = mean(dat2$areaKM))
KMcomm.pred$pred <- as.vector(predict(acc.m1, type="response", newdata=KMcomm.pred, re.form=NA))

KMcomm.plot <- ggplot(KMcomm.pred, aes(x=KM_Comm, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=KM_Comm, y=ForPix))+
                theme_classic()

# garbage
garbage.pred <- data.frame(garbage = seq(min(dat2$garbage), max(dat2$garbage), length.out=100),
                       dist_sch = mean(dat2$dist_sch, na.rm=TRUE),
                       KM_Comm = mean(dat2$KM_Comm),      
                       areaKM = mean(dat2$areaKM))
garbage.pred$pred <- as.vector(predict(acc.m1, type="response", newdata=garbage.pred, re.form=NA))

garbage.plot <- ggplot(garbage.pred, aes(x=garbage, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=garbage, y=ForPix))+
                theme_classic()

# crim_case
crim.pred <- data.frame(crim_case = seq(min(dat2$crim_case), max(dat2$crim_case), length.out=100),
                       land_confl = mean(dat2$land_confl),      
                       areaKM = mean(dat2$areaKM))
crim.pred$pred <- as.vector(predict(soc.m1, type="response", newdata=crim.pred, re.form=NA))

crim.plot <- ggplot(crim.pred, aes(x=crim_case, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=crim_case, y=ForPix))+
                theme_classic()


# land_confl
confl.pred <- data.frame(land_confl = seq(min(dat2$land_confl), max(dat2$land_confl), length.out=100),
                       crim_case = mean(dat2$crim_case),      
                       areaKM = mean(dat2$areaKM))
confl.pred$pred <- as.vector(predict(soc.m1, type="response", newdata=confl.pred, re.form=NA))

confl.plot <- ggplot(confl.pred, aes(x=land_confl, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=land_confl, y=ForPix))+
                theme_classic()


# Pax_migt_in
migtIn.pred <- data.frame(Pax_migt_in = seq(min(dat2$Pax_migt_in), max(dat2$Pax_migt_in), length.out=100),
                       Pax_migt_out = mean(dat2$Pax_migt_out),      
                       areaKM = mean(dat2$areaKM))
migtIn.pred$pred <- as.vector(predict(mig.m1, type="response", newdata=migtIn.pred, re.form=NA))

migtIn.plot <- ggplot(migtIn.pred, aes(x=Pax_migt_in, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=Pax_migt_in, y=ForPix))+
                theme_classic()


# Pax_migt_out
migtOut.pred <- data.frame(Pax_migt_out = seq(min(dat2$Pax_migt_out), max(dat2$Pax_migt_out), length.out=100),
                       Pax_migt_in = mean(dat2$Pax_migt_in),      
                       areaKM = mean(dat2$areaKM))
migtOut.pred$pred <- as.vector(predict(mig.m1, type="response", newdata=migtOut.pred, re.form=NA))

migtOut.plot <- ggplot(migtOut.pred, aes(x=Pax_migt_out, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=Pax_migt_out, y=ForPix))+
                theme_classic()


# mean_elev
elev.pred <- data.frame(mean_elev = seq(min(dat2$mean_elev), max(dat2$mean_elev), length.out=100),
                       areaKM = mean(dat2$areaKM))
elev.pred$pred <- as.vector(predict(elev.m1, type="response", newdata=elev.pred, re.form=NA))

elev.plot <- ggplot(elev.pred, aes(x=mean_elev, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=mean_elev, y=ForPix))+
                theme_classic()


# dist_border
bord.pred <- data.frame(dist_border = seq(min(dat2$dist_border), max(dat2$dist_border), length.out=100),
                        dist_provCap = mean(dat2$dist_provCap),
                        elc = 0,
                        PA = 0,
                        areaKM = mean(dat2$areaKM))
bord.pred$pred <- as.vector(predict(hum.m1, type="response", newdata=bord.pred, re.form=NA))

bord.plot <- ggplot(bord.pred, aes(x=dist_border, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=dist_border, y=ForPix))+
                theme_classic()


# dist_provCap
provCap.pred <- data.frame(dist_provCap = seq(min(dat2$dist_provCap), max(dat2$dist_provCap), length.out=100),
                        dist_border = mean(dat2$dist_border),
                        elc = 0,
                        PA = 0,
                        areaKM = mean(dat2$areaKM))
provCap.pred$pred <- as.vector(predict(hum.m1, type="response", newdata=provCap.pred, re.form=NA))

provCap.plot <- ggplot(provCap.pred, aes(x=dist_provCap, y=pred))+
                geom_line()+
                geom_point(data=dat2, aes(x=dist_provCap, y=ForPix))+
                theme_classic()


# elc
elc.pred <- data.frame(elc = c(1,0),
                        dist_border = mean(dat2$dist_border),
                        dist_provCap = mean(dat2$dist_provCap),
                        PA = 0,
                        areaKM = mean(dat2$areaKM))
elc.pred$pred <- as.vector(predict(hum.m1, type="response", newdata=elc.pred, re.form=NA))

elc.plot <- ggplot(elc.pred, aes(x=elc, y=pred))+
                geom_point()+
                geom_point(data=dat2, aes(x=elc, y=ForPix))+
                theme_classic()


# PA
PA.pred <- data.frame(PA = c(1,0),
                        dist_border = mean(dat2$dist_border),
                        dist_provCap = mean(dat2$dist_provCap),
                        elc = 0,
                        areaKM = mean(dat2$areaKM))
PA.pred$pred <- as.vector(predict(hum.m1, type="response", newdata=PA.pred, re.form=NA))

PA.plot <- ggplot(PA.pred, aes(x=PA, y=pred))+
                geom_point()+
                geom_point(data=dat2, aes(x=PA, y=ForPix))+
                theme_classic()




#
      # plots continous ####

prov_plot_cont <- pop_den.plot + prop_ind.plot + edu.plot + primSec.plot + secSec.plot + rice.plot +
                  pig.plot + distSch.plot + KMcomm.plot + garbage.plot + crim.plot + confl.plot +
                  migtIn.plot + migtOut.plot + elev.plot + bord.plot + provCap.plot 


## change y axis titles for plots 1, 6, 11, and 16 and remove all others

# plots to have y axis titles 
y <- c(1,6,11,16)

# plots to remove y axis titles
yn <- c(2:5,7:10,12:15,17)

for(i in y){
  prov_plot_cont[[i]] <- prov_plot_cont[[i]] + ylab("Forest cover (pixels)")
}

for(i in yn){
  prov_plot_cont[[i]] <- prov_plot_cont[[i]] + theme(axis.title.y = element_blank())
}



# remove y axis text except for 1, 6, 11, 16

# plots to remove ticks
yn <- c(2:5,7:10,12:15,17)

for(i in yn){
  prov_plot_cont[[i]] <- prov_plot_cont[[i]] + theme(axis.text.y = element_blank())
}


# change line color to red and increase size
n <- 1:17
for(i in n){
  prov_plot_cont[[i]] <- prov_plot_cont[[i]] + geom_line(size=2, color="red")
}


# add x axis labels instead of plot titles
prov_plot_cont[[1]] <- prov_plot_cont[[1]] + xlab("Population density")
prov_plot_cont[[2]] <- prov_plot_cont[[2]] + xlab("Proportion indigenous")
prov_plot_cont[[3]] <- prov_plot_cont[[3]] + xlab("Proportion (16-24) in school")
prov_plot_cont[[4]] <- prov_plot_cont[[4]] + xlab("Proportion employed in primary sector")
prov_plot_cont[[5]] <- prov_plot_cont[[5]] + xlab("Proportion employed in secondary sector")
prov_plot_cont[[6]] <- prov_plot_cont[[6]] + xlab("Proportion with no farmland")
prov_plot_cont[[7]] <- prov_plot_cont[[7]] + xlab("Proportion with pigs")
prov_plot_cont[[8]] <- prov_plot_cont[[8]] + xlab("Distance to school (km)")
prov_plot_cont[[9]] <- prov_plot_cont[[9]] + xlab("Distance to Commune office (km)")
prov_plot_cont[[10]] <- prov_plot_cont[[10]] + xlab("Proportion with waste collection")
prov_plot_cont[[11]] <- prov_plot_cont[[11]] + xlab("Crime per capita")
prov_plot_cont[[12]] <- prov_plot_cont[[12]] + xlab("Land conflict cases")
prov_plot_cont[[13]] <- prov_plot_cont[[13]] + xlab("Number of in-migrants")
prov_plot_cont[[14]] <- prov_plot_cont[[14]] + xlab("Number of out-migrants")
prov_plot_cont[[15]] <- prov_plot_cont[[15]] + xlab("Mean elevation (m)")
prov_plot_cont[[16]] <- prov_plot_cont[[16]] + xlab("Distance to Intl border (km)")
prov_plot_cont[[17]] <- prov_plot_cont[[17]] + xlab("Distance to Provincial capital (km)")



# change font size of titles
n <- 1:17
for(i in n){
  prov_plot_cont[[i]] <- prov_plot_cont[[i]] + theme(plot.title = element_text(size=10))
}


# change font size of x axis labels
n <- 1:17
for(i in n){
  prov_plot_cont[[i]] <- prov_plot_cont[[i]] + theme(axis.title.x = element_text(size=10))
}


ggsave("Results/Socioeconomics/Plots/Province_level/prov_level_cont.png", prov_plot_cont,
       width = 30, height = 30, units = "cm", dpi=300)


  
    # Categorical ####

# None of the above models showed any effect at all. But based on the histograms from the above exploratory section, many of the variables have two "peaks". In other words, there are two distinct "types" of province when it comes to certain variables, and certain provinces are clumped together based on those variables. Therefore it may be interesting to turn some of the variables into categorical variables. 

# the variables I will try this with (based on histograms) are: land_confl, Pax_migt_in, Pax_migt_out, 
# pop_den, M6_24_sch, propPrimSec, propSecSec, Les1_R_Land, pig_fam, dist_sch, crim_case, mean_elev, 
# dist_border, dist_provCap


      # data preparation ####

# no need to do the below. Just load dat_cat in model section below


### The way I will do this for now is to just split the vars by their means

# function that pulls out the variable and assigns it into either the low category or the high category depending on which side of the mean it sits
split.fun <- function(province,var,dat){
  
  df <- dat[dat$Province==province, c("Province", "year", var)]
  
  vars <- dat[ ,var]
  av <- mean(vars) 
  df$cat <- ifelse(df[,3] < av, "low", "high")
  
  return(df)
  
  
}

# list of provinces
provs <- unique(dat2$Province)


### land_confl

# initialise empty dataframe
land_confl.df <- data.frame(Province = NULL,
                            year = NULL,
                            land_confl = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "land_confl", dat2)
  
  land_confl.df <- rbind(land_confl.df, df)
  
}

# re-name column
land_confl.df <- land_confl.df %>% rename(land_confl.cat = cat)



### Pax_migt_in

# initialise empty dataframe
Pax_migt_in.df <- data.frame(Province = NULL,
                            year = NULL,
                            Pax_migt_in = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "Pax_migt_in", dat2)
  
  Pax_migt_in.df <- rbind(Pax_migt_in.df, df)
  
}

# re-name column
Pax_migt_in.df <- Pax_migt_in.df %>% rename(Pax_migt_in.cat = cat)




### Pax_migt_out

# initialise empty dataframe
Pax_migt_out.df <- data.frame(Province = NULL,
                            year = NULL,
                            Pax_migt_out = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "Pax_migt_out", dat2)
  
  Pax_migt_out.df <- rbind(Pax_migt_out.df, df)
  
}

# re-name column
Pax_migt_out.df <- Pax_migt_out.df %>% rename(Pax_migt_out.cat = cat)




### pop_den

# initialise empty dataframe
pop_den.df <- data.frame(Province = NULL,
                            year = NULL,
                            pop_den = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "pop_den", dat2)
  
  pop_den.df <- rbind(pop_den.df, df)
  
}

# re-name column
pop_den.df <- pop_den.df %>% rename(pop_den.cat = cat)




### M6_24_sch

# initialise empty dataframe
M6_24_sch.df <- data.frame(Province = NULL,
                            year = NULL,
                            M6_24_sch = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "M6_24_sch", dat2)
  
  M6_24_sch.df <- rbind(M6_24_sch.df, df)
  
}

# re-name column
M6_24_sch.df <- M6_24_sch.df %>% rename(M6_24_sch.cat = cat)




### propPrimSec

# initialise empty dataframe
propPrimSec.df <- data.frame(Province = NULL,
                            year = NULL,
                            propPrimSec = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "propPrimSec", dat2)
  
 propPrimSec.df <- rbind(propPrimSec.df, df)
  
}

# re-name column
propPrimSec.df <- propPrimSec.df %>% rename(propPrimSec.cat = cat)




### propSecSec

# initialise empty dataframe
propSecSec.df <- data.frame(Province = NULL,
                            year = NULL,
                            propSecSec = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "propSecSec", dat2)
  
 propSecSec.df <- rbind(propSecSec.df, df)
  
}

# re-name column
propSecSec.df <- propSecSec.df %>% rename(propSecSec.cat = cat)




### Les1_R_Land

# initialise empty dataframe
Les1_R_Land.df <- data.frame(Province = NULL,
                            year = NULL,
                            Les1_R_Land = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "Les1_R_Land", dat2)
  
 Les1_R_Land.df <- rbind(Les1_R_Land.df, df)
  
}

# re-name column
Les1_R_Land.df <- Les1_R_Land.df %>% rename(Les1_R_Land.cat = cat)




### pig_fam

# initialise empty dataframe
pig_fam.df <- data.frame(Province = NULL,
                            year = NULL,
                            pig_fam = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "pig_fam", dat2)
  
 pig_fam.df <- rbind(pig_fam.df, df)
  
}

# re-name column
pig_fam.df <- pig_fam.df %>% rename(pig_fam.cat = cat)




### dist_sch

# initialise empty dataframe
dist_sch.df <- data.frame(Province = NULL,
                            year = NULL,
                            dist_sch = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "dist_sch", dat2)
  
 dist_sch.df <- rbind(dist_sch.df, df)
  
}

# re-name column
dist_sch.df <- dist_sch.df %>% rename(dist_sch.cat = cat)





### crim_case

# initialise empty dataframe
crim_case.df <- data.frame(Province = NULL,
                            year = NULL,
                            crim_case = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "crim_case", dat2)
  
 crim_case.df <- rbind(crim_case.df, df)
  
}

# re-name column
crim_case.df <- crim_case.df %>% rename(crim_case.cat = cat)




### mean_elev

# initialise empty dataframe
mean_elev.df <- data.frame(Province = NULL,
                            year = NULL,
                            mean_elev = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "mean_elev", dat2)
  
 mean_elev.df <- rbind(mean_elev.df, df)
  
}

# re-name column
mean_elev.df <- mean_elev.df %>% rename(mean_elev.cat = cat)




### dist_border

# initialise empty dataframe
dist_border.df <- data.frame(Province = NULL,
                            year = NULL,
                            dist_border = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "dist_border", dat2)
  
 dist_border.df <- rbind(dist_border.df, df)
  
}

# re-name column
dist_border.df <- dist_border.df %>% rename(dist_border.cat = cat)




### dist_provCap

# initialise empty dataframe
dist_provCap.df <- data.frame(Province = NULL,
                            year = NULL,
                            dist_provCap = NULL,
                            cat = NULL)

# loop through each province and apply the function
for(i in 1:length(provs)){

  df <- split.fun(province=provs[i], "dist_provCap", dat2)
  
 dist_provCap.df <- rbind(dist_provCap.df, df)
  
}

# re-name column
dist_provCap.df <- dist_provCap.df %>% rename(dist_provCap.cat = cat)




### Now merge them all

# land_confl, Pax_migt_in, Pax_migt_out, pop_den, M6_24_sch, propPrimSec, propSecSec, Les1_R_Land, pig_fam, dist_sch, crim_case, mean_elev, dist_border, dist_provCap

df.list <- list(land_confl.df, Pax_migt_in.df, Pax_migt_out.df, pop_den.df, M6_24_sch.df, 
                propPrimSec.df, propSecSec.df, Les1_R_Land.df, pig_fam.df, dist_sch.df, crim_case.df, 
                mean_elev.df, dist_border.df, dist_provCap.df)

dat_cat <- df.list %>% reduce(left_join, by=c("Province","year"))

# remove original vars
dat_cat <- dat_cat %>% select(Province,year,land_confl.cat,Pax_migt_in.cat,Pax_migt_out.cat,pop_den.cat,
                              M6_24_sch.cat,propPrimSec.cat,propSecSec.cat,Les1_R_Land.cat,pig_fam.cat,
                              dist_sch.cat,crim_case.cat,mean_elev.cat,dist_border.cat,dist_provCap.cat)

# extract the other vars from dat2 that I need
other.df <- dat2 %>% select(Province,year,ForPix,areaKM,elc,PA)

# merge with dat_cat
dat_cat <- left_join(dat_cat,other.df, by=c("Province","year"))

# save dat_cat
write.csv(dat_cat, file="Data/commune/dat_cat.csv")


#
      # models ####


## load dat_cat
dat_cat <- read.csv("Data/commune/dat_cat.csv", stringsAsFactors = TRUE)
dat_cat <- dat_cat[ ,-1]
str(dat_cat)


## scroll down to the bottom for the information theory model selection

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


# global model with all vars
model.all.cat <- glmer(ForPix ~ pop_den.cat + Land_conflict + Crime + In_migration + Out_migration +
                         School_attendance + Primary_sec + Secondary_sec + no_farmland + Owns_pigs +
                         Distance_school + Elevation +Distance_border + Distance_capital + 
                         Economic_concession + PAs + offset(log(areaKM)) + (year|Province),
                       data=dat_cat, family = "poisson")
summary(model.all.cat)

## Models run above in commune-level section
m1 <- glmer(ForPix ~ pop_den.cat + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m2 <-  glmer(ForPix ~ School_attendance + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
               offset(log(areaKM)) + (year|Province),
             data=dat_cat, family="poisson",
             control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m3 <- glmer(ForPix ~ Primary_sec + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m4 <- glmer(ForPix ~ Owns_pigs + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m5 <- glmer(ForPix ~ Distance_school + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m6 <- glmer(ForPix ~ Crime + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m7 <- glmer(ForPix ~ Out_migration + Elevation + Distance_border+Distance_capital+Economic_concession+PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m8 <- glmer(ForPix ~ School_attendance + Distance_school + Elevation + Distance_border+Distance_capital
            +Economic_concession + PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

m9 <- glmer(ForPix ~ Primary_sec + Out_migration + Elevation + Distance_border+Distance_capital+
              Economic_concession+PAs + 
              offset(log(areaKM)) + (year|Province),
            data=dat_cat, family="poisson",
            control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))


## simpler models

# pop_den
popden.mcat <- glmer(ForPix ~ pop_den.cat + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(popden.mcat)

popden_pred <- data.frame(pop_den.cat = c("low", "high"),
                          areaKM = mean(dat_cat$areaKM))
popden_pred$pred <- as.vector(predict(popden.mcat, type="response", newdata=popden_pred, re.form=NA))

popden_cat_plot <- ggplot()+
                  geom_point(data=dat_cat, aes(x=pop_den.cat, y=ForPix), shape=1)+
                  geom_point(data=popden_pred, aes(x=pop_den.cat, y=pred), shape=19, size=5, col="red")+
                  theme_classic()
# no effect


# social justice (land_confl, crim_case)
socjus.mcat <- glmer(ForPix ~ Land_conflict + Crime + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(socjus.mcat)

landconfl_pred <- expand.grid(Land_conflict = c("low", "high"),
                              Crime = c("low", "high"),
                             areaKM = mean(dat_cat$areaKM))
landconfl_pred$pred <- as.vector(predict(socjus.mcat, type="response", newdata=landconfl_pred, re.form=NA))

landconfl_cat_plot <- ggplot()+
                  geom_point(data=dat_cat, aes(x=Land_conflict, y=ForPix), shape=1)+
                  geom_point(data=landconfl_pred, aes(x=Land_conflict, y=pred), shape=19, size=5, col="red")+
                  facet_wrap(~Crime, labeller = label_both)+
                  theme_classic()
# no effect

crim_cat_plot <- ggplot()+
                  geom_point(data=dat_cat, aes(x=Crime, y=ForPix), shape=1)+
                  geom_point(data=landconfl_pred, aes(x=Crime, y=pred), shape=19, size=5, col="red")+
                  facet_wrap(~Land_conflict, labeller = label_both)+
                  theme_classic()
# no effect



# migration
mig.mcat <- glmer(ForPix ~ In_migration + Out_migration + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(mig.mcat)

mig_pred <- expand.grid(In_migration = c("low", "high"),
                        Out_migration = c("low", "high"),
                        areaKM = mean(dat_cat$areaKM))
mig_pred$pred <- as.vector(predict(mig.mcat, type="response", newdata=mig_pred, re.form=NA))

migtin_cat_plot <- ggplot()+
                geom_point(data=dat_cat, aes(x=In_migration, y=ForPix), shape=1)+
                geom_point(data=mig_pred, aes(x=In_migration, y=pred), shape=19, size=5, col="red")+
                facet_wrap(~Out_migration, labeller = label_both)+
                theme_classic()

migtout_cat_plot <- ggplot()+
                  geom_point(data=dat_cat, aes(x=Out_migration, y=ForPix), shape=1)+
                  geom_point(data=mig_pred, aes(x=Out_migration, y=pred), shape=19, size=5, col="red")+
                  facet_wrap(~In_migration, labeller = label_both)+
                  theme_classic()
  # no effect



# education
edu.mcat <- glmer(ForPix ~ School_attendance + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(edu.mcat)

edu_pred <- expand.grid(School_attendance = c("low", "high"),
                        areaKM = mean(dat_cat$areaKM))
edu_pred$pred <- as.vector(predict(edu.mcat, type="response", newdata=edu_pred, re.form=NA))

edu_cat_plot <- ggplot()+
              geom_point(data=dat_cat, aes(x=School_attendance, y=ForPix), shape=1)+
              geom_point(data=edu_pred, aes(x=School_attendance, y=pred), shape=19, size=5, col="red")+
              theme_classic()
# no effect



# employment
emp.mcat <- glmer(ForPix ~ Primary_sec + Secondary_sec + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(emp.mcat)

emp_pred <- expand.grid(Primary_sec = c("low", "high"),
                        Secondary_sec = c("low", "high"),
                        areaKM = mean(dat_cat$areaKM))
emp_pred$pred <- as.vector(predict(emp.mcat, type="response", newdata=emp_pred, re.form=NA))

primsec_cat_plot <- ggplot()+
                  geom_point(data=dat_cat, aes(x=Primary_sec, y=ForPix), shape=1)+
                  geom_point(data=emp_pred, aes(x=Primary_sec, y=pred), shape=19, size=5, col="red")+
                  facet_wrap(~Secondary_sec, labeller = label_both)+
                  theme_classic()

secsec_cat_plot <- ggplot()+
                  geom_point(data=dat_cat, aes(x=Secondary_sec, y=ForPix), shape=1)+
                  geom_point(data=emp_pred, aes(x=Secondary_sec, y=pred), shape=19, size=5, col="red")+
                  facet_wrap(~Primary_sec, labeller = label_both)+
                  theme_classic()
# no effect


# economic security
econ.mcat <- glmer(ForPix ~ no_farmland + Owns_pigs + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(econ.mcat)

econ_pred <- expand.grid(no_farmland = c("low", "high"),
                         Owns_pigs = c("low", "high"),
                        areaKM = mean(dat_cat$areaKM))
econ_pred$pred <- as.vector(predict(econ.mcat, type="response", newdata=econ_pred, re.form=NA))

rice_cat_plot <- ggplot()+
                geom_point(data=dat_cat, aes(x=no_farmland, y=ForPix), shape=1)+
                geom_point(data=econ_pred, aes(x=no_farmland, y=pred), shape=19, size=5, col="red")+
                facet_wrap(~Owns_pigs, labeller = label_both)+
                theme_classic()

pig_cat_plot <- ggplot()+
                geom_point(data=dat_cat, aes(x=Owns_pigs, y=ForPix), shape=1)+
                geom_point(data=econ_pred, aes(x=Owns_pigs, y=pred), shape=19, size=5, col="red")+
                facet_wrap(~no_farmland, labeller = label_both)+
                theme_classic()
# no effect


# access to services
acc.mcat <- glmer(ForPix ~ Distance_school + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(acc.mcat)

acc_pred <- expand.grid(Distance_school = c("low", "high"),
                        areaKM = mean(dat_cat$areaKM))
acc_pred$pred <- as.vector(predict(acc.mcat, type="response", newdata=acc_pred, re.form=NA))

sch_cat_plot <- ggplot()+
                geom_point(data=dat_cat, aes(x=Distance_school, y=ForPix), shape=1)+
                geom_point(data=acc_pred, aes(x=Distance_school, y=pred), shape=19, size=5, col="red")+
                theme_classic()
# no effect



# environmental 
elev.mcat <- glmer(ForPix ~ Elevation + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(elev.mcat)

elev_pred <- expand.grid(Elevation = c("low", "high"),
                        areaKM = mean(dat_cat$areaKM))
elev_pred$pred <- as.vector(predict(elev.mcat, type="response", newdata=elev_pred, re.form=NA))

elev_cat_plot <- ggplot()+
                geom_point(data=dat_cat, aes(x=Elevation, y=ForPix), shape=1)+
                geom_point(data=elev_pred, aes(x=Elevation, y=pred), shape=19, size=5, col="red")+
                theme_classic()
# no effect



# other human vars
hum.mcat <- glmer(ForPix ~ Distance_border + Distance_capital + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(hum.mcat)

hum_pred <- expand.grid(Distance_border = c("low", "high"),
                        distance_capital = c("low", "high"),
                        areaKM = mean(dat_cat$areaKM))
hum_pred$pred <- as.vector(predict(hum.mcat, type="response", newdata=hum_pred, re.form=NA))

border_cat_plot <- ggplot()+
                geom_point(data=dat_cat, aes(x=Distance_border, y=ForPix), shape=1)+
                geom_point(data=hum_pred, aes(x=Distance_border, y=pred), shape=19, size=5, col="red")+
                facet_wrap(~distance_capital, labeller = label_both)+
                theme_classic()

provCap_cat_plot <- ggplot()+
                  geom_point(data=dat_cat, aes(x=distance_capital, y=ForPix), shape=1)+
                  geom_point(data=hum_pred, aes(x=distance_capital, y=pred), shape=19, size=5, col="red")+
                  facet_wrap(~Distance_border, labeller = label_both)+
                  theme_classic()



# elcs and PAs
areas.mcat <- glmer(ForPix ~ Economic_concession + PAs + offset(log(areaKM)) + (year|Province),
                     data=dat_cat, family="poisson")

summary(areas.mcat)

areas_pred <- expand.grid(Economic_concession = c("0", "1"),
                        PAs = c("0", "1"),
                        areaKM = mean(dat_cat$areaKM))
areas_pred$pred <- as.vector(predict(areas.mcat, type="response", newdata=areas_pred, re.form=NA))

dat_cat$Economic_concession <- as.factor(dat_cat$Economic_concession)
dat_cat$PAs <- as.factor(dat_cat$PAs)


elc_cat_plot <- ggplot()+
                geom_point(data=dat_cat, aes(x=Economic_concession, y=ForPix), shape=1)+
                geom_point(data=areas_pred, aes(x=Economic_concession, y=pred), shape=19, size=5, col="red")+
                facet_wrap(~PAs, labeller = label_both)+
                theme_classic()

PA_cat_plot <- ggplot()+
              geom_point(data=dat_cat, aes(x=PAs, y=ForPix), shape=1)+
              geom_point(data=areas_pred, aes(x=PAs, y=pred), shape=19, size=5, col="red")+
              facet_wrap(~Economic_concession, labeller = label_both)+
              theme_classic()



# compare models
model.sel(popden.mcat,socjus.mcat,mig.mcat,edu.mcat,emp.mcat,econ.mcat,acc.mcat,
        elev.mcat,hum.mcat,areas.mcat,m1,m2,m3,m4,m5,m6,m7,m8,m9)

# m8 has the most support. m5 has some support (dAIC = 5), but is a simpler model of m8 and so inference can come from m8

summary(m8)












#
      # plot categorical ####

# Based on IT model selection, m8 is the top model. Below is some old code that plots the predictions from all of the indiviudal models above. can ignore

M6_24_sch.cat + dist_sch.cat + Elevation + Distance_border+Distance_capital
+Economic_concession + PAs

# create new data
m8_newdat <- expand.grid(School_attendance=c("high","low"),
                         Distance_school=c("high","low"),
                         Elevation=c("high","low"),
                         Distance_border=c("high","low"),
                         Distance_capital=c("high","low"),
                         Economic_concession=c("1","0"),
                         PAs=c("1","0"),
                         Province=levels(dat_cat$Province),
                         areaKM=mean(dat_cat$areaKM),
                         year=mean(dat_cat$year))
str(m8_newdat)

# predict for the two socioeconomic variables - School attendance and distance to school
m8_pred <- as.vector(predict(m8, newdata=m8_newdat, type="response", re.form=~(year|Province)))
                     

m8_newdat$pred <- m8_pred

### plots

# M6_24_sch (all others set to reference - low, and ELC/PA set to 1)
p1.dat <- m8_newdat %>% filter(Distance_school=="low",
                               Elevation=="low",
                               Distance_border=="low",
                               Distance_capital=="low",
                               Economic_concession=="1",
                               PAs=="1")
# attach SEs


# convert predictions to KM
p1.dat$Forest_cover_km <- p1.dat$pred*0.09

p1 <- ggplot(p1.dat, aes(x=School_attendance, y=Forest_cover_km))+
        geom_point(size=5)+
        #geom_point(data=dat_cat, aes(x=School_attendance, y=ForPix))+
        facet_wrap(~Province,labeller = label_both)+
        theme_classic()+
        #ylim(3250,3800)+
        xlab("School attendance (males aged 6-24)")+
        ylab("Predicted forest cover (km2)")+
        theme(axis.title = element_text(size=15),
              axis.text = element_text(size=15),
              strip.text.x = element_text(size=12))

ggsave("Results/Socioeconomics/Plots/Province_level/School_atten_facet_prov.png", p1,
       height=30, width=30, unit="cm", dpi=300)

# dist_sch (all others set to reference - low, and ELC/PA set to 1)
p2.dat <- m8_newdat %>% filter(School_attendance=="low",
                               Elevation=="low",
                               Distance_border=="low",
                               Distance_capital=="low",
                               Economic_concession=="1",
                               PAs=="1")
# attach SEs


# convert predictions to KM
p2.dat$Forest_cover_km <- p2.dat$pred*0.09

p2 <- ggplot(p2.dat, aes(x=Distance_school, y=Forest_cover_km))+
      geom_point(size=5)+
      facet_wrap(~Province,labeller = label_both)+
      theme_classic()+
      #ylim(3250,3800)+
      xlab("Distance to nearest school (km)")+
      ylab("Predicted forest cover (km2)")+
      theme(axis.title = element_text(size=15),
            axis.text = element_text(size=15),
            strip.text.x = element_text(size=12))

ggsave("Results/Socioeconomics/Plots/Province_level/dist_sch_facet_prov.png", p2,
       height=30, width=30, unit="cm", dpi=300)




### Old code for plotting predictions from individual models 

prov_cat_plot <- popden_cat_plot + landconfl_cat_plot + crim_cat_plot + migtin_cat_plot + 
                 migtout_cat_plot + edu_cat_plot + primsec_cat_plot + secsec_cat_plot + 
                 rice_cat_plot + pig_cat_plot + sch_cat_plot + elev_cat_plot + 
                 border_cat_plot + provCap_cat_plot + elc_cat_plot + PA_cat_plot

# remove y axis labels for 2:4, 6:8, 10:12, 14:16
ry <- c(2:4, 6:8, 10:12, 14:16)
for(i in ry){
  prov_cat_plot[[i]] <- prov_cat_plot[[i]] + theme(axis.title.y = element_blank())
}


# add y axis labels for 1, 5, 9, 13
yy <- c(1, 5, 9, 13)
for(i in yy){
  prov_cat_plot[[i]] <- prov_cat_plot[[i]] + ylab("Forest cover (pixels)")
}


# remove y axis text for 2:4, 6:8, 10:12, 14:16
ry <- c(2:4, 6:8, 10:12, 14:16)
for(i in ry){
  prov_cat_plot[[i]] <- prov_cat_plot[[i]] + theme(axis.text.y = element_blank())
}


# re-name x axes
prov_cat_plot[[1]] <- prov_cat_plot[[1]] + xlab("Population density")
prov_cat_plot[[2]] <- prov_cat_plot[[2]] + xlab("Land conflict cases")
prov_cat_plot[[3]] <- prov_cat_plot[[3]] + xlab("Crime per capita")
prov_cat_plot[[4]] <- prov_cat_plot[[4]] + xlab("Number of in-migrants")
prov_cat_plot[[5]] <- prov_cat_plot[[5]] + xlab("Number of out-migrants")
prov_cat_plot[[6]] <- prov_cat_plot[[6]] + xlab("Proportion (16-24) in school")
prov_cat_plot[[7]] <- prov_cat_plot[[7]] + xlab("Proportion employed in primary sector")
prov_cat_plot[[8]] <- prov_cat_plot[[8]] + xlab("Proportion employed in secondary sector")
prov_cat_plot[[9]] <- prov_cat_plot[[9]] + xlab("Proportion with no farmland")
prov_cat_plot[[10]] <- prov_cat_plot[[10]] + xlab("Proportion with pigs")
prov_cat_plot[[11]] <- prov_cat_plot[[11]] + xlab("Distance to school (km)")
prov_cat_plot[[12]] <- prov_cat_plot[[12]] + xlab("Mean elevation (m)")
prov_cat_plot[[13]] <- prov_cat_plot[[13]] + xlab("Distance to Intl border (km)")
prov_cat_plot[[14]] <- prov_cat_plot[[14]] + xlab("Distance to Provincial capital (km)")
prov_cat_plot[[15]] <- prov_cat_plot[[15]] + xlab("Presence of ELCs")
prov_cat_plot[[16]] <- prov_cat_plot[[16]] + xlab("Presence of PAs")


ggsave("Results/Socioeconomics/Plots/Province_level/prov_level_cat.png",prov_cat_plot,
       width = 30, height = 30, unit="cm", dpi=300)


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


# one thing to ask Jeoren/Nils about is when predicting for provincial means and predicting for PA/no-PA, I have been using a range of the predictor (e.g. pop_den) that is found within that province. Ie in Stung Treng I have only used the range of pop_den values that actually exists in Stunf Treng. I guess it would be interesting to see what the predictions say when you are looking at plausible increases/decreases in the predictor beyond the range of that province.  I know you're not really supposed to predict beyond the range of the data, but what if you were only predicting within the national range?
 

# make an excel spreadsheet that lists all the provinces that have large variation between communes for the different predictions (just use the plots). This will make it easier to see if there are consistencies in which provinces a) have larger effects and b) have the most within-province variation. 

# Ask Jeroen how to find the slopes for each commune for each prediction. Do the same as above but for all communes that have steep slopes - to look for trends and consitencies

# need to check with Jeroen about what the global predictions are actually predicting - i.e. what is the output, is it number of forest pixels, or number of pixels per unit area (km2)? If it is pixels per unit area, I need to go back and check all the global predictions, because I will have written off some becuase the pred values were really low, but actually you can only get a maximum of 11.11 pixels into a single km2, so perhaps they weren't as bad as I thought. 

# ask Jeroen what he thinks about keeping elc and PA in the model when they don't have any effect but conceptually/theoretically they should be accounted for

# add observed data onto grid plots. do the same for reduced x axis range

# try median area in predictions

