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


