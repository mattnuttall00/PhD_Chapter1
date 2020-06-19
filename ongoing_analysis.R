#' ---
#' title: Mixed models - socioeconomic predictors of forest cover
#' author: Matt Nuttall
#' date: 17/06/20
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

#+ include=FALSE
library(sjPlot)
library(DHARMa)
library(lme4)
library(patchwork)
library(pbkrtest)
library(MuMIn)
library(RColorBrewer)
library(tidyverse)

# load data
dat <- read.csv("Data/commune/dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

dat1 <- dat %>% 
  mutate_at(c("year","tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec","propSecSec","Les1_R_Land",
              "pig_fam","dist_sch","garbage","KM_Comm","land_confl","crim_case","Pax_migt_in",
              "Pax_migt_out","mean_elev","dist_border","dist_provCap"), ~(scale(.) %>% as.vector))


# merge Province and Commune into a new unique variable (to remove issue of communes with the same name)
dat1 <- dat1 %>% mutate(Provcomm = paste(dat1$Province, dat1$Commune, sep = "_"))

# add original year (i.e. not scaled)
dat1$year.orig <- dat$year


#' This document is a summary of the ongoing analysis into predictors of forest cover/loss at the commune level across Cambodia.
#' 
#' Currently the response variable is raw count of forest pixels, and there are 9 predictor variable sets with varying number of predictors in each set. All predictor variables have been scaled and centered prior to analysis. A GLMM framework has been used, with a Poisson distribution and the following random effects structure:
#' 
#' (year|Province/Commune)
#' 
#' The logged area of each commune has been used as an offset term in each model.
#' 
#' The variable sets are as follow:
#' 
#' * POPULATION DEMOGRAPHICS - population density, total population, proportion indigenous
#' * EDUCATION -  proportion of males aged 6-24 in school
#' * EMPLOYMENT - proportion of people employed in the Primary sector, proportion of people emplyed in the Secondary sector
#' * ECONOMIC SECURITY - proportion of population with less than 1ha of farming land, proportion of families with pigs
#' * ACCESS TO SERVICES - median distance to nearest school, median distance to the Commune Office, proportion of families with access to waste collection services
#' * SOCIAL JUSTICE - total number of land conflict cases, number of criminal cases per capita
#' * MIGRATION - total number of in-migrants, total number of out-migrants
#' * ENVIRONMENTAL (CONTROL) - mean elevation, habitat
#' * HUMAN (CONTROL) - distance to international border, distance to Provincial Capital, PA presence, PA category, economic land concession presence
#' 
#' ## Brief summary
#' 
#' Unfortunately, none of the socioeconomic predictors appear to have any predictive power!  The only exception is population density.  The "control" variables however, do have some predictive power. Below I will show the results of the initial models for each set.
#' 
#' My general conclusion thus far however is that with this response, I don't think I am asking the question I want to. I think the models below are telling us which variables can predict forest cover (i.e. where is the forest? Which communes have lots of forest and which ones have little forest?).  I don't think the below models can really tell me anything about forest LOSS, or which variables are able to predict forest cover loss/change.  I feel like I need to go further and look at forest cover change as a response (e.g. rate of change / difference in forest pixels).  That's not to say that what I have done so far is not useful - I think in combination with further analysis we can build up a wider picture of forest cover and forest cover dynamics.  For example, the overall analysis and our results could be made up of the following:
#' 
#' * Which variables predict where forest cover is greatest / lowest across the country? (this is what I have done. Although at the moment the question is really "...where forest cover is greatest/lowest given there is SOME forest. This is because I removed all communes with 0 forest cover right at the start, as at that stage I was planning on useing rate of change as a response and so communes with no forest were false 0's)
#' * Which variables predict where forest is likely to be lost? (Presumably I could use binomial logistic model here: forest lost/not lost)
#' * Then subset the communes to only those that have lost some forest, and then model rate of change / magnitude of change. (Initially this is what I was going to do, either manually i.e. two steps, or via a hurdle model)
#' 
#' I keep having to remind myself of what the purpose of this analysis is.  The purpose is to use the resulting model(s) to predict where in the country forest is likely to be lost under various potential scenarios. I want to be able to highlight communes where forest cover is likely to be lost if, say, population density in rural areas goes up, or if the number of land conflicts in communes with land concessions spike.  The aim WAS then to use these predictions to assess the robustness of the national wildlife corridor network to future changes.  But in theory I could do a number of things, e.g. identifiy which PAs are most vulnerable to certain future scenarios etc. 
#' The main point being though, is that I don't think I can use the below models to do that.  
#' 
#' Unless I am mis-understanding the usefulness of the models below.  The only way I can think to use these models to do what I want, is using the below approach:
#' 
#' * Ignore the "global" model (i.e. the model with RE's set to means)
#' * Pull out the individual communes of interest, e.g. the ones within the wildlife corridor, or the ones in/around the PAs
#' * run the commune-specific models (i.e. with RE's specific to that commune) with varying ranges of the predictors, e.g. increasing population density.  Do you think this does what I want?
#' 
#' The obvious problem with the above approach is that I don't really have any socioeconomic predictors (apart from population density) to play with as they're all a bit shit!  The other predictor variables that have some predictive power are mostly unchangeable i.e. elevation, distance to Provincial capital etc.
#' 
#' So I am currently unsure how best to proceed, and I would very much appreciate some guidance!  Below I will summarise the models, the diagnostics, and the predictions.
#' 
#' # Model results
#' 
#' ## Population demographics   
#' 
#' Full population demographics model:

#+ popdem.m1
popdem.m1 <- glmer(ForPix ~ tot_pop + pop_den + prop_ind + offset(log(areaKM)) + (year|Province/Provcomm),
                   data=dat1, family="poisson")

summary(popdem.m1)

#' Variance for Province and commune are similar. Variance for year at both levels is small, but an order of magnitude smaller for the Province level. Approximate p values are sig for pop_den only. tot_pop is positive effect, pop_den and prop_ind are negative. no real correlation between fixed effects.Note: I did run the model without offset and the variance for Province and Provcomm:Province were double what they are now that offset is included.
#'
#' Plot the random effects. First plot is Commune, second plot is Province

#+ popdem.m1 RE plots, echo=FALSE, results=TRUE
plot_model(popdem.m1, type="re")

#' The model summary and likelihood ratio tests suggested proportion indigneous was contributing little, and so that variable was dropped. There was evidence of an interaction between population density and total population, and likelihood ratio tests suggested the model with both terms in was better than the model with only population density, but these two variables are intrinsically linked, and so I dropped total population, as it had the weaker effect. As a note on LRT's - my protocol was to begin with simple 'anova(test="Chisq")' and if there was a variable/model that had a p-value that was hovering around 0.05, then I would progress to another test (e.g. profiled CI's, bootstrapping, but this was never really necessary).
#' 
#' Therefore the final population demographics model is:
#' 

#+ popdem.m6, 
popdem.m6 <- glmer(ForPix ~ pop_den + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")

summary(popdem.m6)

#' ### Diagnostics
#' 
#+ popdem.m6 predictions and residuals, include=FALSE
# copy data
m6.diag.dat <- dat1
m6.diag.dat$Provcomm <- as.factor(m6.diag.dat$Provcomm)

# Make "fitted" predictions, i.e. fully conditional:
m6.diag.dat$m6pred <- predict(popdem.m6, type = "response")

# attach residuals
m6.diag.dat$m6res <- resid(popdem.m6)

#' Plot of predicted values vs observed values

#+ popdem.m6 fitted vs observed plot, echo=FALSE, results=TRUE
plot(m6.diag.dat$ForPix, m6.diag.dat$m6pred, ylab = "Predicted ForPix", xlab = "Observed ForPix")

#' Looks good
#' 
#' Plot residuals vs fitted values
#' 
#+ popdem.m6 plot fitted vs residuals, echo=FALSE,results=TRUE
plot(m6.diag.dat$m6pred, m6.diag.dat$m6res)

#' Quite bad heterogeneity here. Jeroen suggested this could be due to missing predictors, however this was also the case for previous models that had the other 2 predictors. At low predicted values, error is greater. As Jeroren said - given the structure in the data, this is almost inevitable given the extent of variation across communes. AS we will see, this is an issues with all of the models.
#' 
#' Exploration of the residuals over the predictors, and between provinces and communes
#' 
#+ popdem.m6 residual plots, echo=FALSE,results=TRUE
par(mfrow=c(2,2))
plot(m6.diag.dat$pop_den, m6.diag.dat$m6res, ylab = "residuals", xlab = "pop density")
boxplot(m6res ~ factor(Province), data = m6.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Province")
boxplot(m6res ~ factor(Provcomm), data = m6.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Commune")

#' A lot of the heterogeneity is driven by relatively few communes/provinces. I investigated further. If you look at the map (in GIS), the provinces with the smallest residuals are the smaller provinces in the south surrounding the capital Phnom Penh. These are provinces that generally have very little forest. But they are not the only provinces with no forest cover, so I am not sure that is the (only) reason. I think I need to look at the communes within the provinces
#' 
#+ popdem.m6 extract problem residuals, include=FALSE
provs <- c("Battambang","Kampong Chhnang","Kampong Thom","Kracheh","Pursat","Ratanak Kiri","Siem Reap",
           "Stung Treng")

# extract all original data from those provinces
prov.dat <- dat %>% filter(Province %in% provs) 
prov.dat$type <- "problem"
# create subset of all data NOT including the above
prov.excl <- dat %>% filter(!Province %in% provs)
prov.excl$type <- "other"
prov.all <- rbind(prov.dat,prov.excl)

#' Here I plot foresr pixels for each commune, separated by the communes with high residuals in the model ("problem"), and the rest ("other")
#' 
#+ popdem.m6 plot problem communes ForPix, echo=FALSE, results=TRUE, warnings=FALSE
ggplot(prov.all, aes(Commune,y=ForPix, colour=type))+
  geom_point()+
  theme(element_blank())

#' I would say that the provinces that are causing the issues tend to have higher ForPix than the others. It also look potentially like the provinces causing issues are almost always losing forest cover over time (lots of vertical lines of dots)
#' 
#' Plot of population density (the predictor) against forest pixels, split by the problem communes again. I have zoomed in on both axes so we can see what is gong on
#' 
#+ popdem.m6 plot problem communes ForPix ~ pop_den, echo=FALSE, results=TRUE, warnings=FALSE
ggplot(prov.all, aes(x=pop_den, y=ForPix, colour=type))+
  geom_point()+
  ylim(0,10000)+
  xlim(0,1000)+
  theme(element_blank())

#' This plot makes it also look like the problem provinces tend to have communes with higher ForPix. There is a chunk of blue with no pink where pop_den has increased to about 100 but ForPix has not decreased (whereas almost all the pink dots are lower, i.e. in the other provinces/communes at that population density forest cover is lower). 
#' 
#' The plot below shows the forest pixles by province (rather than commune), split by the problem provinces. 
#' 
#+ popdem.m6 plot problem provinces and ForPix, echo=FALSE, results=TRUE, warnings=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)+
  theme(element_blank())

#' This plot suggests that the problem provinces don't necessarily have more (mean) forest cover, but they do tend to have more outliers (individual communes) that are very high forest cover. 
#' 
#' The plot below shows the population density by province, split by the problme provinces.
#' 
#+ popdem.m6 plot problem provinces and pop_den, echo=FALSE, results=TRUE, warnings=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=pop_den, colour="Other provinces"))+
  theme(element_blank())

#' This plot suggests that overall, the problem provinces tend to have lower median pop_den value compared to the other provinces, but they again tend to have more outliers.  This is what I would expect - the communes with lower population denstiy will also likely be the communes with higher forest cover (as we saw in the above plots).
#' 
#' The plot below shows the same as above but by communes, rather than province
#' 
#+ popdem.m6 plot problem communes and pop_den, echo=FALSE, results=TRUE, warnings=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=pop_den, colour="Other provinces"))+
  ylim(0,1000)+
  theme(element_blank())

#' Although slightly difficult to interpret, again this looks like there are more communes with higher pop_den values in the non-problematic provinces. And in the problem communes with higher population density values, they also tend to have much more variation in population density (i.e. the pop_den changes a lot over time)
#' 
#' The plot below shows the problem communes and their forest pixels
#' 
#+ popdem.m6 plot problem communes and ForPix, echo=FALSE, results=TRUE, warnings=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)+
  theme(element_blank())

#' Again, difficult to see a huge amount however I would say that it looks like the problem provinces in general have more variation in ForPix, both within communes and between communes. 
#' 
#' If you look at the plot of the main effect in the section below, the line is pretty flat, even at low pop_den and ForPix values. This is where the model is not predicitng well for communes with low population density but higher forest cover. This is because there is so much variation in the communes - i.e. there are so many that have low pop_den but low ForPix, which is dragging the model down.  So the communes with large residuals are going to be the commune with low pop_den values and higher ForPix values I think.