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
#' If you look at the plot of the main effect in the section below, the line is pretty flat, even at low pop_den and ForPix values. This is where the model is not predicitng well for communes with low population density but higher forest cover. This is because there is so much variation in the communes - i.e. there are so many that have low pop_den but low ForPix, which is dragging the model down.  So the communes with large residuals are going to be the commune with low pop_den values and higher ForPix values I think.  I do not think there is much I can do about this?  Would including year as a fixed effect perhaps help account for the variation within communes across years?
#' 
#' ### Predict main (global) effects
#' 
#+ popdem.m6 predict & plot main effects, echo=FALSE, results=TRUE
# create new data
m6.newdat <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                        areaKM = mean(dat1$areaKM))

# add predictions
m6.newdat$pred <- as.vector(predict(popdem.m6, type="response", newdata=m6.newdat, re.form=NA))

# plot with free y axis
popdem.m6.p1 <- ggplot(m6.newdat, aes(x=pop_den, y=pred))+
  geom_line()+
  theme(element_blank())+
  xlab("Population density (scaled and centered)")+
  ylab("Predicted forest pixels")

# plot with large y axis
popdem.m6.p2 <-ggplot(m6.newdat, aes(x=pop_den, y=pred))+
  geom_line()+
  ylim(0,5000)+
  theme(element_blank())+
  xlab("Population density (scaled and centered)")+
  ylab("Predicted forest pixels")

popdem.m6.p1 + popdem.m6.p2

#' We can see in the plots above that the effect is there (left plot), but it is small when you look at it with a more realistic y-axis, i.e. with forest pixel values more like those that are actually seen (right plot)
#' 
#' ### Predict for specific communes
#' 
#' Here I want to plot grids of different communes with the overall predicted effect, plus the commune-specific effect. I want to do this for communes with commune-level intercepts close to the mean, and communes with commune-level intercpets at the extremes.  I will also do this for a random sample of communes.
#' 
#+ popdem.m6 commune predictions, include=FALSE
# save the popdem.m4 commune-level random effects
m6.re.com <- ranef(popdem.m6)[[1]]

# re-order
m6.re.com <- m6.re.com[order(m6.re.com[ ,"(Intercept)"], decreasing = FALSE),]

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

#' In the above plots, the top row are communes with intercepts closest to the mean, the middle row are communes with intercepts furthest above the mean, and the bottom row are communes with intercepts furthest below the mean.  The coloured solid lines are the commune-specific predictions, i.e. those made using all RE's specific to that commune. The dashed black line is the global model (i.e. for an "average" commune). The poins are the actual pop_den ~ ForPix values for that commune. The plots support the diagnostics in the section above - the global model does not predict well for communes with high forest cover - because the global model always predicts low forest cover, even for communes with low population density.  Essentially there are too many communes with low forest cover, and so the global model is dragged down, meaning that the communes with high forest cover get poor predictions. This does suggest though that the global model predicts well at high population density values, as these communes tend to have low forest cover.
#' 
#' Now I will do the same but for 9 random selections of 9 communes.
#' 
#+ popdem.m6 commune predictions from random selection, echo=FALSE,results=TRUE
# randomly sample communes
par(mfrow = c(3,3))
set.seed(123)
runs <- c(1:9)

for(i in 1:length(runs)){
  
rand.com <- sample(dat1$Provcomm,9, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17)]

# define the range of pop_den to predict for, first. Min/max per commune:
pop_den_min <- tapply(dat1$pop_den, dat1$Provcomm, min)
pop_den_max <- tapply(dat1$pop_den, dat1$Provcomm, max)
# Min/max within your selection of communes:
pop_den_min <- min(pop_den_min[names(pop_den_min) %in% rand.com])
pop_den_max <- max(pop_den_max[names(pop_den_max) %in% rand.com])

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
m6_newdat_com_ran$pred.glo <- rep(m6.newdat$pred, times=9)

# set levels
m6_newdat_com_ran$Provcomm <- as.factor(m6_newdat_com_ran$Provcomm)
provcomm_lvls <- levels(m6_newdat_com_ran$Provcomm) 


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
}
#'
#' The above plots I think support the assessment that the global model predicts poorly for most communes, but I would expect this as there is so much variation between communes, it would be impossible to get a single model that predicted well. The above plots suggest that the commune-specific models predict better for communes where population density changes over time, but they predict poorly for communes when forest pixels change over time (i.e. forest is lost). Communes with more forest are, in general, more likely to lose forest (see plot below), which I think exacerbates the problem of the global model predicting poorly for communes with high forest cover i.e. it's a double whammy of poor predictions due to high forest cover, and poor predictions due to forest loss.
#' 
#+ plot forest loss comparison between high and low forested communes, echo=FALSE,results=TRUE
## check whether communes with more forest are more likely to lose forest

# separate dat1 into the quarter with the most forest and then the rest
com.for <- dat1[dat1$ForPix>982,] # 982 is the 3rd quantile
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

#' The above plot splits the communes by forest cover - communes in the top 3rd quarter of most forested are in the "high forest" category, and the rest are in the "low forest" category. I have then summed the difference in pixels within each commune across the study period (i.e. if a commune loses 10 pixels each year it would have a y-axis value of 50). We can see that the communes with more forest, are more likely to lose forest. 
#' 
#' 
#' ## Education
#' 
#' There is only one variable in this set - the number of males aged 6-24 in school. There were more raw variables, but they were all highly correlated and so this one was taken forward as I think it is the most relevant to forest cover. This is because young men are the most likely to be engaging in agriculture, forest clearance, logging etc.
#' 
#+ edu.m1, include=TRUE
edu.m1 <- glmer(ForPix ~ M6_24_sch + offset(log(areaKM)) + (year|Province/Provcomm), family="poisson", data=dat1)

summary(edu.m1)

#' The model output suggests there is nothing going on here.  I can use 'plot_model' from the sjPlot package to quickly plot a prediction. It basically does exactly what I would do - create newdata with the variable of interest varying from the min to the max, and holding all others at their mean.
#' 
#+ edu.m1 plot_model, echo=FALSE, results=TRUE
plot_model(edu.m1, type="pred", terms="M6_24_sch")

#' Flat as a pancake. 
#' 
#' Below are some commune-specific predictions from another random selection of communes
#'  
#+ edu.m1 commune predictions, echo=FALSE,results=T

par(mfrow = c(3,3))
set.seed(123)
runs <- c(1:9)

for(i in 1:length(runs)){
# randomly sample communes
rand.com <- sample(dat1$Provcomm, 9, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17)]

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
edu.m1_newdat_com_ran$pred.glo <- rep(edu.m1_glo_pred$pred, times=9)

# set levels
edu.m1_newdat_com_ran$Provcomm <- as.factor(edu.m1_newdat_com_ran$Provcomm)
provcomm_lvls <- levels(edu.m1_newdat_com_ran$Provcomm) 

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
  if(i == 1) {
  # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$M6_24_sch,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Proportion of males aged 6-24 in school (scaled & standardised",
       ylab = "Predicted forest cover (forest pixel count)")
  } else {
  lines(preddat_i$M6_24_sch,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$M6_24_sch,preddat_i$pred.glo, lty=2)
  }
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$M6_24_sch, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}
}

#' I think it is fair to say that there is no relationship between forest cover and males in school.