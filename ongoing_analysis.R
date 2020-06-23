#' ---
#' title: Mixed models - socioeconomic predictors of forest cover
#' author: Matt Nuttall
#' date: 23/06/20
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
#+ popdem.m6 residual plots, echo=FALSE,results=TRUE, fig.width=10, fig.height=10, dpi=100
par(mfrow=c(2,2))
plot(m6.diag.dat$pop_den, m6.diag.dat$m6res, ylab = "residuals", xlab = "pop density")
boxplot(m6res ~ factor(Province), data = m6.diag.dat, outline = T, xlab = "Province", ylab = "Residuals w/i Province")
boxplot(m6res ~ factor(Provcomm), data = m6.diag.dat, outline = T, xlab = "Commune", ylab = "Residuals w/i Commune")

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
  theme(element_blank())+
  ylab("Forest pixels")

#' I would say that the provinces that are causing the issues tend to have higher ForPix than the others. It also look potentially like the provinces causing issues are almost always losing forest cover over time (lots of vertical lines of dots)
#' 
#' Plot of population density (the predictor) against forest pixels, split by the problem communes again. I have zoomed in on both axes so we can see what is gong on
#' 
#+ popdem.m6 plot problem communes ForPix ~ pop_den, echo=FALSE, results=TRUE, warning=FALSE
ggplot(prov.all, aes(x=pop_den, y=ForPix, colour=type))+
  geom_point()+
  ylim(0,10000)+
  xlim(0,1000)+
  theme(element_blank())+
  ylab("Forest pixels")+
  xlab("population density")

#' This plot makes it also look like the problem provinces tend to have communes with higher ForPix. There is a chunk of blue with no pink where pop_den has increased to about 100 but ForPix has not decreased (whereas almost all the pink dots are lower, i.e. in the other provinces/communes at that population density forest cover is lower). 
#' 
#' The plot below shows the forest pixles by province (rather than commune), split by the problem provinces. 
#' 
#+ popdem.m6 plot problem provinces and ForPix, echo=FALSE, results=TRUE, warning=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)+
  theme(element_blank())+
  ylab("Forest pixels")

#' This plot suggests that the problem provinces don't necessarily have more (mean) forest cover, but they do tend to have more outliers (individual communes) that are very high forest cover. 
#' 
#' The plot below shows the population density by province, split by the problme provinces.
#' 
#+ popdem.m6 plot problem provinces and pop_den, echo=FALSE, results=TRUE, warning=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Province, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Province, y=pop_den, colour="Other provinces"))+
  theme(element_blank())+
  ylab("Population density")

#' This plot suggests that overall, the problem provinces tend to have lower median pop_den value compared to the other provinces, but they again tend to have more outliers.  This is what I would expect - the communes with lower population denstiy will also likely be the communes with higher forest cover (as we saw in the above plots).
#' 
#' The plot below shows the same as above but by communes, rather than province
#' 
#+ popdem.m6 plot problem communes and pop_den, echo=FALSE, results=TRUE, warning=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=pop_den,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=pop_den, colour="Other provinces"))+
  ylim(0,1000)+
  theme(element_blank())+
  ylab("Population density")

#' Although slightly difficult to interpret, again this looks like there are more communes with higher pop_den values in the non-problematic provinces. And in the problem communes with higher population density values, they also tend to have much more variation in population density (i.e. the pop_den changes a lot over time)
#' 
#' The plot below shows the problem communes and their forest pixels
#' 
#+ popdem.m6 plot problem communes and ForPix, echo=FALSE, results=TRUE, warning=FALSE
ggplot()+
  geom_boxplot(data=prov.dat, aes(x=Commune, y=ForPix,colour="Problem provinces"))+
  geom_boxplot(data=prov.excl, aes(x=Commune, y=ForPix, colour="Other provinces"))+
  ylim(0,10000)+
  theme(element_blank())+
  ylab("Forest pixels")

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
#+ popdem.m6 commune predictions, echo=FALSE, results=T,fig.width=10, fig.height=10, dpi=100
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
       ylab = "Predicted forest cover pixels")
  # } else {
  lines(preddat_i$pop_den,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$pop_den,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$pop_den, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

#' In the above plots, the top row are communes with intercepts closest to the mean, the middle row are communes with intercepts furthest above the mean, and the bottom row are communes with intercepts furthest below the mean.  The coloured solid lines are the commune-specific predictions, i.e. those made using all RE's specific to that commune. The dashed black line is the global model (i.e. for an "average" commune). The points are the actual pop_den ~ ForPix values for that commune. The plots support the diagnostics in the section above - the global model does not predict well for communes with high forest cover - because the global model always predicts low forest cover, even for communes with low population density.  Essentially there are too many communes with low forest cover, and so the global model is dragged down, meaning that the communes with high forest cover get poor predictions. This does suggest though that the global model predicts well at high population density values, as these communes tend to have low forest cover.
#' 
#' Now I will do the same but for 9 random selections of 9 communes.
#' 
#+ popdem.m6 commune predictions from random selection, echo=FALSE,results=TRUE, fig.width=10, fig.height=10, dpi=100
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

#' The model output suggests there is nothing going on here.  I can use 'plot_model' from the sjPlot package to quickly plot a prediction. It basically does exactly what I would do - create newdata with the variable of interest varying from the min to the max, and holding all others at their mean. You can specify whether you want it to plot conditional on the fixed effects only (i.e. "average" comune / global model), or on the random effects. The below is conditionl on fixed effects only.
#' 
#+ edu.m1 plot_model, echo=FALSE, results=TRUE
plot_model(edu.m1, type="pred", terms="M6_24_sch")

#' Flat as a pancake. 
#' 
#' Below are some commune-specific predictions from another random selection of communes
#'  
#+ edu.m1 commune predictions, echo=FALSE,results=T, fig.width=10, fig.height=10, dpi=100

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
       ylab = "Predicted forest pixels")
  } else {
  lines(preddat_i$M6_24_sch,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$M6_24_sch,preddat_i$pred.glo, lty=2)
  }
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$M6_24_sch, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}
}

#' I think it is fair to say that there is no relationship between forest cover and males in school.
#' 
#' ## Employment
#' 
#' There are two variables in this set - the proportion of the population engaged in the Primary Sector and the proportion engaged in the Secondary Sector. The primary sector includes sectors of the economy that extracts or harvests products from the earth such as raw materials and basic foods, and includes farming, fishing, mining etc. The secondary sector includes the production of finished goods from raw materials, and includes manufacturing, processing, and construction.  
#' 
#' I have no *a priori* hypothesis about an interaction between these two variables, and so I have not tested one.  It is also plausible that although these two are no correlated (-0.2), they may well be related as they are both proportions drawn from the same population, therefore including an interaction might be misleading.
#' 
#' The model:
#' 
#+ emp.m1, include=TRUE

emp.m1 <- glmer(ForPix ~ propPrimSec + propSecSec + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(emp.m1)

#' Province and commune variance similar to other model sets.  Year variance for commune is order of magnitude larger than for province, but both very small. propPrimSec has positive effect, propSecSec has negative, but both very small and approximate p values > 0.5.  The model produces an warning about large eigenvalues.  The variables are already scaled so I guess this just suggests a poor model fit. 
#' 
#' Quick plot of the effects
#' 
#+ plot_model emp.m1, echo=FALSE, results=TRUE 
plot_model(emp.m1, type="re")

plot_model(emp.m1, type="pred", terms=c("propPrimSec"))

plot_model(emp.m1, type="pred", terms=c("propSecSec"))

#' These two variables do not appear to be important.  I will try and remove propSecSec
#' 
#+ emp.m2, include=TRUE
emp.m2 <- glmer(ForPix ~ propPrimSec + offset(log(areaKM)) + (year|Province/Provcomm), family="poisson", data=dat1)

summary(emp.m2)

#' Compare the two models:
#' 
#+ anova emp.m1 and emp.m2, include=TRUE

anova(emp.m1, emp.m2, test="Chisq")

#' The simpler model is better, but is still pretty uninteresting
#' 
#' ## Economic Security
#' 
#' In this set there are two variables - the proportion of families who have less than 1 hectare of agricultural land, and the proportion of families who keep pigs.  Both of these things are fairly good proxies for a family's economic security, especially the farming land variable.
#' 
#+ econ.m1, include=TRUE

econ.m1 <- glmer(ForPix ~ Les1_R_Land + pig_fam + offset(log(areaKM)) + (year|Province/Provcomm),
                 family="poisson", data=dat1)

summary(econ.m1)


#' Very small effects with large approximate p values. Random effects similar to previous models.  Les1_R_Land has a tiny negative effect, pig_fam has a small positive effect.
#'
#' Quick plot of the main effects:
#' 
#+ plot_model econ.m1, echo=FALSE, results=TRUE

plot_model(econ.m1, type="pred", terms="Les1_R_Land")
plot_model(econ.m1, type="pred", terms="pig_fam")

#' Again, these variables do not look like they can provide much. I did run model selection, and the simpler models (i.e. with just one of the vars) were better than model 1 with both variables, but the effects were similarly tiny.
#' 
#' ## Access to Services
#' 
#' This model set has three variables - median distance to the nearest school, median distance to the Commune office, and the proportion of families with access to waste collection. 
#' 
#+ acc.m1, include=TRUE
acc.m1 <- glmer(ForPix ~ dist_sch + garbage + KM_Comm + offset(log(areaKM)) + 
                  (year|Province/Provcomm),family="poisson", data=dat1)

summary(acc.m1)

#' Also produces warning about large eigenvalues. All three variables have tiny effects with large approximate p values. 
#' 
#' Quick plots of the main effects
#' 
#+ plot_model acc.m1, echo=FALSE, results=TRUE
plot_model(acc.m1, type="pred", terms="dist_sch")
plot_model(acc.m1, type="pred", terms="garbage")
plot_model(acc.m1, type="pred", terms="KM_Comm")

#' These variables do not appear to be useful. I completed model selection using a combination of LRT's and AICc comparison, and all of the models with only an individual predictor were better than any model with more than one predictor. The top three models were therefore models with each of the single predictors, and there was virtually no difference between them (dAICc < 0.1). But they were equally as useless as the acc.m1 i.e. tiny effects, large approximate p values, flat main effect.
#' 
#' ## Social Justice
#' 
#' This set includes two predictor variables - the raw number of land conflict cases, and the per capita number of criminal cases. I had no *a priori* hypothesis about an interaction between these two variables, and so I did not test one.
#' 
#+ just.m1, include=TRUE
jus.m1 <- glmer(ForPix ~ crim_case + land_confl + offset(log(areaKM)) + (year|Province/Provcomm),
                          family="poisson", data=dat1)

summary(jus.m1)

#' Very small effects with large approximate p values. Similar RE variances to previous model sets. 
#' 
#' Quick plots of main effects:
#' 
#+ plot_model jus.m1, echo=FALSE, results=TRUE

plot_model(jus.m1, type="pred", terms="crim_case")
plot_model(jus.m1, type="pred", terms="land_confl")

#' I removed land_confl for jus.m2, and LRT suggested the simpler model was better, but the resulting odel still had tiny effect for srim_case, with large approximate p value. The plot was just as flat as jus.m1.
#' 
#' ## Migration
#' 
#' There are two predictors in this set - raw number of in-migrants and out-migrants. For these variables, I thought there was cause to test an interaction, as the relationship between migration in and out of a commune is potentially complex, and intertwined with various aspects of the local economy and natural resource use.
#' 
#+ mig.m1, include=TRUE
mig.m1 <- glmer(ForPix ~ Pax_migt_in*Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m1)

#' Very small effects and large approximate p values. No change in the RE variances
#' 
#' Quick plots of the marginal effects of interaction terms:
#' 
#+ plot_model mig.m1, echo=FALSE, results=TRUE
plot_model(mig.m1, type="pred", terms=c("Pax_migt_in","Pax_migt_out[-0.47,0,16.7]"))
plot_model(mig.m1, type="pred", terms=c("Pax_migt_out","Pax_migt_in[-0.53,0,11.1]"))

#' There appears to be a small interaction. When out-migration from a commune is large, then the effect of in-migration is positive, whereas there is no effect when out-migration is low or at its mean. Similarly, when in-migration to a commune is large, then there is a small positive effect of out-migration. There is no effect of out-migration when in-migration is low or at its mean.  Nevertheless, neither term, nor the interaction are particularly convincing. Model selection using LRT suggested that a model with just Pax_migt_out was significantly better than the model with both terms. And the resulting model has as equally unimpressive effect.
#' 
#+ mig.m2, include=TRUE
mig.m2 <- glmer(ForPix ~ Pax_migt_out + offset(log(areaKM)) + (year|Province/Provcomm),
                family="poisson", data=dat1)

summary(mig.m2)

anova(mig.m1,mig.m2,test="Chisq")
# simpler model is better

plot_model(mig.m2, type="pred", terms="Pax_migt_out")

#' ## Environmental variables
#' 
#' This set initially had two predictor variables - mean elevation and habiat. These variables were intially supposed to be "control" variables, i.e. ensuring that other, non-socioeconomic predictors that would be likely to affect forest cover, were taken into account.   I did run some models, diagnostics, and predictions using both variables, but I have decided to drop habitat altogether. This is because the layer I was using for the habitat was the same layer that produced the response - i.e. to get the "forest pixel" layer used in the response, I simply aggregated all of the forest habitat types.  Therefore I don't think it is appropriate to use the same layer as a predictor!  That leaves me with mean elevation.
#' 
#+ env.m2 elevation only, include=TRUE
env.m2 <- glmer(ForPix ~ mean_elev + offset(log(areaKM)) + (year|Province/Provcomm),
                family = "poisson", data=dat1)

summary(env.m2)

#' The RE variances have decreased slightly compared to previous model sets, suggesting that the elevation predictor is explaning more of the total variance.  Elevation appears to have a positive effect on forest pixels and appears significant (very small approximate p value).
#' 
#' ### Diagnostics
#' 
#+ env.m2 diagnostics 1, include=FALSE
# copy data
env.m2.diag <- dat1
env.m2.diag$Provcomm <- as.factor(env.m2.diag$Provcomm)

# attach residuals
env.m2.diag$m2res <- resid(env.m2)

# attach conditional predictions
env.m2.diag$m2pred <- as.vector(predict(env.m2, type="response"))

#+ env.m2 predicted vs observed plot, echo=FALSE, results=TRUE
plot(env.m2.diag$m2pred, env.m2.diag$ForPix)

#' This is the predicted versus observed plot, which looks good.

#+ env.m2 residuals vs fitted plot, echo=FALSE, results=TRUE
plot(env.m2.diag$m2pred, env.m2.diag$m2res)


#' Again heteroskedasicity is an issues here - particularly bad a low predicted forest cover. Similar to the popdem models.
#' 
#' Bit more exploration of residuals, but now over the explanatory variable:

#+ env.m2 residual plots, echo=FALSE, results=TRUE, fig.width=10, fig.height=10, dpi=100
par(mfrow=c(2,2))
plot(env.m2.diag$mean_elev, env.m2.diag$m2res, ylab = "residuals", xlab = "mean elevation")
boxplot(m2res ~ factor(Province), data = env.m2.diag, outline = T, xlab = "Province", 
        ylab = "Residuals w/i Province")
boxplot(m2res ~ factor(Provcomm), data = env.m2.diag, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")

#' There seems to be a slightly odd pattern in the first plot - it looks like there are certain elevation values that produce large residuals. Zoom in:

#+ env.m2 elevation vs residuals, echo=FALSE, results=TRUE
plot(env.m2.diag$mean_elev, env.m2.diag$m2res, xlim = c(-1,2),ylab = "residuals", xlab = "mean elevation")

#' Looks like elevation values of approx -0.6, -0.4, -0.1, 0.2, 0.8.
#' 
#' I will take a closer look at the Provinces that have the communes that appear to produce the largest residuals.
#' 
#+ env.m2 provinces with largest residuals, echo=FALSE, results=TRUE 

provs <- c("Battambang","Kampong Cham","Kampong Chhnang","Kampong Thom","Koh Kong","Kracheh",
           "Mondul Kiri","Otdar Meanchey","Preah Sihanouk","Pursat","Ratanak Kiri","Siem Reap",
           "Stung Treng")
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
  theme(element_blank())+
  ylab("Mean elevation")

#' The above plot shows the mean elevation (on the original scale) for each Province, with the "problem" provinces (i.e. the ones with the largest residuals) coloured blue. There is not a stiking trend, but the 4 provinces with the lowest mean elevation are all *not* problematic ones. 
#' 
#' Now I will pull out the individual communes that have residual values that match the weird pattern and see if there is any other pattern that might explain it.

#+ env.m2 prob.com, echo=FALSE, results=TRUE
par(mfrow=c(1,1))
prob.coms <- env.m2.diag %>% filter(m2res > 0.5 | m2res < -0.5)

plot(prob.coms$mean_elev, prob.coms$m2res)

#' These are now the communes with the weird shape. Let's have a look that these communes and their mean elevaiton (on the original scale), compared with all other communes:
#' 
#+ env.m2 prob.coms, echo=FALSE, results=TRUE 
coms <- prob.coms$Commune
prob.coms.orig <- dat[dat$Commune %in% coms,]
other.coms.orig <- dat %>% filter(!Commune %in% coms)

ggplot()+
  geom_point(data=other.coms.orig, aes(x=Commune, y=mean_elev, colour="other"))+
  geom_point(data=prob.coms.orig, aes(x=Commune, y=mean_elev, colour="problem"))+
  theme(element_blank())+
  ylab("Mean elevation")

#' I can't see an obvious pattern here. The other fixed effect is the offset - areaKM.  Perhaps this is the source of the issue:
#' 
#+ env.m2 prob.com - areaKM plot, echo=F, results=T
ggplot()+
  geom_point(data=other.coms.orig, aes(x=Commune, y=areaKM, colour="other"))+
  geom_point(data=prob.coms.orig, aes(x=Commune, y=areaKM, colour="problem"))+
  theme(element_blank())+
  ylab("Area (KM)")

#' No obvious pattern. Let's look at the response (ForPix):
#' 
#+ env.m2 prob.com ForPix plot, echo=F, results=T
ggplot()+
  geom_point(data=other.coms.orig, aes(x=Commune, y=ForPix, colour="other"))+
  geom_point(data=prob.coms.orig, aes(x=Commune, y=ForPix, colour="problem"))+
  theme(element_blank())+
  ylab("Forest pixels")

#' Ok so there is an obvious difference here.  The problem communes clearly mostly lose forest over time (vertical lines of dots), whereas the others generally do not (single points). This is the same issue as highlighted in the popdem model. So the global model does not fit well when communes lose forest over time.
#' 
#' ### Predict main effects 
#' 
#+ env.m2 predict main effects, echo=FALSE, results=TRUE
env_m2_newdata <- data.frame(mean_elev = seq(from=min(dat1$mean_elev), to=max(dat1$mean_elev),
                                             length.out = 100),
                             areaKM = mean(dat1$areaKM))
# predict
env_m2_newdata$pred <- as.vector(predict(env.m2, type="response", newdata=env_m2_newdata, 
                                         re.form=NA))

# plot with free y axis
env.m2.main.plot <- ggplot(env_m2_newdata, aes(x=mean_elev, y=pred))+
                    geom_line()+
                    theme(element_blank())+
                    xlab("Mean elevation (scaled)")+
                    ylab("Predicted forest cover (pixels)")

# plot
env.m2.main.plot2 <- ggplot(env_m2_newdata, aes(x=mean_elev, y=pred))+
                      geom_line()+
                      theme(element_blank())+
                      ylim(0,20000)+
                      xlab("Mean elevation (scaled)")+
                      ylab("Predicted forest cover (pixels)")

env.m2.main.plot + env.m2.main.plot2

#' The above two plots show the predicted effects of mean elevation for an "average" commune (i.e. not commune-specific RE's), with a free y-axis (left) and with a realistic y axis (right).  We can see that mean elevation has a positive effect on forest cover. I was expecing this relationship. Most of the highly forested areas in Cambodia are in the provinces with higher elevation.
#' 
#' ### Predict for specific commmunes
#' 
#' I will start by predicting for the 4 communes with intercepts closest to 0, the 4 communes with intercepts furthest above 0, and the communes with intercepts furthest below 0.
#' 
#+ env.m2 commune predictions, echo=FALSE, results=TRUE, fig.width=8, fig.height=8, dpi=100

env.m2.com <- ranef(env.m2)[[1]]

# re-order
env.m2.com <- env.m2.com[order(env.m2.com[ ,"(Intercept)"], decreasing = FALSE),]

# which communes
coms <- c("Koh Kong_Bak Khlang","Kracheh_Bos Leav","Preah Vihear_Kuleaen Tboung","Ratanak Kiri_Saom Thum",
          "Kampong Cham_Tuol Snuol","Banteay Meanchey_Paoy Char","Kampong Cham_Khpob Ta Nguon","Kampong Chhnang_Dar",
          "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Kampong Thom_Tang Krasang","Siem Reap_Nokor Pheas")

# which provinces
provs <- c("Koh Kong","Kracheh","Preah Vihear","Ratanak Kiri",
           "Kampong Cham","Banteay Meanchey","Kampong Cham","Kampong Chhnang",
           "Kampong Thom","Kracheh","Kampong Thom","Siem Reap")


# customise the range of elevations I am predicting for, on the basis of each commune.

### Easiest to define the range of mean_elev to predict for, first. Min/max per commune:
mean_elev_min <- tapply(dat1$mean_elev, dat1$Provcomm, min)
mean_elev_max <- tapply(dat1$mean_elev, dat1$Provcomm, max)
### Min/max within your selection of communes:
mean_elev_min <- min(mean_elev_min[names(mean_elev_min) %in% coms])
mean_elev_max <- max(mean_elev_max[names(mean_elev_max) %in% coms])

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

# colours
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
       ylab = "Predicted forest pixels",
       main = unique(preddat_i$Provcomm))
  # } else {
  lines(preddat_i$mean_elev,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$mean_elev,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$mean_elev, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

#' In the above plots, the top row are the communes with intercepts clsoest to 0, middle row are those with intercepts furthest above 0, and bootm row are those with intercepts furthest below 0. The dashed black line is the global model, and the coloured solid lines are the commune-specific predictions. The dots are the actual ForPix ~ mean elevation points for that commune. 
#' We can see that as for the population demographics model, the global model predicts poorly for communes with high forest cover, regardless of mean elevation. If a commune has low forest cover, then the global model predicts well for increasing values of elevation, until elevation reaches a scaled value of ~1, after which the model predicts poorly (becasue a commune can't increase in elevation).  
#' 
#' Let's do the same predictions but for a random set of communes: 
#' 
#+ env.m2 commune predictions - random, echo=FALSE, results=TRUE,fig.width=8, fig.height=8, dpi=100

par(mfrow = c(3,3))
set.seed(123)
runs <- c(1:9)

for(i in 1:length(runs)){
# randomly sample communes
rand.com <- sample(dat1$Provcomm, 9, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17)]

# define the range of pop_den to predict for, first. Min/max per commune:
mean_elev_min <- tapply(dat1$mean_elev, dat1$Provcomm, min)
mean_elev_max <- tapply(dat1$mean_elev, dat1$Provcomm, max)
# Min/max within your selection of communes:
mean_elev_min <- min(mean_elev_min[names(mean_elev_min) %in% rand.com])
mean_elev_max <- max(mean_elev_max[names(mean_elev_max) %in% rand.com])
# min not really different but max very different

# create new prediction grid for specific communes with varying pop_den
env.m2_newdat_com_ran <- data.frame(Provcomm = rep(rand.com, each=100),
                                Province = rep(rand.prov, each=100),
                                mean_elev = seq(from=mean_elev_min, to=mean_elev_max, length.out = 100),
                                year = mean(dat1$year))


# add commune-specific areaKM offset                         
env.m2_newdat_com_ran$areaKM <-  dat1$areaKM[match(env.m2_newdat_com_ran$Provcomm, dat1$Provcomm)]


# attach commune-specific predictions
env.m2_newdat_com_ran$pred.com <- as.vector(predict(env.m2, type="response", 
                                    newdata=env.m2_newdat_com_ran, 
                                    re.form=~(year|Province/Provcomm)))


# attach global predictions
env.m2_newdat_com_ran$pred.glo <- rep(env_m2_newdata$pred, times=9)

# set levels
env.m2_newdat_com_ran$Provcomm <- as.factor(env.m2_newdat_com_ran$Provcomm)
provcomm_lvls <- levels(env.m2_newdat_com_ran$Provcomm) 

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
  if(i == 1) {
  # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$mean_elev,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Mean elevation (scaled & standardised",
       ylab = "Predicted forest pixels",
       main = unique(preddat_i$Provcomm))
  } else {
  lines(preddat_i$mean_elev,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$mean_elev,preddat_i$pred.glo, lty=2)
  }
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$mean_elev, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}
}

#' Each plot contains a random sample of 9 communes. Again the black dashed line is the global model. Note the varying y-axes. After running the above random sampling code a few times, it looks as though the global model is good at predicting at low forest cover and low elevation (as with the population density model), as this is where most of the communes sit on the spectrum.  It performs ok for communes with higher elevation provided they don't have too much forest!  As soon as the commune has lots of forest or very high elevation, the global model performs badly.  It looks as though the commune-specific models predict quite well, provided the communes does not lose forest (as with the population density model).
#' 
#' ## Additional human predictor variables
#' 
#' As with the environmental predictor above, these variables were supposed to be "control" variables. The variables included in this set are distance to border (from the centre of the commune), distance to the Provincial Capital (from the centre of the commune), presence of economic land concessions ("elc", binary), presence of a PA (any kind of PA, binary), PA category (7 levels, including "none").
#' 
#+ hum.m1, include=TRUE
hum.m1 <- glmer(ForPix ~ dist_border+dist_provCap+elc+PA+PA_cat+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

summary(hum.m1)

#' Model produces a rank deficiency warning. Interestingly, PA doesn't appear to look important.  This surprises me as you would assume that PAs would be placed in areas with high forest cover. elc doesn't appear to be important, which is also surprising because I could imagine two scenarios 1) it would have a postive relationship because elc's were often placed in forested areas, and 2) a negative relationship because the elc's would have cleared a lot of forest in the areas they were placed.  However, during this time period, perhaps not much forest clearing had happened yet. dist_border and dst_provCap appear to be important, with both having a positive effect. dist_provCap I can understand - communes further away from urban centres are likely to be more forested. Not sure yet about dist_border. I think all vars require further investigation. 
#' 
#' I will do some quick plots below:
#'  
#+ hum.m1 plot_models, echo=FALSE, results=TRUE, warning=F, fig.width=8, fig.height=8, dpi=100
# quick plots
hum.p1 <- plot_model(hum.m1, type="pred", terms="dist_border")
hum.p2 <- plot_model(hum.m1, type="pred", terms="dist_provCap")
hum.p3 <- plot_model(hum.m1, type="pred", terms="elc")
hum.p4 <- plot_model(hum.m1, type="pred", terms="PA")
hum.p5 <- plot_model(hum.m1, type="pred", terms="PA_cat") # surprised RMS level is not sig given the coefficient

hum.p1+hum.p2+hum.p3+hum.p4+hum.p5 

#' From the quick plots above, it looks like both dist_border and dist_provCap do have a relationship with forest cover.  elc does not look promising (note the y axis - differnece is tiny), and neither really does PA.  Not much difference in predicted forest cover for the different PA categories, apart from the RMS level.   
#' 
#' I conducted LRT's and AICc comparisons, and moved forward with predictions and plotting for models with dist_border, dist_provCap, PA and elc.  But it became clear that PA and elc did very little, and so I have settled on a model with just dist_border and dist_provCap. 
#' 
#' ### Diagnostics
#' 
#+ hum.m5 pred and res and plot observerd vs predicted, echo=FALSE, results=TRUE
# copy data for diagnostics

hum.m5 <- glmer(ForPix ~ dist_border+dist_provCap+offset(log(areaKM)) +
                  (year|Province/Provcomm), family = "poisson", data=dat1)

hum.diag.dat <- dat1

# residuals
hum.diag.dat$m5res <- resid(hum.m5)

# conditional predictions
hum.diag.dat$m5.pred <- as.vector(predict(hum.m5, type="response", re.form=NA))

plot(hum.diag.dat$m5.pred, hum.diag.dat$ForPix)

#' The above plot is the observed values versus the predicted values (from a fully conditional model).  This plot is not great - worse than the previous model sets, and it looks like the model is under-predicting by quite a long way.  
#' 
#+ hum.m5 plot residuals versus predicted, echo=FALSE, results=TRUE
plot(hum.diag.dat$m5.pred, hum.diag.dat$m5res)

#' This doesn't look great. Some outlier large predictions which have small residuals, but a lot of heterogeneity at smaller predicted values. There's an odd line of residuals just below 2000 (x axis) which suggests there's one predicted value that is appearing quite a few times?  Below I look more closely at the residuals.
#' 
#+ hum.m5 residual plots, echo=FALSE, results=TRUE, fig.width=10, fig.height=10, dpi=100
par(mfrow=c(2,2))
plot(hum.diag.dat$dist_border, hum.diag.dat$m5res, ylab = "residuals", xlab = "distance to border")
plot(hum.diag.dat$dist_provCap, hum.diag.dat$m5res, ylab = "residuals", xlab = "distance to Prov Cap")
boxplot(m5res ~ factor(Province), data = hum.diag.dat, outline = T, xlab = "Province", 
        ylab = "Residuals w/i Province")
boxplot(m5res ~ factor(Provcomm), data = hum.diag.dat, outline = T, xlab = "Commune", 
        ylab = "Residuals w/i Commune")

#' Based on the first two plots, it looks like there's only a relatively small number of communes that have really large residuals (and there seems to be patterns in these).  
#' 
#' Zoom in:
#' 
#+ hum.m5 residual plots zoom, echo=FALSE, results=TRUE, fig.width=10, fig.height=10, dpi=100
par(mfrow=c(2,1))
plot(hum.diag.dat$dist_border, hum.diag.dat$m5res, ylim=c(-3,3),
     ylab = "residuals", xlab = "distance to border")
plot(hum.diag.dat$dist_provCap, hum.diag.dat$m5res, ylim=c(-3,3),
     ylab = "residuals", xlab = "distance to Prov Cap")

#' The slightly odd patterns are smaller residuals between 0 and 1 dist_border, and between probably 0 and 0.3 for dist_provCap.
#'  
#' I had a look at which provinces have the larger residuals to see if they match with the problem provinces from the previous model sets.  They are Battambang, Kampong Cham, Kampong Chhnanhg, Kampong Thom, Koh Kong, Kracheh, Mondul Kiri, Otdar Meanchey, Pursat, Ratanak Kiri, Siem Reap, Stung Treng. These are the same provinces that are causing issues in the other model sets.
#' 
#' As I have mentioned before, I think these problem communes are the ones that are losing forest over time.  I used the raw data to assess which Provinces contain communes that lose some forest over the study period.
#' 
#+ hum.m5 Provinces that lose forest, echo=FALSE, results=TRUE
diffPix <- dat1 %>% group_by(Provcomm) %>% summarise(sum = sum(diffPix))
provs <- unlist(strsplit(diffPix$Provcomm, "_"))
provs1 <- provs[seq(1, length(provs), 2)]
diffPix$Province <- provs1

unique(diffPix$Province[diffPix$sum > 0])

#' So this may go some way towards explaining the issues. However, there are still Provinces that lose no forest but are still causing issues, such as Kampong Chhnang and Kampong Thom. I looked more closely at some of the individual communes that had very large residuals.
#' 
#+ hum.m5 communes larege residusals, echo=FALSE, results=TRUE
prob.coms <- hum.diag.dat[hum.diag.dat$m5res > 1 | hum.diag.dat$m5res < -1,]
prob.coms$type <- "problem"
other.coms <- hum.diag.dat %>% filter(!Provcomm %in% prob.coms$Provcomm)
other.coms$type <- "other"
all.coms <- rbind(prob.coms, other.coms)


plot(prob.coms$m5.pred, prob.coms$m5res)

#' The residuals above are the largest ones. Below, I have split all of the communes into the ones that produced the residuals above, and the rest:
#' 
#+ hum.m5 prob coms vs ForPix, echo=F, results=T, fig.width=10, fig.height=10, dpi=100
hum.plot1 <- ggplot(all.coms, aes(x=Provcomm, y=ForPix, colour=type))+
              geom_point()+
              theme(element_blank())+
              labs(x="Commune", y="Forest pixels")

hum.plot2 <- ggplot(all.coms, aes(x=Provcomm, y=areaKM, colour=type))+
              geom_point()+
              theme(element_blank())+
              labs(x="Commune", y="Commune area (KM)")

hum.plot3 <- ggplot(all.coms, aes(x=Provcomm, y=dist_border, colour=type))+
              geom_point()+
              theme(element_blank())+
              labs(x="Commune", y="Distance to border")

hum.plot4 <- ggplot(all.coms, aes(x=Provcomm, y=dist_provCap, colour=type))+
              geom_point()+
              theme(element_blank())+
              labs(x="Commune", y="Distance to Provincial Captital")

hum.plot1+hum.plot2+hum.plot3+hum.plot4

#' The top left plot is forest pixels per commune, the top right plot is the area (KM) of the communes, the bottom left plot is distance to border, and the bottom right is distance to provincial capital. None of these show an obvious pattern to me - and in fact in the top left plot (forest pixels) the loss of forest over time doesn't look unique to the problem communes, as it did in the previous sets.  The only pattern I can see in these plots is that the problem communes are spatially relatively close and clumped (they are grouped by Province in the dataframe and so if they are plotted near one another on the x axis they are probably in the same Province). But the issues are certainly not unique to a single province, or to a set of communes all with similar characteristics (that I can think of anyway!).  So am not sure exactly what the other reasons are for the large residuals in those communes.
#' 
#' ### Predict main effects
#' 
#' Below are the predictions for dist_border and dist_provCap from the 'global' model.
#' 
#+ hum.m5 main effects predictions, echo=F, results=T

# create new data for dist_border
dist_border_newdat <- expand.grid(dist_border = seq(min(dat1$dist_border), max(dat1$dist_border), 
                                                    length.out = 100),
                                  dist_provCap = mean(dat1$dist_provCap),
                                  areaKM = mean(dat1$areaKM))

# predict
dist_border_newdat$pred <- as.vector(predict(hum.m5, type="response", newdata=dist_border_newdat, re.form=NA))

# create new data for dist_provCap
dist_provCap_newdat <- expand.grid(dist_provCap = seq(min(dat1$dist_provCap), max(dat1$dist_provCap), 
                                                      length.out = 100),
                                   dist_border = mean(dat1$dist_border),
                                   areaKM = mean(dat1$areaKM))

# predict
dist_provCap_newdat$pred <- as.vector(predict(hum.m5, type="response", newdata=dist_provCap_newdat, 
                                              re.form=NA))



# plot
hum.p6 <-  ggplot(dist_border_newdat, aes(x=dist_border, y=pred))+
  geom_line(size=1)+
  theme(element_blank())+
  ylim(0,2000)+
  xlab("Distance to international border (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")+
  ggtitle("Distance to border")

hum.p7 <-ggplot(dist_provCap_newdat, aes(x=dist_provCap, y=pred))+
  geom_line(size=1)+
  theme(element_blank())+
  ylim(0,2000)+
  xlab("Distance to provincial capital (scaled and centered)")+
  ylab("Predicted forest cover (pixels)")+
  ggtitle("Distance to provincial captial")

hum.p6 + hum.p7

#' We can see that both predictors have a positive effect on forest cover, and that distance to provincial capital is the stronger effect. The relationship with dist_provCap makes sense - the more rural/remote a communes is, the more likely it is to be forested. Communes that are in or around large urban centres, are unlikely to be heavily forested. The dist_border relationship is more difficult to explain easily. Based on my knowledge of the country I was expecting the opposite - lots of the large PAs are near international borders, whereas much of the central part of the country is farmland. I will need to investigate this further.
#' 
#' ### Predict for specific communes
#' 
#' Below I have run predictions for 12 communes - the four with intercepts closest to 0, four with intercepts furthest above 0, and the four with intercepts furthest below 0.
#' 
#' First for dist_border
#' 
#+ hum.m5 commune predictions dist_border, echo=F, results=T,  fig.width=8, fig.height=8, dpi=100

hum.m5.com <- ranef(hum.m5)[[1]]

# re-order
hum.m5.com <- hum.m5.com[order(hum.m5.com[ ,"(Intercept)"], decreasing = TRUE),]

# which communes
coms <- c("Stung Treng_Kbal Romeas","Preah Vihear_Chhaeb Pir","Kampong Cham_Kampoan","Preah Vihear_Kuleaen Tboung",
          "Kampot_Preaek Tnaot","Kampot_Kaoh Touch","Kampong Chhnang_Chieb","Kampong Cham_Pongro",
          "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Preah Vihear_Reaksmei","Siem Reap_Nokor Pheas")

# which provinces
provs <- c("Stung Treng","Preah Vihear","Kampong Cham","Preah Vihear",
           "Kampot","Kampot","Kampong Chhnang","Kampong Cham",
           "Kampong Thom","Kracheh","Preah Vihear","Siem Reap")


###  define the range of dist_border to predict for. Min/max per commune:
dist_border_min <- tapply(dat1$dist_border, dat1$Provcomm, min)
dist_border_max <- tapply(dat1$dist_border, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_border_min <- min(dist_border_min[names(dist_border_min) %in% coms])
dist_border_max <- max(dist_border_max[names(dist_border_max) %in% coms])

# create new prediction grid for specific communes with varying dist_border
hum_m5_newdat_bord <- data.frame(Provcomm = rep(coms, each=100),
                                 Province = rep(provs, each=100),
                                  dist_border = seq(dist_border_min, dist_border_max, length.out = 100),
                                  year = mean(dat1$year))

# add dist_provCap
hum_m5_newdat_bord$dist_provCap <- dat1$dist_provCap[match(hum_m5_newdat_bord$Provcomm, dat1$Provcomm)]


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
       ylab = "Predicted forest pixels",
       main = unique(preddat_i$Provcomm))
  #} else {
  lines(preddat_i$dist_border,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$dist_border,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$dist_border, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

#' These plots suggest the global model fits poorly for communes with higher forest cover, but fits better for communes with low forest cover, regardless of dist_border, which is much the same as previous models. The global model is better at predicting for communes with intercpets around the global mean, or below it. 
#' 
#' Now I will do the same plots but for a random subset of communes.
#' 
#+ hum.m5 random commune predictions dist_border, echo=F, results=T, fig.width=8, fig.height=8, dpi=100

par(mfrow = c(3,3))
set.seed(123)
runs <- c(1:9)

for(i in 1:length(runs)){
# randomly sample communes
rand.com <- sample(dat1$Provcomm, 9, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17)]

# define the range of pop_den to predict for, first. Min/max per commune:
dist_border_min <- tapply(dat1$dist_border, dat1$Provcomm, min)
dist_border_max <- tapply(dat1$dist_border, dat1$Provcomm, max)
# Min/max within your selection of communes:
dist_border_min <- min(dist_border_min[names(dist_border_min) %in% rand.com])
dist_border_max <- max(dist_border_max[names(dist_border_max) %in% rand.com])
# min not really different but max very different

# create new prediction grid for random communes with varying dist_border
hum_m5_newdat_bord_ran <- data.frame(Provcomm = rep(rand.com, each=100),
                                 Province = rep(rand.prov, each=100),
                                  dist_border = seq(dist_border_min, dist_border_max, length.out = 100),
                                  year = mean(dat1$year))

hum_m5_newdat_bord_ran$Provcomm <- as.factor(hum_m5_newdat_bord_ran$Provcomm)


# add dist_provCap
hum_m5_newdat_bord_ran$dist_provCap <- dat1$dist_provCap[match(hum_m5_newdat_bord_ran$Provcomm, dat1$Provcomm)]


# add commune-specific areaKM offset                         
hum_m5_newdat_bord_ran$areaKM <-  dat1$areaKM[match(hum_m5_newdat_bord_ran$Provcomm, dat1$Provcomm)]

# attach commune-specific predictions
hum_m5_newdat_bord_ran$pred.com <- as.vector(predict(hum.m5, type="response", newdata=hum_m5_newdat_bord_ran, 
                                                 re.form=~(year|Province/Provcomm)))


# attach global predictions. need to alter areaKM below so that the global model has only the mean areakM rather than the commune-specific one. This is fine as pred.com is already done
hum_m5_newdat_bord_ran <- hum_m5_newdat_bord_ran %>% rename(areaKM_com = areaKM)
hum_m5_newdat_bord_ran$areaKM <- mean(dat1$areaKM)
hum_m5_newdat_bord_ran$pred.glo <- as.vector(predict(hum.m5,type="response",
                                                     newdata=hum_m5_newdat_bord_ran,re.form=NA))

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
provcomm_lvls <- levels(hum_m5_newdat_bord_ran$Provcomm) 

### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### dist_border range across all communes, so we need to do this overall:
ylo <- min(hum_m5_newdat_bord_ran$pred.com)*0.9
yhi <- max(hum_m5_newdat_bord_ran$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_bord_ran$Provcomm),"dist_border"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_bord_ran$Provcomm),"dist_border"])
### Iterate through the communes (levels in hum_m5_newdat_bord$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- hum_m5_newdat_bord_ran[hum_m5_newdat_bord_ran$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  if(i == 1) {
  # Plot predicted ForPix as function of dist_border; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$dist_border,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Distance to intl border (scaled & standardised)",
       ylab = "Predicted forest pixels",
       main = unique(preddat_i$Provcomm))
  } else {
  lines(preddat_i$dist_border,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$dist_border,preddat_i$pred.glo, lty=2)
  }
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$dist_border, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}
}


#' The above plots I think show that again, the commune-specific models are fitting well, provided forest cover doesn't change.  
#' 
#' Below I will do the same predictions and plotting, but for varying dist_provCap.  First, I will plot the 12 communes above, below, and around the mean intercept.
#' 
#+ hum.m5 commune predictions dist_provCap, echo=F, results=T,  fig.width=8, fig.height=8, dpi=100

###  define the range of dist_provCap to predict for. Min/max per commune:
dist_provCap_min <- tapply(dat1$dist_provCap, dat1$Provcomm, min)
dist_provCap_max <- tapply(dat1$dist_provCap, dat1$Provcomm, max)
### Min/max within your selection of communes:
dist_provCap_min <- min(dist_provCap_min[names(dist_provCap_min) %in% coms])
dist_provCap_max <- max(dist_provCap_max[names(dist_provCap_max) %in% coms])

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
       ylab = "Predicted forest pixels",
       main = unique(preddat_i$Provcomm))
  #} else {
  lines(preddat_i$dist_provCap,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$dist_provCap,preddat_i$pred.glo, lty=2)
  #}
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$dist_provCap, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}

#' The above plots paint a similar picture to the dist_border plots.  Below I will predict and plot for a random subset of communes.
#' 
#+ hum.m5 random commune predictions dist_provCap, echo=F, results=T,  fig.width=8, fig.height=8, dpi=100

par(mfrow = c(3,3))
set.seed(123)
runs <- c(1:9)

for(i in 1:length(runs)){
# randomly sample communes
rand.com <- sample(dat1$Provcomm, 9, replace = FALSE)
rand.prov <- unlist(strsplit(rand.com, "_"))
rand.prov <- rand.prov[c(1,3,5,7,9,11,13,15,17)]

# define the range of pop_den to predict for, first. Min/max per commune:
dist_provCap_min <- tapply(dat1$dist_provCap, dat1$Provcomm, min)
dist_provCap_max <- tapply(dat1$dist_provCap, dat1$Provcomm, max)
# Min/max within your selection of communes:
dist_provCap_min <- min(dist_provCap_min[names(dist_provCap_min) %in% rand.com])
dist_provCap_max <- max(dist_provCap_max[names(dist_provCap_max) %in% rand.com])
# min not really different but max very different

# create new prediction grid for random communes with varying dist_border
hum_m5_newdat_provCap_ran <- data.frame(Provcomm = rep(rand.com, each=100),
                                 Province = rep(rand.prov, each=100),
                                  dist_provCap = seq(dist_provCap_min, dist_provCap_max, length.out = 100),
                                  year = mean(dat1$year))

hum_m5_newdat_provCap_ran$Provcomm <- as.factor(hum_m5_newdat_provCap_ran$Provcomm)


# add dist_border
hum_m5_newdat_provCap_ran$dist_border <- dat1$dist_border[match(hum_m5_newdat_provCap_ran$Provcomm, dat1$Provcomm)]


# add commune-specific areaKM offset                         
hum_m5_newdat_provCap_ran$areaKM <-  dat1$areaKM[match(hum_m5_newdat_provCap_ran$Provcomm, dat1$Provcomm)]

# attach commune-specific predictions
hum_m5_newdat_provCap_ran$pred.com <- as.vector(predict(hum.m5, type="response", 
                                                        newdata=hum_m5_newdat_provCap_ran, 
                                                 re.form=~(year|Province/Provcomm)))


# attach global predictions. need to alter areaKM below so that the global model has only the mean areakM rather than the commune-specific one. This is fine as pred.com is already done
hum_m5_newdat_provCap_ran <- hum_m5_newdat_provCap_ran %>% rename(areaKM_com = areaKM)
hum_m5_newdat_provCap_ran$areaKM <- mean(dat1$areaKM)
hum_m5_newdat_provCap_ran$pred.glo <- as.vector(predict(hum.m5,type="response",
                                                     newdata=hum_m5_newdat_provCap_ran,re.form=NA))

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
provcomm_lvls <- levels(hum_m5_newdat_provCap_ran$Provcomm) 

### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### dist_border range across all communes, so we need to do this overall:
ylo <- min(hum_m5_newdat_provCap_ran$pred.com)*0.9
yhi <- max(hum_m5_newdat_provCap_ran$pred.com)*1.1
xlo <- min(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_provCap_ran$Provcomm),"dist_provCap"])
xhi <-  max(dat1[dat1$Provcomm %in% levels(hum_m5_newdat_provCap_ran$Provcomm),"dist_provCap"])
### Iterate through the communes (levels in hum_m5_newdat_bord$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i <- hum_m5_newdat_provCap_ran[hum_m5_newdat_provCap_ran$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i <- dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  if(i == 1) {
  # Plot predicted ForPix as function of dist_provCap; as "line" type. Note this is where you set axis limits.
  plot(preddat_i$dist_provCap,preddat_i$pred.com, 
       type = "l", 
       col = com_colours[i], 
       ylim = c(ylo,yhi), xlim = c(xlo,xhi),
       xlab = "Distance to Provincial capital (scaled & standardised)",
       ylab = "Predicted forest pixels",
       main = unique(preddat_i$Provcomm))
  } else {
  lines(preddat_i$dist_provCap,preddat_i$pred.com, col = com_colours[i])
  lines(preddat_i$dist_provCap,preddat_i$pred.glo, lty=2)
  }
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$dist_provCap, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}
}


