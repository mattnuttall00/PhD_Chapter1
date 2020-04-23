#Mixed effects models
#AdvInR

#Script developed by Nils Bunnefeld, and modified by Luc Bussière November 2014, with further subsequent edits by Tom Houslay, Ane Timenes Laugen and Luc Bussiere

# last modified Oct 21, 2019

# clear workspace
rm(list=ls())

library(arm)
library(sjPlot)
library(DHARMa)
library(tidyverse)
library(lme4)
library(Matrix)
library(rcpp)
library(psych)

# analysis of grouse
# import data file
GROUSE<-read_csv("grouse_shooting_mod.csv")
str(GROUSE)

# your first step will need to be recoding some variables as factors
GROUSE$moor<-factor(GROUSE$moor)
GROUSE$drive<-factor(GROUSE$drive)


# now we will want to divide nr_shot by drive size to facilitate contrasts
GROUSE$shotperkm<-GROUSE$nr_shot/GROUSE$km2
# then examine distribution
hist(GROUSE$shotperkm)
# variable is bounded below and count data
# to keep things simple, let's see if a log transform will help
hist(log(GROUSE$shotperkm))
# looks like this will be better even if not perfect
# let's store the vector for later use
GROUSE$shot<-log(GROUSE$shotperkm)

# the counts will similarly need a log transform
GROUSE$count<-log(GROUSE$totalcount)

# now plot the relationship and predict effects
plot(shot~count,data=GROUSE)
# looks like a positive linear relationship (in log-log space), with slope approx 1
plot(shot~jitter(prev),data=GROUSE)
# looks like a negative relationship, maybe with slope around -0.5??

# but recall that we would really like to know ahead of time whether there is an interaction
# let's build a plot that separately illustrates the effects of previous shooting number on count
library(ggplot2)
ggplot(GROUSE, aes(x = count, y = shot)) + 
  geom_point() + 
  stat_smooth(method = "lm") +
  facet_grid(. ~ prev)
# there is no strong evidence from these plots for an interaction, because the lines are more or less parallel
# however, it does look like the numbers of shot grouse are highest in the first shooting, followed by the second and third 


# to start, let's keep things simple with the maximal linear model
g1<-lm(shot~count*prev,data=GROUSE)
par(mfrow=c(2,2))
plot(g1)
par(mfrow=c(1,1))
# QQ plot looks a bit weak, others OK
summary(g1)
# guess at coefficient for count not bad, but other suffers -- however interaction still in so model needs simplifying

g2<-update(g1,~. -count:prev)
anova(g1,g2)
# simpler model better, as expected

par(mfrow=c(2,2))
plot(g2)
par(mfrow=c(1,1))
# QQ maybe slightly better, still a bit poor
summary(g2)
# now guess for coefficients quite good, and supported statistically

# can't simplify further, because p-vals for linear models are already for p-on-deletion

# problems is that this linear model features pseudoreplication
# both moor and drive visited repeatedly, and would like to soak up the variance assoc with these factors

# but having done the fixed version of the model gives us a good idea of what to expect from a successful mixed mode, so it was worth starting there!


#Extending the linear model to account for pseudoreplication

#We know that we also have repeated measurements for some drives (hunting area) and drive is nested within moor (management unit owned by a single land owner). We need to address this by specifying the random effects in the model. 

# What if we didn't use mixed effects models and added moor and drive as fixed effects? Let's try it out. 

g4<-lm(shot~moor+drive+prev+count,data=GROUSE)
summary(g4)

#Why are there NA estimates in the summary output?
# Try these tables which tell you how many of each combination of moor and drive (and previous numbers of shootings) there are
xtabs(~moor+drive,data=GROUSE)
xtabs(~moor+drive+prev,data=GROUSE)

# most combinations are poorly represented, and the small overall sample means we don't have the degrees of freedom to estimate all the coefficients needed for a fixed model

#Let's try the same analysis in a mixed effect model framework. In mixed effects models, we need to specify the random part of the model.
# I prefer the syntax of lme4 to that of nlme, but both are OK
library(lme4)

g1.mixed<-lmer(shot~prev*count+(1|moor)+(1|drive),data=GROUSE)

# examine diagnostics
plot(g1.mixed)
hist(residuals(g1.mixed))
qqnorm(residuals(g1.mixed))
plot_model(g1.mixed, type = "diag")

summary(g1.mixed)

# notice how there are no p-values, because it's not clear what denominator df are in mixed models

# but R will let us assess significance by simplifying, even though there is substantial debate about whether this is a good method for judging significance of terms
g2.mixed<-update(g1.mixed,~. - prev:count)
anova(g1.mixed,g2.mixed)
# notice the message in red telling us that R has refit the models using a different estimation procedure -- we compare models fit with ML, but report coefficients fit with REML
# the simpler model is preferred (if we believe the likelihood ratio test method for testing significance)
# but behaviour of chi squared biased, so best to apply Kenward Roger correction or conduct parametric bootstraps
# both implemented in package pbkrtest
#######
# insert pbkrtest
# when using pbkrtest package, always ensure the simpler model is listed second, or else the function will fail

library(pbkrtest)
kr.b<-KRmodcomp(g1.mixed,g2.mixed)
summary(kr.b)
# The Kenward-Roger approximation is often more conservative

pb.b<-PBmodcomp(g1.mixed,g2.mixed,nsim=500)
summary(pb.b)
# p-vals similar using a parametric bootstrap



summary(g2.mixed)
# can we simplify further?
g3.mixed<-update(g2.mixed,~. - prev)
kr.c<-KRmodcomp(g2.mixed,g3.mixed)
summary(kr.c)
# OR
pb.c<-PBmodcomp(g2.mixed,g3.mixed,nsim=500)
summary(pb.c)
# need to retain prev



g4.mixed<-update(g2.mixed,~. - count)
anova(g2.mixed,g4.mixed)

kr.d<-KRmodcomp(g2.mixed,g4.mixed)
summary(kr.d)
# OR
pb.d<-PBmodcomp(g2.mixed,g4.mixed,nsim=500)
summary(pb.d)
# can't lose count either
# if we care about p-values, we can use either KR or pb values


# so min adequate model is g2.mixed
summary(g2.mixed)
# coeffs similar but not identical to those in fixed model
# also similar to published table, but note that Bunnefeld et al model shooting event number as a factor
# They may have reasoned that because the 3 or 4 shootings category was binned, modelling prev as a factor did not constrain the outcome of that group's coefficient as much as if it were a continuous predictor. In fact, there is little difference between models.


# why are df known for model simp, but not for tests of coefs?

# could also compare models with AIC
# because sample is small, may prefer AICc
# may need to install MuMIn, which has one implementation of AICc
library(MuMIn)

AICc(g1.mixed,g2.mixed,g3.mixed,g4.mixed)
# methods are in agreement
# best model is g2.mixed

#The summary looks like the one from linear models, but we have additional parts here. The most important are the variances of the random effects and th enumber of observations to check that we have done the right thing. Try to understand the three different parts, the random effects, the fixed effects and the data structure (Number of obs).



#Now do a variance components analysis, which is basically extracting the variance from the random effects and calculating the relative contribution of each term of the random effects to the total variation explained by the random effects. Extract the variance for all levels plus the residual variance from the summary().
summary(g2.mixed)
print(VarCorr(g2.mixed),comp="Variance") 

vars<- c(0.04346,0.08877,0.12457)
vars/sum(vars)  

#report the predictions of the best fitting model using REML=TRUE because REML gives more robust estimates then ML; but REML is not suited for model comparison. 
 
# visualizing fits
# plotting from mixed models used to be somewhat clunky
# the code below is Nils Bunnefeld's (slightly modified) code that extracts coefficients from his model summaries and uses those to plot lines
# Note the coefficients don't match our own in part because Nils treated prev as a factor
# # Mixed model coefficients are hard to translate directly into plots because the coeffs are mean centred
# So one common approach is to use coeffs from fixed models (assuming they are representative of those of mixed models)
# In the code below, Nils uses his coeffs from a fixed model including prev as a categorical variable
# could plot all three lines and all data on a single panel
plot(shot~count,data=GROUSE,
     ylab="Grouse shot per km2 (log)",
     xlab="Grouse counted per km2 (log)",
     type="n")
# in the previous line, type ="n" suppresses plotting anything, so I can add points by group and better control how the plot looks

points(shot[prev=="1"]~count[prev=="1"],data=GROUSE, pch=20)
points(shot[prev=="2"]~count[prev=="2"],col="blue",data=GROUSE,  
       pch=20)
points(shot[prev=="3"]~count[prev=="3"],col="red",data=GROUSE, 
       pch=20)
abline(-3.98,1.4)
abline(-3.98-0.53,1.41,lty=3,col="blue")
abline(-3.98-0.94,1.41,lty=4,col="red")

#IMO this graph looks a bit messy. Use mfrow() to split the graphics area into three separate plotting areas, or use ggplot to make a publication quality figure

par(mfrow=c(1,3))
plot(shot~count,data=GROUSE,type="n",xlab="Grouse counted (log)", ylab="Number of grouse shot (log)",cex.lab=1.2)
title(main="First shooting event")  
points(shot[prev=="1"]~count[prev=="1"],data=GROUSE,col="purple",pch=20)
abline(-3.98,1.4,col="purple")
plot(shot~count,data=GROUSE,type="n",xlab=" Grouse counted (log)",ylab="",cex.lab=1.2)
title(main="Second shooting event")
points(shot[prev=="2"]~count[prev=="2"],col="blue",data=GROUSE, 
       pch=20)
abline(-3.98-0.53,1.41,lty=3,col="blue")
plot(shot~count,data=GROUSE,type="n",xlab=" Grouse counted (log)", ylab="", cex.lab=1.2 )
title(main="Third/fourth shooting event")
points(shot[prev=="3"]~count[prev=="3"],col="red",data=GROUSE, 
       pch=20)
abline(-3.98-0.94,1.41,lty=4,col="red")
par(mfrow=c(1,1))



############
# Using predict to plot mixed models
# the code below exploits the recently implemented ability of predict() to get predictions of mixed models
# first, create a new dataframe to feed to predict
# For a simple presentation, we need to choose a drive and moor, and in the example below I have chosen moor="Danby" and drive="PikeLaw1"

NEWGROUSE_DB<-expand.grid(count=seq(4.5,6.0, length=5), 
                       prev=c(1,2,3), moor="Danby", 
                       drive="Bainleys")

# then generate predictions, adding in the random effects form
PREDSHOT_DB<-predict(g2.mixed, newdata=NEWGROUSE_DB, re.form=~(1|drive)+(1|moor))
PREDSNEW_DB<-cbind(NEWGROUSE_DB,PREDSHOT_DB)
head(PREDSNEW_DB)



ggplot(PREDSNEW_DB, aes(x = count, y = PREDSHOT_DB, color=prev))+
  geom_line()+
  facet_wrap(~prev)+
  geom_jitter(data=GROUSE, aes(y = shot),position = position_jitter(width = 0.02))


# More commonly, we don't just want the effects for one moor or drive. In that case we might like to illustrate the effects of random levels using small multiples

NEWGROUSE<-expand.grid(count=seq(4.5,6.0, length=5), 
                       prev=c(1,2,3), moor=levels(GROUSE$moor), 
                       drive=levels(GROUSE$drive))

# in the predict command below, I ask R to use both random effects in getting predictions
PREDSHOT<-predict(g2.mixed, newdata=NEWGROUSE, re.form=~(1|drive)+(1|moor))
PREDSNEW<-cbind(NEWGROUSE,PREDSHOT)
head(PREDSNEW)

PREDSHOT<-predict(g2.mixed, newdata=NEWGROUSE, re.form=~(1|drive)+(1|moor))
PREDSNEW<-cbind(NEWGROUSE,PREDSHOT)
head(PREDSNEW)

# I then ask ggplot to plot the predictions using a facet_grid of all possible moors and drives, including combinations that don't actually exist. This is perhaps a fun way to illustrate the combined influence of both random effects, but for a real paper you might like to restrict the illustration to real combinations. In the last ggplot line we have added actual observed points using a slight jitter to facilitate visualization
ggplot(PREDSNEW, aes(x = count, y = PREDSHOT, color=factor(prev)))+
  geom_line()+
  facet_grid(moor~drive)+
  scale_x_continuous(breaks = c(4,5,6)) +
  geom_jitter(data=GROUSE, aes(y = shot),position = position_jitter(width = 0.02))


###
# Try tidyr expand, using the 'nesting' argument to 
#  create only those combinations of moor and drive
#  that exist in our data frame
###
# the following line forces R to consider the expand function from the tidyr package instead of lme4
expand<-tidyr::expand
GROUSE_reduced <- expand(GROUSE, 
                         count = seq(4,6.5, length = 10),
                         prev = 1:3,
                         nesting(moor, drive))

PREDSHOT_2 <- predict(g2.mixed, 
                      newdata = GROUSE_reduced, 
                      re.form = ~(1|drive) + (1|moor))

PREDSNEW_2 <- cbind(GROUSE_reduced, PREDSHOT_2)
head(PREDSNEW_2)

##
# We can plot our predictions and raw data on a grid, which gives
#  blank panels for combinations that do not exist
##
ggplot(PREDSNEW_2, aes(x = count, y = PREDSHOT_2, color=factor(prev)))+
  geom_line()+
  geom_jitter(data=GROUSE, aes(y = shot),
              position = position_jitter(width = 0.02)) +
  scale_x_continuous(breaks = c(4,5,6)) +
  facet_grid(moor~drive)
  
##
# We may prefer to use facet wrap with both moor and drive,
#  so that we only plot predicted slopes for those combinations
#  of drive and moor that do exist.
# Use 'label_both' to make sure we know what the combination is,
#  and 'scales = "fixed"' so that all panels have the same limits.
##
ggplot(PREDSNEW_2, aes(x = count, y = PREDSHOT_2, color=factor(prev)))+
  geom_line()+
  geom_jitter(data=GROUSE, aes(y = shot),
              position = position_jitter(width = 0.02)) +
  scale_x_continuous(breaks = c(4,5,6)) +
  facet_wrap(c("moor","drive"),
             labeller = "label_both",
             scales = "fixed")

### CROSSED FIXED AND RANDOM EFFECTS ####


# Fly quant genetics
# clear workspace
 rm(list=ls())

# import data file
FLIES<-read_csv("KatieFlies4R.csv")
str(FLIES)




# recode categories as factors
FLIES$Maternal.Fam<-as.factor(FLIES$Maternal.Fam)
FLIES$Paternal.Fam<-as.factor(FLIES$Paternal.Fam)
FLIES$Population<-as.factor(FLIES$Population)
FLIES$Sex<-as.factor(FLIES$Sex)
FLIES$Cross<-as.factor(FLIES$Cross)

str(FLIES)

names(FLIES)

# select males
MFLIES<-FLIES[FLIES$Sex=="M",]

# want to play with data on mass so need to check dist
hist(MFLIES$Wet.Mass)
# looks great

# visualize effects of temp on male mass by pop
library(ggplot2)
ggplot(MFLIES, aes(x = Temp, y = Wet.Mass)) + 
  geom_point() +
  stat_smooth(method="lm") +
  facet_grid(.~ Population)

# looks like temp has a negative effect on mass, but maybe in only some sex:pop combinations

# build a fixed model first
Mfixed.mod1<-lm(Wet.Mass~Temp,data=MFLIES)


par(mfrow=c(2,2))
plot(Mfixed.mod1)
par(mfrow=c(1,1))
# looks good
summary(Mfixed.mod1)
# support for interaction, but pseudorep

# could add in Population as fixed factor

Mfixed.mod2<-lm(Wet.Mass~Temp*Population,data=MFLIES)
par(mfrow=c(2,2))
plot(Mfixed.mod2)
par(mfrow=c(1,1))
# problems with QQ plot at lower end
summary(Mfixed.mod2)
# seem to be some strong Pop effects, but model the same
# can I remove Int?
anova(Mfixed.mod2,Mfixed.mod1)
# nope



# F test says we need to keep int, but this is still pseudoreplicated



# so now try mixed model, first without "interaction" between temp and Pop
names(MFLIES)
library(lme4)
Mmixed.mod1<-lmer(Wet.Mass~Temp*Population+(1|Maternal.Fam)+(1|Paternal.Fam),data=MFLIES)

summary(Mmixed.mod1)
vcov(Mmixed.mod1)

# is int important?
Mmixed.mod2<-update(Mmixed.mod1,~. - Temp:Population)

library(pbkrtest)
kr.e<-KRmodcomp(Mmixed.mod1,Mmixed.mod2)
summary(kr.e)
# yup, interaction is needed

pb.e<-PBmodcomp(Mmixed.mod1,Mmixed.mod2,nsim=500)
summary(pb.e)

# either way get same result, and try fitting pop as a random effect



Mmixed.mod3<-lmer(Wet.Mass~Temp+(1|Population)+(1|Maternal.Fam)+(1|Paternal.Fam),data=MFLIES)

summary(Mmixed.mod3)
# NB pop not soaking up much variance here


# now can we get effects of temp to vary by population?
Mmixed.mod4<-lmer(Wet.Mass~Temp+(Temp|Population)+(1|Maternal.Fam)+(1|Paternal.Fam),data=MFLIES)

# mod 4 slightly better than mod3
summary(Mmixed.mod4)
# Now that we allow slopes to vary Population becomes more important

#######

kr.f<-KRmodcomp(Mmixed.mod3,Mmixed.mod4)
# can't run KR test because models not nested!

pb.f<-PBmodcomp(Mmixed.mod3,Mmixed.mod4,nsim=500)
# takes a long time to run this code! some warnings
warnings()
# failures to achieve convergence
summary(pb.f)
# finally we get this to work
# result supports more complex model


# correlation of fixed effects implies that populations that have a strong negative effect of Temp tend to have the largest intercepts


# now how to illustrate effects?
str(Mmixed.mod4)
str(MFLIES)
NEWTEMP<-expand.grid(Temp=seq(16,24,length=10),Population=levels(MFLIES$Population),Maternal.Fam=levels(MFLIES$Maternal.Fam),Paternal.Fam=levels(MFLIES$Paternal.Fam))
PREDMASS<-predict(Mmixed.mod4,newdata=NEWTEMP,re.form=~(Temp|Population))

PREDSFRAME<-cbind(NEWTEMP,PREDMASS)
head(PREDSFRAME)

ggplot(PREDSFRAME, aes(x = Temp, y = PREDMASS)) + 
  geom_line() +
  facet_grid(. ~ Population)+
  geom_point(data=MFLIES, aes(y = Wet.Mass))

# way cool, and actually shows effects

# could also visualize combined effects of Maternal and paternal family, for example
# code is similar to code for main practical, and exploits nesting in tidyr

expand<-tidyr::expand
names(MFLIES)
MFLIES_reduced<-MFLIES %>% 
  select(Population, Maternal.Fam,Paternal.Fam,Temp) %>% 
  expand(Temp=c(16,20,24),nesting(Paternal.Fam,Maternal.Fam,Population))
  
summary(MFLIES_reduced)

PREDMASS_2 <- predict(Mmixed.mod4, 
                      newdata = MFLIES_reduced, 
                      re.form = ~(1|Paternal.Fam) + (1|Maternal.Fam)
                      + (Temp|Population))

PREDSFRAME_2 <- cbind(MFLIES_reduced, PREDMASS_2)
head(PREDSFRAME_2)

ggplot(PREDSFRAME_2, aes(x = Temp, y = PREDMASS_2))+
  geom_line()+
  geom_point(data=MFLIES, aes(y = Wet.Mass)) +
  scale_x_continuous(breaks = c(16,20,24)) +
  facet_wrap(c("Maternal.Fam","Paternal.Fam"),
           labeller = "label_both",
           scales = "fixed")


