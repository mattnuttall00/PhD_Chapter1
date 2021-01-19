### Load packages ####

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

### Load data ####

# load data (centred)
dat1 <- read.csv("Data/commune/dat1.csv", header = TRUE, stringsAsFactors = TRUE)

dat1$elc <- as.factor(dat$elc)
dat1$PA <- as.factor(dat$PA)

### Run model ####


sat9c <- glmer(ForPix ~ pop_den + mean_elev + dist_border + dist_provCap + elc + PA + 
                offset(log(areaKM)) + (year|Province/Provcomm),
                data=dat1, family="poisson", 
                control=glmerControl(optimizer="nlminbwrap",  optCtrl=list(maxfun=100000)))

## Notes on the model.  I am using the "nlminbwrap" optimizer as the original model was producing warnings, and after doing some reading on the issue I tested various different optimizers and this one effectively removed the warning.  I have also got "elc" and "PA" in the model for theoretical reasons, even though neither appear to have much of an effect at all. But theoretically, and from my experience, both variables SHOULD be important to account for. So I have left them in for that reason - to control for them.  I am happy to remove them if you think the above is not a good enough reason to keep them in.
### Simple diagnostics ####


# copy data
sat9c.diag.dat <- dat1
sat9c.diag.dat$Provcomm <- as.factor(sat9c.diag.dat$Provcomm)

### Make "fitted" predictions, i.e. fully conditional:
sat9c.diag.dat$pred <- predict(sat9c, type = "response")

### Plot predicted against observed:
plot(sat9c.diag.dat$ForPix, sat9c.diag.dat$pred, ylab = "Predicted ForPix", xlab = "Observed ForPix")

### Extract model residuals:
sat9c.diag.dat$resid <- resid(sat9c)

### Plot residuals against fitted values:
plot(sat9c.diag.dat$pred, sat9c.diag.dat$resid)
# lots of heterogeneity at lower predicted values of ForPix


# Variance component analysis
print(VarCorr(sat9c),comp="Variance") 
vars <- data.frame(term = c("Commune","year/com", "Province", "year/Prov"),
                   variance = c(10.4,0.0046,6.7,0.00048))
vars$relative.contrib <- vars$variance/sum(vars$variance)
# Commune is making up the majority of the variance (~60%), followed by Province (~39%), with year making up the negligible rest. 

# marginal and conditional r2
r.squaredGLMM(sat9c)
# Marginal r2 (fixed effects) is relatively high 0.78, and the conditional (fixed + random) is 1.  This means that the fixed effects are actually accounting for most of the variance. 

### Examples of global predictions ####
  # Population density ####

# create new data. I will set mean_elev, dist_border, dist_provCap at their national means, and I will set elc and PA to 0.
pop_den_newdat <- data.frame(pop_den = seq(min(dat1$pop_den), max(dat1$pop_den), length.out = 200),
                             mean_elev = mean(dat1$mean_elev),
                             dist_border = mean(dat1$dist_border),
                             dist_provCap = mean(dat1$dist_provCap),
                             elc = "0",
                             PA = "0",
                             areaKM = mean(dat1$areaKM))
pop_den_newdat$pred <- as.vector(predict(sat9c, type="response", newdata=pop_den_newdat, re.form=NA))

# plot
ggplot(pop_den_newdat, aes(x=pop_den, y=pred))+
  geom_line(size=1)+
  xlim(-0.18,5)+
  #ylim(0,26000)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"))

  # Mean elevation ####

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
ggplot(mean_elev_newdat, aes(x=mean_elev, y=pred))+
  geom_line(size=1)+
  #xlim(-0.18,5)+
  ylim(0,26000)+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "grey20"),
        axis.title = element_text(size=17),
        axis.text = element_text(size=15))+
  ylab("Predicted forest pixels per unit area (km2)")+
  xlab("Elevation (scaled)")
### Examples of province-level and commune-level predictions ####
  # population density ####

# This function produces a "mean" prediction for the whole province (taken as the mean of all commune predictions) and 95% quantiles (to show within-province variation). Plotted below as a single line with error ribbons
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


# This function does the same as above but instead of taking the 95% quantiles, it outputs the mean plus ALL commune predictions. These are plotted as lots of grey lines (like the plot I sent to you originally on Teams)
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


# This function is used to run the above two functions. I.e. it is used to run the above functions for ALL provinces
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


# To run the above functions for population density use:
pop_den_quants <- RunFun(dat1, ProvMean.popden, sat9c) # quantiles
pop_den_lines  <- RunFun(dat1, ProvMeanLine.popden, sat9c) # all commune lines
  # mean elevation ####

# same function as population density above, but for mean elevation (see population density section for further explanation)
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

# Same function as population density above, but for mean elevation
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

# same function as population density above - allows the above two functions to be run for ALL provinces
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


# These two lines will run the above functions for mean elevation and produce the output dataframes
mean_elev_quants <- RunFun(dat1, ProvMean.elev, sat9c)
mean_elev_lines  <- RunFun(dat1, ProvMeanLine.elev, sat9c)