

### Load libraries & data ####

library('nlme')
library('ggplot2')
library('cowplot')
library('tidyverse')
library('gamm4')
library('mgcv')
library('voxel')
library('nlstools')
library('minpack.lm')
library('broom')
library('export')
library('propagate')
library('quantreg')
library('gridExtra')
library('grid')


dat_econ <- read_csv("macroeconomic_vars.csv")
dat_econ <- mutate(dat_econ, year = as.factor(year))
str(dat_econ)
dat_resp <- read_csv("ForCov_LU_econVars_PCs.csv")

# create working dataframe (excluding principal components)
dat_work <- dat_resp %>% 
  mutate(year = as.factor(year)) %>% 
  dplyr::select(year,for_cov_pix,for_cov_area,for_cov_perc,for_cov_roc,urban_pix,urban_area,
                urban_perc,urban_roc,agric_pix,agric_area,agric_perc,agric_roc) %>% 
  left_join(.,dat_econ, by = "year")

str(dat_work)

### Exploratory Plots ####

# For_cov_roc and agr_gdp over time
p1 <- ggplot(dat_work)+ 
       geom_point(aes(x=year, y=for_cov_roc))
  
p2 <- ggplot(dat_work)+
       geom_point(aes(x=year, y=agr_gdp))

plot_grid(p1,p2)

# for_cov_roc and ind_gdp over time
p3 <- ggplot(dat_work, aes(x=year, y=ind_gdp))+
       geom_point()+
       theme(axis.text.x = element_text(angle=90, hjust = 1))
p4 <- ggplot(dat_work, aes(x=year, y=for_cov_roc))+
       geom_point()+
       theme(axis.text.x = element_text(angle=90, hjust = 1))

plot_grid(p4,p3)


## Scatter plots of raw economic predictors and forest cover rate of change ####


# scatter plot for_cov_roc ~ gdp
p5 <- ggplot(dat_work, aes(x=gdp, y=for_cov_roc))+
       geom_point()

# scatter plot for for_cov_roc ~ fdi
p6 <- ggplot(dat_work, aes(x=fdi, y=for_cov_roc))+
       geom_point()

# scatter plot for for_cov_roc ~ agr_gdp
p7 <- ggplot(dat_work, aes(x=agr_gdp, y=for_cov_roc))+
       geom_point()
  
# scatter plot for for_cov_roc ~ ind_gdp
p8 <- ggplot(dat_work, aes(x=ind_gdp, y=for_cov_roc))+
       geom_point()

# scatter plot for for_cov_roc ~ dev_agri
p9 <- ggplot(dat_work, aes(x=dev_agri, y=for_cov_roc))+
       geom_point()

# scatter plot for for_cov_roc ~ dev_env
p10 <- ggplot(dat_work, aes(x=dev_env, y=for_cov_roc))+
        geom_point()

plot_grid(p5,p6,p7,p8,p9,p10)

## Scatter plots of raw economic predictors and raw forest loss ####

# scatter plot for_cov_area ~ gdp
p5 <- ggplot(dat_work, aes(x=gdp, y=for_cov_area))+
       geom_point()

# scatter plot for for_cov_area ~ fdi
p6 <- ggplot(dat_work, aes(x=fdi, y=for_cov_area))+
       geom_point()

# scatter plot for for_cov_area ~ agr_gdp
p7 <- ggplot(dat_work, aes(x=agr_gdp, y=for_cov_area))+
       geom_point()
  
# scatter plot for for_cov_area ~ ind_gdp
p8 <- ggplot(dat_work, aes(x=ind_gdp, y=for_cov_area))+
       geom_point()

# scatter plot for for_cov_area ~ dev_agri
p9 <- ggplot(dat_work, aes(x=dev_agri, y=for_cov_area))+
       geom_point()

# scatter plot for for_cov_area ~ dev_env
p10 <- ggplot(dat_work, aes(x=dev_env, y=log(for_cov_area)))+
        geom_point()

plot_grid(p5,p6,p7,p8,p9,p10)

## Scatter plots of raw economic predictors with log-transformed Y ####

# scatter plot for_cov_roc ~ gdp
p11 <- ggplot(dat_work, aes(x=gdp, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ fdi
p12 <- ggplot(dat_work, aes(x=fdi, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ agr_gdp
p13 <- ggplot(dat_work, aes(x=agr_gdp, y=log(for_cov_roc)))+
       geom_point()
  
# scatter plot for for_cov_roc ~ ind_gdp
p14 <- ggplot(dat_work, aes(x=ind_gdp, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ dev_agri
p15 <- ggplot(dat_work, aes(x=dev_agri, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ dev_env
p16 <- ggplot(dat_work, aes(x=dev_env, y=log(for_cov_roc)))+
        geom_point()

plot_grid(p11,p12,p13,p14,p15,p16)

## The two extreme data points (1994 and 2015) are wildly different from the rest of the data, and are therefore acting as extreme outliers.  I will see what happens when I remove them

## subset data & remove outliers ####

# add forest cover change
dat_work <- dat_work %>% mutate(for_change = for_cov_area - lag(for_cov_area, default = first(for_cov_area)))

# add economic variable changes
dat_work <- dat_work %>% mutate(gdp_change = gdp - lag(gdp, default = first(gdp)))
dat_work <- dat_work %>% mutate(fdi_change = fdi - lag(fdi, default = first(fdi)))
dat_work <- dat_work %>% mutate(ind_gdp_change = ind_gdp - lag(ind_gdp, default = first(ind_gdp)))
dat_work <- dat_work %>% mutate(agr_gdp_change = agr_gdp - lag(agr_gdp, default = first(agr_gdp)))
dat_work <- dat_work %>% mutate(dev_agri_change = dev_agri - lag(dev_agri, default = first(dev_agri)))
dat_work <- dat_work %>% mutate(dev_env_change = dev_env - lag(dev_env, default = first(dev_env)))

# add commdity variable changes
dat_work <- dat_work %>% mutate(armi_change = armi - lag(armi, default = first(armi)))
dat_work <- dat_work %>% mutate(rice_med_change = rice_med - lag(rice_med, default = first(rice_med)))
dat_work <- dat_work %>% mutate(rub_med_change = rub_med - lag(rub_med, default = first(rub_med)))
dat_work <- dat_work %>% mutate(corn_med_change = corn_med - lag(corn_med, default = first(corn_med)))
dat_work <- dat_work %>% mutate(sug_med_change = sug_med - lag(sug_med, default = first(sug_med)))

# Remove the first two years (due to NA in for_cov_roc and the outlier), and the last year (due to outlier)
dat_sub <- dat_work %>% 
  filter(.,!year %in% c("1993","1994","2015"))
dat_sub <- as.data.frame(dat_sub)

dat_sub1 <- dat_work %>% 
  filter(.,!year %in% c("1993"))

## Scatter plots of raw economic variables and log-transformed Y with subsetted data ####

# scatter plot for_cov_roc ~ gdp
p17 <- ggplot(dat_sub, aes(x=gdp, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ fdi
p18 <- ggplot(dat_sub, aes(x=fdi, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ agr_gdp
p19 <- ggplot(dat_sub, aes(x=agr_gdp, y=log(for_cov_roc)))+
       geom_point()+
       stat_smooth(method="lm")
  
# scatter plot for for_cov_roc ~ ind_gdp
p20 <- ggplot(dat_sub, aes(x=ind_gdp, y=log(for_cov_roc)))+
       geom_point()+
       stat_smooth(method="lm")

# scatter plot for for_cov_roc ~ dev_agri
p21 <- ggplot(dat_sub, aes(x=dev_agri, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ dev_env
p22 <- ggplot(dat_sub, aes(x=dev_env, y=log(for_cov_roc)))+
        geom_point()


plot_grid(p17,p18,p19,p20,p21,p22)

## detrend response
dt_lm_forCov_roc <- lm(for_cov_roc~I(1:length(for_cov_roc)),data=dat_sub)
dt_forCov_roc <- dat_sub$for_cov_roc[1:20] - predict(lm(for_cov_roc~I(1:length(for_cov_roc)),
                                                        data=dat_sub))
summary(dt_lm_forCov_roc)
dat_sub$dt_for_cov_roc <- dt_forCov_roc

# scatter plot of detrended for_cov_roc ~ fdi
p23 <- ggplot(dat_sub, aes(x=fdi, y=dt_for_cov_roc))+
        geom_point()

plot_grid(p18,p23)


## Scatter plots of changes in economic predictors and changes in forest cover ####

cp1 <- ggplot(dat_sub1, aes(x=gdp_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cp2 <- ggplot(dat_work, aes(x=fdi_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cp3 <- ggplot(dat_work, aes(x=ind_gdp_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cp4 <- ggplot(dat_sub, aes(x=agr_gdp_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cp5 <- ggplot(dat_sub, aes(x=dev_agri_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cp6 <- ggplot(dat_sub, aes(x=dev_env_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")

plot_grid(cp1,cp2,cp3,cp4,cp5,cp6)

# compare agr_gdp plots
plot_grid(cp4,p19)

# compare ind_gdp plots
plot_grid(cp3,p20)

## check for lags

# becasue I want to look for a lag in for_change, the vector of for_change will be one value short, so I need to add 2015 back in (it was removed from dat_sub in the subset section above)
dat_work %>% dplyr::select(for_change) %>% filter(year=="2015") # -0.0900

# agr_gdp
cp7 <- ggplot(dat_sub, aes(x= agr_gdp_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cp4,cp7)
# The slope gets flatter with the lag


# ind_gdp
cp8 <- ggplot(dat_sub, aes(x= ind_gdp_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cp3,cp8)
# The slope gets flatter with the lag


# gdp
cp9 <- ggplot(dat_sub, aes(x= gdp_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cp1,cp9)
# The slope gets steeper with the lag

# fdi
cp10 <- ggplot(dat_sub, aes(x= fdi_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cp2,cp10)
# The slope gets slightly flatter with the lag

# dev_agri
cp11 <- ggplot(dat_sub, aes(x= dev_agri_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cp5,cp11)
# The slope gets steeper with the lag

# dev_env
cp12 <- ggplot(dat_sub, aes(x= dev_env_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cp6,cp12)
# The slope changes direction from positive (no lag) to negative (lag)


## Scatter plots of raw commodity predictors and raw Y ####

# scatter plot for_cov_roc ~ armi
p23 <- ggplot(dat_work, aes(x=armi, y=for_cov_roc))+
       geom_point()

# scatter plot for for_cov_roc ~ rice_med 
p24 <- ggplot(dat_work, aes(x=rice_med, y=for_cov_roc))+
       geom_point()

# scatter plot for for_cov_roc ~ rub_med
p25 <- ggplot(dat_work, aes(x=rub_med, y=for_cov_roc))+
       geom_point()
  
# scatter plot for for_cov_roc ~ corn_med
p26 <- ggplot(dat_work, aes(x=corn_med, y=for_cov_roc))+
       geom_point()

# scatter plot for for_cov_roc ~ sug_med
p27 <- ggplot(dat_work, aes(x=sug_med, y=for_cov_roc))+
       geom_point()


plot_grid(p23,p24,p25,p27)

## Scatter plots of raw commodity predictors with log-transformed Y with subsetted data ####

# scatter plot for for_cov_roc ~ armi
p28 <- ggplot(dat_sub, aes(x=armi, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ rice-med
p29 <- ggplot(dat_sub, aes(x=rice_med, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ rub_med
p30 <- ggplot(dat_sub, aes(x=rub_med, y=log(for_cov_roc)))+
       geom_point()
  
# scatter plot for for_cov_roc ~ corn_med
p31 <- ggplot(dat_sub, aes(x=corn_med, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ sug_med
p32 <- ggplot(dat_sub, aes(x=sug_med, y=log(for_cov_roc)))+
       geom_point()

plot_grid(p28,p29,p30,p32)


## Scatter plots of changes in commodity predictors and changes in forest cover ####

cc1 <- ggplot(dat_sub, aes(x=armi_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cc2 <- ggplot(dat_sub, aes(x=rice_med_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cc3 <- ggplot(dat_sub, aes(x=rub_med_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cc4 <- ggplot(dat_sub, aes(x=corn_med_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")
cc5 <- ggplot(dat_sub, aes(x=sug_med_change, y=for_change))+geom_point()+ stat_smooth(method = "lm")

plot_grid(cc1,cc2,cc3,cc4,cc5)

## check for lags

# becasue I want to look for a lag in for_change, the vector of for_change will be one value short, so I need to add 2015 back in (it was removed from dat_sub in the subset section above)
dat_work %>% dplyr::select(for_change) %>% filter(year=="2015") # -0.0900

# armi
cc6 <- ggplot(dat_sub, aes(x= armi_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cc1,cc6)
# Positive slope which gets steeper with lag

# rice_med
cc7 <- ggplot(dat_sub, aes(x= rice_med_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cc2,cc7)
# one extreme outlier in rice_med. Let's see what happens when it's removed

rice2 <- dat_sub[c(1:13,15:20),]

# compare original fit with subsetted fit 
cc8 <- ggplot(rice2, aes(x= rice_med_change, y=for_change))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cc2,cc8)
# When the outlier is removed the slope changes direction

# compare new fit with new fit lagged
cc9 <- ggplot(rice2, aes(x= rice_med_change, y=c(tail(rice2$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cc8,cc9)
# The lag changes the direction of the slope back to a positive relationship

# rub_med
cc10 <- ggplot(dat_sub, aes(x= rub_med_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cc3,cc10)
# Lag changes direction of slope from negative to positive (weak relationship)

# corn_med
cc11 <- ggplot(dat_sub, aes(x= corn_med_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cc4,cc11)
# Lag makes slope slightly steeper

# sug_med
cc12 <- ggplot(dat_sub, aes(x= sug_med_change, y=c(tail(dat_sub$for_change,-1), -0.0900)))+
        geom_point() + stat_smooth(method="lm")

plot_grid(cc5,cc12)
# lag makes slope slightly steeper


## Scatter plots of raw producer predictors and raw Y ####

# scatter plot for_cov_roc ~ prod_rice
p33 <- ggplot(dat_work, aes(x=prod_rice, y=for_cov_roc))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_rub 
p34 <- ggplot(dat_work, aes(x=prod_rub, y=for_cov_roc))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_cass
p35 <- ggplot(dat_work, aes(x=prod_cass, y=for_cov_roc))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_corn
p36 <- ggplot(dat_work, aes(x=prod_corn, y=for_cov_roc))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_sug
p37 <- ggplot(dat_work, aes(x=prod_sug, y=for_cov_roc))+
  geom_point()

plot_grid(p35,p37)


## Scatter plots of raw producer predictors and log-transformed Y with subsetted data ####

# scatter plot for_cov_roc ~ prod_rice
p38 <- ggplot(dat_sub, aes(x=prod_rice, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_rub 
p39 <- ggplot(dat_sub, aes(x=prod_rub, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_cass
p40 <- ggplot(dat_sub, aes(x=prod_cass, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_corn
p41 <- ggplot(dat_sub, aes(x=prod_corn, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_sug
p42 <- ggplot(dat_sub, aes(x=prod_sug, y=log(for_cov_roc)))+
  geom_point()

plot_grid(p38,p40,p42)

###--------------------------------------------------------------------------------------------
### Modelling economic variables ####

# I will be using fdi, agr_gdp, and ind_gdp as predictors. I don't want to be using multiple regression because theoretically I don't think that these predictors are actual drivers of forest cover rate of change, and therefore I don't think that I need to account for the effect of each of the other predictors on the response when looking at a particular predictor (i.e. partial contributions).  I am simply looking for the relationship between forest cover rate of change and each of the predictors. 

# I am going to test simple linear models first.  I may need to use GLMs if there are issues with the variance, errors, or overdispersion

## fdi ####

hist(dat_sub$for_cov_roc)
hist(dat_sub$fdi)
ggplot(dat_sub, aes(x=fdi, y=for_cov_roc))+
  geom_point()
# The relationship does not look linear. Try log-transform
ggplot(dat_sub, aes(x=fdi, y=log(for_cov_roc)))+
  geom_point()
# Bit better


## Simple linear model

# estimate parameters by eye first:
# intercept: 0.5, effect: 3.5/1750 = 0.002

elm1 <- lm(for_cov_roc ~ fdi, data=dat_sub)
par(mfrow=c(2,2))
plot(elm1)
# residuals don't look good. They don't suggest a linear relationship
hist(elm1$residuals)
# although histogram looks alright
summary(elm1)

# Lets look at the model fit
par(mfrow=c(1,1))
newxvars <- seq(84,1873, length = 100)
newyvars <- predict(elm1, newdata = list(fdi=newxvars), int = "c")
str(newyvars)
head(newyvars)

# new dataframe
df.newvars <- data_frame(newx = newxvars,
                         newy = as.numeric(newyvars[,"fit"]),
                         newupr = as.numeric(newyvars[,"upr"]),
                         newlwr = as.numeric(newyvars[,"lwr"]))

# Plot
elm1.plot <- ggplot(df.newvars, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha = 0.25))+
  geom_point(data = dat_sub, aes(x = fdi,y = for_cov_roc))+
  labs(x = "Foreign Direct Investment",
       y = "Forest cover % change")+
    theme_bw()+
    theme(element_blank())+
  theme(legend.position="none")

# The model is a rubbish fit as far as I'm concerned.  I will try a linear model with log-transformed Y

elm2 <- lm(log(for_cov_roc) ~ fdi, data = dat_sub)
par(mfrow=c(2,3))
plot(elm2)
hist(elm2$residuals)
summary(elm2)
# Not conviced that we have normal errors 

# Lets plot the model fit
elm2.plot <- elm2 %>%
  augment() %>%
  ggplot(., aes(x = fdi, y = exp(log.for_cov_roc.))) +
  geom_point(size = 1) +
  geom_line(aes(x = fdi, y = exp(.fitted))) +
  geom_ribbon(aes(ymin = exp(.fitted - (1.96*.se.fit)),
                  ymax = exp(.fitted + (1.96*.se.fit))),
              alpha = 0.5) +
  theme_bw() +
  theme(element_blank())+
  labs(x = "Foreign Direct Investment",
       y = "Forest cover % change")


# plot the two linear models
plot_grid(elm1.plot,elm2.plot)

## Try GLM rather than LM

eglm1 <- 

## Exponential models

## 3 parameter exponential
# Starting values
a <- 1.5
b <- -0.0005
c <- 400

enlm1 <- nlsLM(for_cov_roc ~ a*exp(-b*fdi)+c, start = list(a=a, b=b,c=c), data=dat_sub,
               control = nls.lm.control(maxiter = 1000))
summary(enlm1)
par(mfrow=c(2,2))
plot(nlsResiduals(enlm1))
# Diagnostics look ok.  Model summary shows that the model fit is terrible, huge standard errors and coefficients which are not believable

par(mfrow=c(1,1))
plot(for_cov_roc ~ fdi, data=dat_sub)
curve(predict(enlm1, newdata = data.frame(fdi=x)), add=TRUE)
# Plot of model fit shows the model is a poor fit 

## 2 parameter exponential
# Starting values
a <- 1.1
b <- -0.0005*log(2)/a

enlm2 <- nls(for_cov_roc ~ a*exp(-b*fdi), start = list(a=a, b=b), data = dat_sub)
par(mfrow=c(2,2))
plot(nlsResiduals(enlm2))
summary(enlm2)

# Diagnostics look good, as does the output

## Confidence intervals

# Create new dataframe for predicting 
enlm2_newx <- data.frame(fdi = seq(0,1873,length=100))
enlm2_newy <- predictNLS(enlm2, newdata=enlm2_newx, interval = "confidence", alpha = 0.05)
head(enlm2_newy$summary)

# Plot model fit (using original data)
enlm2 %>%
  augment() %>%
  ggplot(., aes(x = fdi, y = for_cov_roc)) +
  geom_point(size = 1.5) +
  geom_line(aes(x = fdi, y = .fitted),size=1, color="#000099") +
  theme_bw() +
  labs(x = "Foreign Direct Investment",
       y = "Forest cover % change")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

graph2ppt(file="fdi_plot", width=8, height=8)

# Plot model fit using new data
lwr_raw <- enlm2_newy$summary[,11]
upr_raw <- enlm2_newy$summary[,12]

enlm2_newdf <- data.frame(newx = enlm2_newx,
                          newy = enlm2_newy$summary[,7],
                          lwr_raw = lwr_raw,
                          upr_raw = upr_raw)
enlm2_newdf<- enlm2_newdf %>% mutate(upr = newy+upr_raw) %>% mutate(lwr = newy -lwr_raw)

fdiplot <- ggplot(enlm2_newdf, aes(x=fdi, y=newy))+
   geom_line(size=1, color="#000099")+
  geom_point(data=dat_sub, aes(x=fdi, y=for_cov_roc))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha = 0.1), show.legend = FALSE)+
  theme_bw() +
  labs(x = "Direct Foreign Investment (USD Millions)",
       y = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

# compare exponential models
anova(enlm1,enlm2)
# 2 parameter model is better

# compare lm model to log linear model
anova(elm2,elm1)
AIC(elm1)
AIC(elm2)

# Compare nls model to lm model
anova(enlm2, elm1)

# Compare nls model to log lm model
AIC(enlm2)
AIC(elm2)
# The non linear model has a much lower AIC, and therefore is the better model.

# I now need to estimate confidence intervals, using Monte Carlo simulation


## agr_gdp ####

hist(dat_sub$for_cov_roc)
hist(dat_sub$agr_gdp)
ggplot(dat_sub, aes(x=agr_gdp, y=for_cov_roc))+
  geom_point()
# The relationship does not look linear. Try log-transform
ggplot(dat_sub, aes(x=agr_gdp, y=log(for_cov_roc)))+
  geom_point()

# I almost think the non-log transformed plot looks more linear. I'll start wit that

elm3 <- lm(for_cov_roc ~ agr_gdp, data=dat_sub)
par(mfrow=c(2,2))
plot(elm3)
# doesn't look too bad.  plots suggest it could be linear relationship, although may have non normal variance

summary(elm3)

# Lets look at the model fit
par(mfrow=c(1,1))
elm3_newxvars <- seq(22.7,44.4, length = 100)
elm3_newyvars <- predict(elm3, newdata = list(agr_gdp=elm3_newxvars), int = "c")
str(elm3_newyvars)
head(elm3_newyvars)

# new dataframe
agr_gdp_newvars <- data_frame(newx = elm3_newxvars,
                         newy = as.numeric(elm3_newyvars[,"fit"]),
                         newupr = as.numeric(elm3_newyvars[,"upr"]),
                         newlwr = as.numeric(elm3_newyvars[,"lwr"]))

# Plot
ggplot(agr_gdp_newvars, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha = 0.25))+
  geom_point(data = dat_sub, aes(x = agr_gdp,y = for_cov_roc))+
  labs(x = "Agricultural contribution to GDP",
       y = "Forest cover % change")+
    theme_bw()+
  theme(legend.position="none")

# Now I will try linear model with log-transformed y
elm4 <- lm(log(for_cov_roc) ~ agr_gdp, data = dat_sub)

par(mfrow=c(2,3))
plot(elm4)
hist(elm2$residuals)
summary(elm4)
acf(residuals(elm4))
pacf(residuals(elm4))
# diagnostic plots suggest log-transforming Y makes it more non-linear

# Lets plot the model fit
agr_gdp_plot <- elm4 %>%
  augment() %>%
  ggplot(., aes(x = agr_gdp, y = exp(log.for_cov_roc.))) +
  geom_point(size = 1) +
  geom_line(aes(x = agr_gdp, y = exp(.fitted)),size=1, color="#000099") +
  geom_ribbon(aes(ymin = exp(.fitted - (1.96*.se.fit)),
                  ymax = exp(.fitted + (1.96*.se.fit))),
              alpha = 0.5) +
  theme_bw() +
  labs(x = "Agricultural contribution (%) to GDP",
       y = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

graph2ppt(file="fdi_plot", width=8, height=8)

# compare models
AIC(elm3) 
AIC(elm4)

## non-linear models

## 2 parameter exponential
# Starting values
a <- 1.1
b <- -0.0005*log(2)/a

enlm3 <- nls(for_cov_roc ~ a*exp(-b*agr_gdp), start = list(a=a, b=b), data = dat_sub)
par(mfrow=c(2,2))
plot(nlsResiduals(enlm2))
summary(enlm1)

# Diagnostics look good, as does the output

# Plot model fit
enlm3 %>%
  augment() %>%
  ggplot(., aes(x = agr_gdp, y = for_cov_roc)) +
  geom_point(size = 1) +
  geom_line(aes(x = agr_gdp, y = .fitted)) +
  theme_bw() +
  labs(x = "Agricultural contribution to GDP",
       y = "Forest cover % change")

AIC(enlm3)
AIC(elm3)
anova(enlm3,elm3)


## ind_gdp ####
par(mfrow=c(1,1))
hist(dat_sub$for_cov_roc)
hist(dat_sub$ind_gdp)
ggplot(dat_sub, aes(x=ind_gdp, y=for_cov_roc))+
  geom_point()
# The relationship does not look linear. Try log-transform
ggplot(dat_sub, aes(x=ind_gdp, y=log(for_cov_roc)))+
  geom_point()

ggplot(dat_sub, aes(x=ind_gdp, y=exp(for_cov_roc)))+
  geom_point()

ggplot(dat_sub, aes(x=ind_gdp, y=-exp(for_cov_roc)^-3))+
  geom_point()

# exp(y) is the best for linearising.  But non-linear model may be the way forward

elm5 <- lm(for_cov_roc ~ ind_gdp, data=dat_sub)
par(mfrow=c(2,2))
plot(elm5)
summary(elm5)
acf(residuals(elm5))
# doesnt look linear

# look at model fit
inf_gdp_newx <- seq(15,30.7, length = 100)
inf_gdp_newy <- predict(elm5, newdata = list(ind_gdp=inf_gdp_newx), int = "c")
str(newyvars)
head(newyvars)

# new dataframe
ind_gdp_df_newvars <- data_frame(newx = inf_gdp_newx,
                         newy = as.numeric(newyvars[,"fit"]),
                         newupr = as.numeric(inf_gdp_newy[,"upr"]),
                         newlwr = as.numeric(inf_gdp_newy[,"lwr"]))

# Plot
ind_gdp_plot <- ggplot(ind_gdp_df_newvars, aes(x = newx, y = newy)) +
  geom_line(size=1, color="#000099") +
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha = 0.5))+
  geom_point(data = dat_sub, aes(x = ind_gdp,y = for_cov_roc))+
  labs(x = "Industrial contribution (%) to GDP",
       y = "")+
    theme_bw()+
  theme(legend.position="none")+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

graph2ppt(file="ind_gdp_plot", width=8, height=8)

# linear model with exp(y)
elm6 <- lm(exp(for_cov_roc) ~ ind_gdp, data=dat_sub)
par(mfrow=c(2,2))
plot(elm6)

# Lets plot the model fit
elm6 %>%
  augment() %>%
  ggplot(., aes(x = ind_gdp, y = exp.for_cov_roc.)) +
  geom_point(size = 1) +
  geom_line(aes(x = ind_gdp, y = exp(.fitted)),size=1, color="#000099") +
  geom_ribbon(aes(ymin = exp(.fitted - (1.96*.se.fit)),
                  ymax = exp(.fitted + (1.96*.se.fit))),
              alpha = 0.5,fill="#000099") +
  theme_bw() +
  labs(x = "Industrial contribution to GDP",
       y = "Forest cover % change")

## non-linear model

## 2 parameter exponential
# Starting values
a <- 1.1
b <- -0.0005*log(2)/a

enlm4 <- nls(for_cov_roc ~ a*exp(-b*ind_gdp), start = list(a=a, b=b), data = dat_sub)
par(mfrow=c(2,2))
plot(nlsResiduals(enlm4))
summary(enlm1)

# Diagnostics look good, as does the output

# Plot model fit
enlm4 %>%
  augment() %>%
  ggplot(., aes(x = ind_gdp, y = for_cov_roc)) +
  geom_point(size = 1) +
  geom_line(aes(x = ind_gdp, y = .fitted)) +
  theme_bw() +
  labs(x = "Industrial contribution to GDP",
       y = "Forest cover % change")

# compare models
AIC(elm5)
AIC(enlm4)

### Modelling commodity variables ####

# I will be modelling Agricultural Raw Materials Index (armi), because it is the stongest loading for PC1, and theoretically I think it is also important as it represents the commodity market at a regional level.  Rice_med has been selected because it is the largest positive loading for PC2.  Sug_med has been selected because it is the largest negative loading for PC2.  Rub_med has been selected for theoretical reasons - rubber has played a major role in economic land concessions.  

## armi ####

# Plots
str(dat_sub)

# for_cov_roc ~ armi
plot(dat_sub$armi, dat_sub$for_cov_roc)

# log(for_cov_roc) ~ armi
plot(dat_sub$armi, log(dat_sub$for_cov_roc))

# There appears to be heteroscedasticity.  I may have to try quantile regression

# First let's try a linear model with log-transformed y

clm1 <- lm(log(for_cov_roc) ~ armi, data = dat_sub)
par(mfrow=c(2,2))
plot(clm1)
summary(clm1)
par(mfrow=c(1,1))
acf(residuals(clm1))

# Lets plot the fit

armi_newxvars <- seq(48,122, length=100)
armi_newyvars <- exp(predict(clm1, newdata = list(armi=armi_newxvars), int = "c"))

# new dataframe
armi_newdf <- data_frame(newx = armi_newxvars,
                         newy = as.numeric(armi_newyvars[,"fit"]),
                         newupr = as.numeric(armi_newyvars[,"upr"]),
                         newlwr = as.numeric(armi_newyvars[,"lwr"]))

# Plot
armiplot <- ggplot(armi_newdf, aes(x = newx, y = newy)) +
  geom_line(size=1, color="#000099") +
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha = 0.25))+
  geom_point(data = dat_sub, aes(x = armi,y = for_cov_roc))+
  labs(x = "Agricultural Raw Materials Index",
       y = "")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# quickly try a linear model with no transformation
clm2 <- lm(for_cov_roc ~ armi, data = dat_sub)
ggplot(dat_sub, aes(x=armi, y=for_cov_roc)) + geom_point() + geom_smooth(method = "lm")
summary(clm2)


# Now let's try quantile regression model

cqm1 <- rq(log(for_cov_roc) ~ armi, data = dat_sub, tau = seq(0.05,0.95,by=0.05))
quantplot <- summary(cqm1)
plot(quantplot, xlim=c(48,122), ylim=c(-0.05,0))

qs <- 1:9/10
ggplot(dat_sub, aes(x=armi, y=for_cov_roc))+
  geom_point()+
  geom_quantile(quantiles=qs)

# Ok I don't actually think I need quantile regression.  The heteroscedasicity only exists in the log-transformed plot.  

# I will try a non-linear model

a <- 1.1
b <- -0.0005*log(2)/a

cnlm1 <- nls(for_cov_roc ~ a*exp(-b*armi), start = list(a=a, b=b), data = dat_sub)
par(mfrow=c(2,2))
plot(nlsResiduals(cnlm1))
summary(cnlm1)


# Plot model fit (using original data)
p2 <- cnlm1 %>%
  augment() %>%
  ggplot(., aes(x = armi, y = for_cov_roc)) +
  geom_point(size = 1.5) +
  geom_line(aes(x = armi, y = .fitted),size=1, color="#000099") +
  theme_bw() +
  labs(x = "Agricultural Raw Materials Index",
       y = "Forest cover % change")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create new dataframe for predicting 
cnlm1_newx <- data.frame(armi = seq(48,122,length=100))
cnlm1_newy <- predictNLS(cnlm1, newdata=cnlm1_newx, interval = "confidence", alpha = 0.05)
head(cnlm1_newy$summary)

# Plot model fit using new data
lwr_raw <- cnlm1_newy$summary[,11]
upr_raw <- cnlm1_newy$summary[,12]

cnlm1_newdf <- data.frame(newx = cnlm1_newx,
                          newy = cnlm1_newy$summary[,7],
                          lwr_raw = lwr_raw,
                          upr_raw = upr_raw)
cnlm1_newdf<- cnlm1_newdf %>% mutate(upr = newy+upr_raw) %>% mutate(lwr = newy -lwr_raw)

p3 <- ggplot(cnlm1_newdf, aes(x=armi, y=newy))+
   geom_line(size=1, color="#000099")+
  geom_point(data=dat_sub, aes(x=armi, y=for_cov_roc))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha = 0.1), show.legend = FALSE)+
  theme_bw() +
  labs(x = "Agricultural Raw Materials Index",
       y = "Rate of forest cover loss (%)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid(p1,p2,p3)
AIC(clm1)
AIC(cnlm1)


## rice_med ####

par(mfrow=c(1,1))
plot(dat_sub$rice_med, dat_sub$for_cov_roc)
plot(dat_sub$rice_med, log(dat_sub$for_cov_roc))

# log-transformed y helps to linearise

# linear model
clm3 <- lm(log(for_cov_roc) ~ rice_med, data=dat_sub)
par(mfrow=c(2,2))
plot(clm3)
summary(clm3)
acf(residuals(clm3))

# Lets plot the fit

rice_med_newx <- seq(171,647, length=100)
rice_med_newy <- exp(predict(clm3, newdata = list(rice_med=rice_med_newx), int = "c"))

# new dataframe
rice_med_df_newvars <- data_frame(newx = rice_med_newx,
                         newy = as.numeric(rice_med_newy[,"fit"]),
                         newupr = as.numeric(rice_med_newy[,"upr"]),
                         newlwr = as.numeric(rice_med_newy[,"lwr"]))

# Plot
rice_med_plot <- ggplot(rice_med_df_newvars, aes(x = newx, y = newy)) +
  geom_line(size=1, color="#000099") +
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha = 0.25))+
  geom_point(data = dat_sub, aes(x = rice_med,y = for_cov_roc))+
  labs(x = "Rice price (USD/ton)",
       y = "")+
    theme_bw()+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# plot linear model (with no log-transformation)
ggplot(dat_sub, aes(x=rice_med, y=for_cov_roc))+
  geom_point()+
  geom_smooth(method="lm")

clm4 <- lm(for_cov_roc ~ rice_med, data=dat_sub)
summary(clm4)

# Try non-linear model

a <- 1.1
b <- -0.0005*log(2)/a

a <- 0.1
b <- -0.001*log(2)/a
# changing staring values don't make a difference

cnlm2 <- nls(for_cov_roc ~ a*exp(-b*rice_med), start = list(a=a, b=b), data = dat_sub)
par(mfrow=c(2,2))
plot(nlsResiduals(cnlm1))
summary(cnlm1)


# Plot model fit (using original data)
p4 <- cnlm2 %>%
  augment() %>%
  ggplot(., aes(x = rice_med, y = for_cov_roc)) +
  geom_point(size = 1.5) +
  geom_line(aes(x = rice_med, y = .fitted),size=1, color="#000099") +
  theme_bw() +
  labs(x = "Rice price",
       y = "Forest cover % change")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create new dataframe for predicting 
cnlm2_newx <- data.frame(rice_med = seq(171,647,length=100))
cnlm2_newy <- predictNLS(cnlm2, newdata=cnlm2_newx, interval = "confidence", alpha = 0.05)
head(cnlm2_newy$summary)

# Plot model fit using new data
lwr_raw <- cnlm2_newy$summary[,11]
upr_raw <- cnlm2_newy$summary[,12]

cnlm2_newdf <- data.frame(newx = cnlm2_newx,
                          newy = cnlm2_newy$summary[,7],
                          lwr_raw = lwr_raw,
                          upr_raw = upr_raw)
cnlm2_newdf<- cnlm2_newdf %>% mutate(upr = newy+upr_raw) %>% mutate(lwr = newy -lwr_raw)

# plot
p5 <- ggplot(cnlm2_newdf, aes(x=rice_med, y=newy))+
  geom_line(size=1, color="#000099")+
  geom_point(data=dat_sub, aes(x=rice_med, y=for_cov_roc))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha = 0.1), show.legend = FALSE)+
  theme_bw() +
  labs(x = "Rice price",
       y = "Rate of forest cover loss (%)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid(p4,p5)

## sug_med ####

plot(dat_sub$sug_med, dat_sub$for_cov_roc)
plot(dat_sub$sug_med, log(dat_sub$for_cov_roc))

# start with linear model with log(y)
clm5 <- lm(log(for_cov_roc) ~ sug_med, data=dat_sub)
par(mfrow=c(2,2))
plot(clm4)
summary(clm5)
acf(residuals(clm5))

# predict and plot
newsugx <- seq(138.33,573.33, length=100)
newsugy <- exp(predict(clm5, newdata = list(sug_med=newsugx), int="c"))
head(newsugy)

sugpred_df <- data.frame(sug_med = newsugx,
                         newy = newsugy[,1],
                         lwr = newsugy[,2],
                         upr = newsugy[,3])
head(sugpred_df)

sug_med_plot <- ggplot(sugpred_df,aes(x=sug_med, y=newy))+
  geom_line(size=1, color="#000099")+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha=0.25), show.legend = FALSE)+
  geom_point(data=dat_sub, aes(x=sug_med, y=for_cov_roc))+
  theme_bw()+
  labs(x="Sugar price (USD/ton)", y="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Try non-linear model

a <- 1.1
b <- -0.0005*log(2)/a

cnlm3 <- nls(for_cov_roc ~ a*exp(-b*sug_med), start = list(a=a, b=b), data = dat_sub)
par(mfrow=c(2,2))
plot(nlsResiduals(cnlm1))
summary(cnlm1)


# Plot model fit (using original data)
p6 <- cnlm3 %>%
  augment() %>%
  ggplot(., aes(x = sug_med, y = for_cov_roc)) +
  geom_point(size = 1.5) +
  geom_line(aes(x = sug_med, y = .fitted),size=1, color="#000099") +
  theme_bw() +
  labs(x = "Sugar price",
       y = "Forest cover % change")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_grid(p5,p6)

# Create new dataframe for predicting 
cnlm3_newx <- data.frame(sug_med = seq(138.33,573.33,length=100))
cnlm3_newy <- predictNLS(cnlm3, newdata=cnlm3_newx, interval = "confidence", alpha = 0.05)
head(cnlm3_newy$summary)

# Plot model fit using new data
lwr_raw <- cnlm3_newy$summary[,11]
upr_raw <- cnlm3_newy$summary[,12]

cnlm3_newdf <- data.frame(newx = cnlm3_newx,
                          newy = cnlm3_newy$summary[,7],
                          lwr_raw = lwr_raw,
                          upr_raw = upr_raw)
cnlm3_newdf<- cnlm3_newdf %>% mutate(upr = newy+upr_raw) %>% mutate(lwr = newy -lwr_raw)

# plot
ggplot(cnlm3_newdf, aes(x=sug_med, y=newy))+
  geom_line(size=1, color="#000099")+
  geom_point(data=dat_sub, aes(x=sug_med, y=for_cov_roc))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha = 0.1), show.legend = FALSE)+
  theme_bw() +
  labs(x = "Sugar price",
       y = "Rate of forest cover loss (%)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## rub_med ####

plot(dat_sub$rub_med, dat_sub$for_cov_roc)
plot(dat_sub$rub_med, log(dat_sub$for_cov_roc))

ggplot(dat_sub, aes(x=rub_med, y=for_cov_roc))+
  geom_point()+
  geom_smooth(method="lm")

# try model with log(y)

clm6 <- lm(log(for_cov_roc) ~ rub_med, data = dat_sub)
par(mfrow=c(2,2))
plot(clm5)
summary(clm6)
acf(residuals(clm6))

# predictions and plotting
newrubx <- seq(585,4830,length=100)
newruby <- exp(predict(clm6, newdata = list(rub_med=newrubx), int = "c"))
rubpred_df <- data.frame(rub_med = newrubx,
                         newy = newruby[,"fit"],
                         upr = newruby[,"upr"],
                         lwr = newruby[,"lwr"])

ggplot(rubpred_df,aes(x=rub_med, y=newy))+
  geom_line()+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha=0.25), show.legend = FALSE)+
  geom_point(data=dat_sub, aes(x=rub_med, y=for_cov_roc))

# Try non-linear model

a <- 1.1
b <- -0.0005*log(2)/a

cnlm4 <- nls(for_cov_roc ~ a*exp(-b*rub_med), start = list(a=a, b=b), data = dat_sub)
par(mfrow=c(2,2))
plot(nlsResiduals(cnlm4))
summary(cnlm4)


# Plot model fit (using original data)
cnlm4 %>%
  augment() %>%
  ggplot(., aes(x = rub_med, y = for_cov_roc)) +
  geom_point(size = 1.5) +
  geom_line(aes(x = rub_med, y = .fitted),size=1, color="#000099") +
  theme_bw() +
  labs(x = "Rubber price",
       y = "Forest cover % change")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create new dataframe for predicting 
cnlm4_newx <- data.frame(rub_med = seq(585,4830,length=100))
cnlm4_newy <- predictNLS(cnlm4, newdata=cnlm4_newx, interval = "confidence", alpha = 0.05)
head(cnlm4_newy$summary)

# Plot model fit using new data
lwr_raw <- cnlm4_newy$summary[,11]
upr_raw <- cnlm4_newy$summary[,12]

cnlm4_newdf <- data.frame(newx = cnlm4_newx,
                          newy = cnlm4_newy$summary[,7],
                          lwr_raw = lwr_raw,
                          upr_raw = upr_raw)
cnlm4_newdf<- cnlm4_newdf %>% mutate(upr = newy+upr_raw) %>% mutate(lwr = newy -lwr_raw)

# plot
rubplot <- ggplot(cnlm4_newdf, aes(x=rub_med, y=newy))+
  geom_line(size=1, color="#000099")+
  geom_point(data=dat_sub, aes(x=rub_med, y=for_cov_roc))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha = 0.1), show.legend = FALSE)+
  theme_bw() +
  labs(x = "Rubber price (USD/ton)",
       y = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



### Modelling producer price variables ####
## prod_rice ####

plot(dat_sub$prod_rice, dat_sub$for_cov_roc)
plot(dat_sub$prod_rice, log(dat_sub$for_cov_roc))

ggplot(dat_sub, aes(x=prod_rice, y=for_cov_roc))+
  geom_point()+
  geom_smooth(method="lm")

# model with log(y)

plm1 <- lm(log(for_cov_roc) ~ prod_rice, data = dat_sub)
par(mfrow=c(2,2))
plot(plm1)    
summary(plm1)

# predict and plot
newx <- seq(96,270,length=100)
newy <- exp(predict(plm1, newdata=list(prod_rice=newx), int="c"))
head(newy)


prodrice_df <- data.frame(prod_rice = newx,
                          newy = newy[,"fit"],
                          lwr = newy[,"lwr"],
                          upr = newy[,"upr"])

prod_rice_plot <- ggplot(prodrice_df,aes(x=prod_rice, y=newy))+
  geom_line(size=1, color="#000099")+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha=0.25), show.legend = FALSE)+
  geom_point(data=dat_sub, aes(x=prod_rice, y=for_cov_roc))+
  labs(x="Producer price - rice (USD/Ton)", y="")

## prod_corn ####

plot(dat_sub$prod_corn, dat_sub$for_cov_roc)
plot(dat_sub$prod_corn, log(dat_sub$for_cov_roc))

ggplot(dat_sub, aes(x=prod_corn, y=for_cov_roc))+
  geom_point()+
  geom_smooth(method="lm")

# model with log(y)

plm2 <- lm(log(for_cov_roc) ~ prod_corn, data = dat_sub)
par(mfrow=c(2,2))
plot(plm2)    
summary(plm2)
acf(residuals(plm2))
pacf(residuals(plm2))

# predict and plot
newx <- seq(74,316,length=100)
newy <- exp(predict(plm2, newdata=list(prod_corn=newx), int="c"))
head(newy)


prodcorn_df <- data.frame(prod_corn = newx,
                          newy = newy[,"fit"],
                          lwr = newy[,"lwr"],
                          upr = newy[,"upr"])

par(mfrow=c(1,1))
ggplot(prodcorn_df,aes(x=prod_corn, y=newy))+
  geom_line()+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha=0.25), show.legend = FALSE)+
  geom_point(data=dat_sub, aes(x=prod_corn, y=for_cov_roc))+
  labs(x="Producer price - corn (USD/Ton)", y="Rate of forest loss (%)")


## prod_sug ####

plot(dat_sub$prod_sug, dat_sub$for_cov_roc)
plot(dat_sub$prod_sug, log(dat_sub$for_cov_roc))

ggplot(dat_sub, aes(x=prod_sug, y=for_cov_roc))+
  geom_point()+
  geom_smooth(method="lm")

# model with log(y)

plm3 <- lm(log(for_cov_roc) ~ prod_sug, data = dat_sub)
par(mfrow=c(2,2))
plot(plm3)    
summary(plm3)
acf(residuals(plm3))
pacf(residuals(plm3))

# predict and plot
newx <- seq(1192,3715,length=100)
newy <- exp(predict(plm3, newdata=list(prod_sug=newx), int="c"))
head(newy)


prodsug_df <- data.frame(prod_sug = newx,
                          newy = newy[,"fit"],
                          lwr = newy[,"lwr"],
                          upr = newy[,"upr"])

par(mfrow=c(1,1))
prod_sug_plot <- ggplot(prodsug_df,aes(x=prod_sug, y=newy))+
  geom_line(size=1, color="#000099")+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha=0.25), show.legend = FALSE)+
  geom_point(data=dat_sub, aes(x=prod_sug, y=for_cov_roc))+
  labs(x="Producer price - sugar (USD/Ton)", y="")


### mass plotting ####

# fdi - enlm2, fdiplot
# agr_gdp - elm4, agr_gdp_plot
# ind_gdp - elm5, ind_gdp_plot

# armi - clm1, armiplot
# rub_med - cnlm4, rubplot
# rice_med - clm3, rice_med_plot
# sug_med - clm5, sug_med_plot

# prod_rice - plm1, prod_rice_plot
# prod_sug - plm3, prod_sug_plot


econ_plots <- grid.arrange(
                arrangeGrob(agr_gdp_plot,ind_gdp_plot,fdiplot, ncol=2, 
                  left = textGrob("Rate of forest loss (%)", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2doc(file="econ_plots", width=8, height=8)

comm_plots <- grid.arrange(
                arrangeGrob(armiplot,rubplot,rice_med_plot,sug_med_plot, ncol=2, 
                  left = textGrob("Rate of forest loss (%)", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2doc(file="comm_plots", width=8, height=8)

prod_plots <- grid.arrange(
                arrangeGrob(prod_rice_plot,prod_sug_plot, ncol=1, 
                  left = textGrob("Rate of forest loss (%)", rot = 90,
                              gp=gpar(fontface="bold", fontsize=15)))
)

graph2doc(file="prod_plots", width=8, height=8)
