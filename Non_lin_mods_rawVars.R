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

### Load data ####
dat_econ <- read_csv("macroeconomic_vars.csv")
dat_econ <- mutate(dat_econ, year = as.character(year))
str(dat_econ)
dat_resp <- read_csv("ForCov_LU_econVars_PCs.csv")

# create working dataframe (excluding principal components)
dat_work <- dat_resp %>% 
  mutate(year = as.character(year)) %>% 
  select(1:13) %>% 
  left_join(.,dat_econ, by = "year")

###---------------------------------------------------------------------------------------------
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


## Scatter plots of raw economic predictors ####
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

plot_grid(p6,p7,p8)

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

plot_grid(p12,p13,p14)

## The two extreme data points (1994 and 2015) are wildly different from the rest of the data, and are therefore acting as extreme outliers.  I will see what happens when I remove them

## subset data & remove outliers ####
# Remove the first two years (due to NA in for_cov_roc and the outlier), and the last year (due to outlier)
dat_sub <- dat_work %>% 
  filter(.,!year %in% c("1993","1994","2015"))

dat_sub1 <- dat_work %>% 
  filter(.,!year %in% c("1993","1994"))

## Scatter plots of raw economic variables and log-transformed Y ####
# scatter plot for_cov_roc ~ gdp
p17 <- ggplot(dat_sub, aes(x=gdp, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ fdi
p18 <- ggplot(dat_sub, aes(x=fdi, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ agr_gdp
p19 <- ggplot(dat_sub, aes(x=agr_gdp, y=log(for_cov_roc)))+
       geom_point()
  
# scatter plot for for_cov_roc ~ ind_gdp
p20 <- ggplot(dat_sub, aes(x=ind_gdp, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ dev_agri
p21 <- ggplot(dat_sub, aes(x=dev_agri, y=log(for_cov_roc)))+
       geom_point()

# scatter plot for for_cov_roc ~ dev_env
p22 <- ggplot(dat_sub, aes(x=dev_env, y=log(for_cov_roc)))+
        geom_point()

plot_grid(p18,p19,p20)


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


plot_grid(p23,p24,p27)

## Scatter plots of raw commodity predictors with log-transformed Y with subsetted data ####
p28 <- ggplot(dat_sub, aes(x=armi, y=log(for_cov_roc)))+
       geom_point()

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

plot_grid(p28,p29,p32)


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


## Scatter plots of raw producer predictors and log-transformed Y ####
# scatter plot for_cov_roc ~ prod_rice
p38 <- ggplot(dat_work, aes(x=prod_rice, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_rub 
p39 <- ggplot(dat_work, aes(x=prod_rub, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_cass
p40 <- ggplot(dat_sub, aes(x=prod_cass, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_corn
p41 <- ggplot(dat_work, aes(x=prod_corn, y=log(for_cov_roc)))+
  geom_point()

# scatter plot for for_cov_roc ~ prod_sug
p42 <- ggplot(dat_sub, aes(x=prod_sug, y=log(for_cov_roc)))+
  geom_point()

plot_grid(p40,p42)

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
ggplot(df.newvars, aes(x = newx, y = newy)) +
  geom_line() +
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha = 0.25))+
  geom_point(data = dat_sub, aes(x = fdi,y = for_cov_roc))+
  labs(x = "Foreign Direct Investment",
       y = "Forest cover % change")+
    theme_bw()+
  theme(legend.position="none")

# The model is a rubbish fit as far as I'm concerned.  I will try a linear model with log-transformed Y

elm2 <- lm(log(for_cov_roc) ~ fdi, data = dat_sub)
par(mfrow=c(2,3))
plot(elm2)
hist(elm2$residuals)
summary(elm2)

# Lets plot the model fit
elm2 %>%
  augment() %>%
  ggplot(., aes(x = fdi, y = exp(log.for_cov_roc.))) +
  geom_point(size = 1) +
  geom_line(aes(x = fdi, y = exp(.fitted))) +
  geom_ribbon(aes(ymin = exp(.fitted - (1.96*.se.fit)),
                  ymax = exp(.fitted + (1.96*.se.fit))),
              alpha = 0.5) +
  theme_bw() +
  labs(x = "Foreign Direct Investment",
       y = "Forest cover % change")

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

# Plot model fit
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

# compare exponential models
anova(enlm1,enlm2)
# 2 parameter model is better

# compare lm model to log linear model
anova(elm2,elm1)

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
newxvars <- seq(22.7,44.4, length = 100)
newyvars <- predict(elm3, newdata = list(agr_gdp=newxvars), int = "c")
str(newyvars)
head(newyvars)

# new dataframe
df.newvars <- data_frame(newx = newxvars,
                         newy = as.numeric(newyvars[,"fit"]),
                         newupr = as.numeric(newyvars[,"upr"]),
                         newlwr = as.numeric(newyvars[,"lwr"]))

# Plot
ggplot(df.newvars, aes(x = newx, y = newy)) +
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
# diagnostic plots suggest log-transforming Y makes it more non-linear

# Lets plot the model fit
elm4 %>%
  augment() %>%
  ggplot(., aes(x = agr_gdp, y = exp(log.for_cov_roc.))) +
  geom_point(size = 1) +
  geom_line(aes(x = agr_gdp, y = exp(.fitted)),size=1, color="#000099") +
  geom_ribbon(aes(ymin = exp(.fitted - (1.96*.se.fit)),
                  ymax = exp(.fitted + (1.96*.se.fit))),
              alpha = 0.5,fill="#000099") +
  theme_bw() +
  labs(x = "Agricultural contribution (%) to GDP",
       y = "Forest cover % change")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

graph2ppt(file="agr_gdp_plot", width=8, height=8)

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
newxvars <- seq(15,30.7, length = 100)
newyvars <- predict(elm5, newdata = list(ind_gdp=newxvars), int = "c")
str(newyvars)
head(newyvars)

# new dataframe
df.newvars <- data_frame(newx = newxvars,
                         newy = as.numeric(newyvars[,"fit"]),
                         newupr = as.numeric(newyvars[,"upr"]),
                         newlwr = as.numeric(newyvars[,"lwr"]))

# Plot
ggplot(df.newvars, aes(x = newx, y = newy)) +
  geom_line(size=1, color="#000099") +
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha = 0.25), fill="#000099")+
  geom_point(data = dat_sub, aes(x = ind_gdp,y = for_cov_roc))+
  labs(x = "Industrial contribution to GDP",
       y = "Forest cover % change")+
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
