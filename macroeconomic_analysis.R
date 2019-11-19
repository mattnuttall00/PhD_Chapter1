### This script is the the analysis of macroeconomic predictors of forest cover loss in Cambodia, 1993-2015.This will constitute half of chapter 2 of my PhD. 

#### Load packages ####

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

#### Load & format data ####


## abreviations

# for_cov_area = absolute area covered by forest
# for_cov_perc = % of forest cover remaining with 1993 being the baseline (i.e. 100%)
# gdp = gdp per capita
# gdp_gr = % growth in gdp
# gni = gross national income per capita
# fdi = foregin direct investment
# ind_gdp = contribution of industrial sector to national gdp as a %
# agr_gdp = contribution of agricultural sector to national gdp as a %
# dev_agri = development flows to agricultural sector 
# dev_env = development flows to environment sector
# pop_den = population density
# armi = agricultural raw materials index
# cpi = crop production index
# nfi = non-food production index
# rice/rub/corn/sug_med = median annual market prices for rice, rubber, corn, sugar
# for_prod = total production values from forestry
# prod_rice/rub/cass/corn/sug = mean annual producer (farm gate) prices for rice, rubber, cassava, corn, sugar
# for_rem = rmaining absolute forest cover

dat <- read.csv("Data/national/macroeconomic_vars.csv")
dat$year <- as.factor(dat$year)
str(dat)

# I want to use the changes in variable quantities, not the raw quantities themselves.

dat_change <- data.frame(year = dat$year,
                         for_cov=dat$for_cov_area-lag(dat$for_cov_area,default=first(dat$for_cov_area)),
                         for_cov_perc=dat$for_cov_perc-lag(dat$for_cov_perc,default=first(dat$for_cov_perc)),
                         gdp=dat$gdp-lag(dat$gdp,default = first(dat$gdp)),
                         gdp_gr=dat$gdp_gr-lag(dat$gdp_gr,default = first(dat$gdp_gr)),
                         gni=dat$gni-lag(dat$gni,default = first(dat$gni)),
                         fdi=dat$fdi-lag(dat$fdi,default = first(dat$fdi)),
                         ind_gdp=dat$ind_gdp-lag(dat$ind_gdp,default = first(dat$ind_gdp)),
                         agr_gdp=dat$agr_gdp-lag(dat$agr_gdp,default = first(dat$agr_gdp)),
                         dev_agri=dat$dev_agri-lag(dat$dev_agri,default = first(dat$dev_agri)),
                         dev_env=dat$dev_env-lag(dat$dev_env,default = first(dat$dev_env)),
                         pop_den=dat$pop_den-lag(dat$pop_den,default = first(dat$pop_den)),
                         armi=dat$armi-lag(dat$armi,default = first(dat$armi)),
                         cpi=dat$cpi-lag(dat$cpi,default = first(dat$cpi)),
                         nfi=dat$nfi-lag(dat$nfi,default = first(dat$nfi)),
                         rice_med=dat$rice_med-lag(dat$rice_med,default = first(dat$rice_med)),
                         rub_med=dat$rub_med-lag(dat$rub_med,default = first(dat$rub_med)),
                         corn_med=dat$corn_med-lag(dat$corn_med,default = first(dat$corn_med)),
                         sug_med=dat$sug_med-lag(dat$sug_med,default = first(dat$sug_med)),
                         for_prod=dat$for_prod-lag(dat$for_prod,default = first(dat$for_prod)),
                         prod_rice=dat$prod_rice-lag(dat$prod_rice,default = first(dat$prod_rice)),
                         prod_rub=dat$prod_rub-lag(dat$prod_rub,default = first(dat$prod_rub)),
                         prod_cass=dat$prod_cass-lag(dat$prod_cass,default = first(dat$prod_cass)),
                         prod_corn=dat$prod_corn-lag(dat$prod_corn,default = first(dat$prod_corn)),
                         prod_sug=dat$prod_sug-lag(dat$prod_sug,default = first(dat$prod_sug)),
                         for_rem=dat$for_rem)

# I have left remaining forest (for_rem) as an absolute value because the change in for_rem would be identical to the change in for_cov, and the literature suggests that the absolute amount of remaining forest can be an important predictor

# remove 1993 as no values
dat_change <- dat_change[2:23, ]
str(dat_change)
head(dat_change)

# remove -ve sign for variables with only negative values
dat_change$for_cov <- abs(dat_change$for_cov)
dat_change$for_cov_perc <- abs(dat_change$for_cov_perc)

str(dat_change)

#### Exploratory plots ####

par(mfrow=c(1,2))
hist(dat_change$for_cov)
hist(dat_change$for_cov_perc)
# what distribution do I think this is?  non-negative continous.  But it's not bounded by 0 - there could feasibly be negative values.  In fact all of the values were originally negative (i.e. negative change in forest cover) but I have removed the -ve sign for ease of interpretation (i.e. changed to amount of forest lost).  I will try standard linear models and see what the residuals look like, before trying GLM which will require a distribution.

# gdp, gdp growth, gni, fdi, ind_gdp, agr_gdp, 
par(mfrow=c(3,2))
hist(dat_change$gdp)
hist(dat_change$gdp_gr)
hist(dat_change$gni)
hist(dat_change$fdi)
hist(dat_change$ind_gdp)
hist(dat_change$agr_gdp)

# GDP -big outlier for change in gdp - 1994.  This is because in 1993 there was the first general election, and this is when the country fully embraced a free market economy (see Chhair & Ung 2013). So strange things happened to GDP between 1993 and 1994 which is messing with the change in GDP and change in GDP growth.  I am not confident that the data points GDP for 1993, and therefore GDP growth for 1994, from the World Bank are reliable.  I will change (in dat_change only) gdp in 1994 to NA, and gdp_gr in 1994 to NAs in the "load & format data" section 

# Most of the gdp_gr values are negative, which indicates a slowing down of gdp growth. I wonder if this is misleading, becasue the gdp is still growing, just at a slower rate? Perhaps I can include raw gdp_gr as well.  There is also an outlier for gdp_gr, caused by the large change in gdp between 1994 and 1995.  ONe outlier in agr_gdp - in 1994 as well.  