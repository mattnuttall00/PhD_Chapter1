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

# remove gdp_gr for 1994 (see explanation in "exploratory plots" section)
dat[2,6] <- NA
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

# I have left remaining forest (for_rem) as an absolute value (rather than change) because the change in for_rem would be identical to the change in for_cov, and the literature suggests that the absolute amount of remaining forest can be an important predictor

# remove 1993 as no values
dat_change <- dat_change[2:23, ]
str(dat_change)
head(dat_change)

# remove -ve sign for variables with only negative values
dat_change$for_cov <- abs(dat_change$for_cov)
dat_change$for_cov_perc <- abs(dat_change$for_cov_perc)

# change 1994 gdp, ind_gdp, agr_gdp to NA (see explanation below in "exploratory plots")
dat_change[1,4] <- NA
dat_change[1,c(8,9)] <- NA

head(dat_change)

#### Exploratory plots ####

  ## Histograms ####
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

# GDP -big outlier for change in gdp - 1994.  This is because in 1993 there was the first general election, and this is when the country fully embraced a free market economy (see Chhair & Ung 2013). So strange things happened to GDP between 1993 and 1994 which is messing with the change in GDP and change in GDP growth.  I am not confident that the data points GDP for 1993, and therefore GDP growth for 1994, from the World Bank are reliable.  I will change gdp in 1994 to NA (in dat_change), and gdp_gr in 1994 to NAs (in dat) in the "load & format data" section 
# Histograms look way better now the above data points are removed

# One outlier in agr_gdp - in 1994 as well. I suppose that if I am removing values for gdp in 1994 then I ought to remove ind_gdp and agr_gdp from 1994 as well, seeing as the data are from the same source. 
# 1994 values for ind_gdp and agr_gdp removed. Histograms look better

# dev_agri, dev_env, pop_den, armi, cpi, nfi
hist(dat_change$dev_agri)
hist(dat_change$dev_env)
hist(dat_change$pop_den)
hist(dat_change$armi)
hist(dat_change$cpi)
hist(dat_change$nfi)
# one outlier in dev_agri (2015), but no valid reason to remove it.  Nothing else looks strange

# rice_med,rub_med,corn_med,sug_med
par(mfrow=c(2,2))
hist(dat_change$rice_med)
hist(dat_change$rub_med)
hist(dat_change$corn_med)
hist(dat_change$sug_med)
# large outlier for rice_med. Nothing suspicious - the price of rice is quite variable, and spikes in 2008, then slowly decrase again after 2008.  

# for_prod, for_rem
par(mfrow=c(1,2))
hist(dat_change$for_prod)
hist(dat_change$for_rem)
# big drop in forestry production around 1999. Changes in remaining forest all positive because forest is lost each year and I removed the negative sign

  ## Scatter plots ####

### NOTE: These scatter plots and models do not take time (ie trends over time for variables) into account and so should be treated with caution. They are for exploratory purposes only

# for_cov ~ gdp
ggplot(dat_change,aes(x=gdp, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
ggplot(dat_change,aes(x=gdp, y=for_cov))+ geom_point()
# negative slope - as the changes in gdp get larger and more positive, the changes in forest cover get smaller


# for_cov ~ gdp_gr
ggplot(dat_change,aes(x=gdp_gr, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=gdp_gr, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# Positive slope - as the changes in gdp_gr become positive and larger, the changes in forest cover get larger

# One outlier skewing the trend. Test removing it
ggplot(dat_change,aes(x=gdp_gr, y=for_cov))+ geom_point(data=subset(dat_change, gdp_gr <7))+ 
  stat_smooth(data=subset(dat_change, gdp_gr <7),method="lm")
# trend disappears without that datum 


# for_cov ~ gni
ggplot(dat_change,aes(x=gni, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=gni, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# negative slope - as changes in gni become positive and larger, changes in forest cover get smaller

# forest_cov ~ fdi
ggplot(dat_change,aes(x=fdi, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=fdi, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# negative slope - as changes in fdi get positive and larger, changes in forest cover get smaller

# forest_cov ~ ind_gdp
ggplot(dat_change,aes(x=ind_gdp, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=ind_gdp, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# positive slope - as changes in ind_gdp become positive and get larger, changes in for_cov get larger. Not what I was expecting

# for_cov ~ agr_gdp
ggplot(dat_change,aes(x=agr_gdp, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=agr_gdp, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# negative slope - when changes in agr_gdp are negative (ie contribution of agriculture to gdp gets smaller) the changes in for_cov are larger (ie more forest is lost), and when changes in agr_gdp become positive (ie agriculture contribution to gdp gets larger), changes in forest cover get smaller.  Not what I was expecting

# for_cov ~ dev_agri
ggplot(dat_change,aes(x=dev_agri, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=dev_agri, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# positive slop - when changes in dev_agri are negative, changes in for_cov are smaller, when changes in dev_agri are positive and larger, changes in for_cov get larger

# there is one outlier, I will see what happens when it is removed
ggplot(dat_change,aes(x=dev_agri, y=for_cov))+ geom_point(data=subset(dat_change, dev_agri > -600))+
  stat_smooth(data=subset(dat_change, dev_agri > -600),method="lm")
# trend disappears without that datum. Its the 1994 and 2015 values, which were causing issues with for_cov_roc previously. They are suspiciously identical, and low


# for_cov ~ dev_env
ggplot(dat_change,aes(x=dev_env, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=dev_env, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# very slight negative slope but doesn't look significant 

# for_cov ~ pop_den
ggplot(dat_change,aes(x=pop_den, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=pop_den, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# positive slope - as changes in pop_den get larger (all positive) then changes in for_cov get larger

# for_cov ~ armi
ggplot(dat_change,aes(x=armi, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=armi, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# very slight negative slope but doesn't look sig

# for_cov ~ cpi
ggplot(dat_change,aes(x=cpi, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=cpi, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# no trend - flat 

# for_cov ~ nfi
ggplot(dat_change,aes(x=nfi, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=nfi, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# negative slope - when chages in nfi are negative (ie production decreases) then changes in for_cov are larger, and when changes in nfi are positive (production increases) then changes in for_cov get smaller

# for_cov ~ rice_med
ggplot(dat_change,aes(x=rice_med, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=rice_med, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# no trend. Large outlier (as identified above).

# if we remove outlier
ggplot(dat_change,aes(x=rice_med, y=for_cov))+ geom_point(data=subset(dat_change, rice_med <300))+ 
  stat_smooth(data=subset(dat_change, rice_med <300),method="lm")
# positive slope when outlier is removed - ie as the changes in price of rice are negative (price goes down) then changes in for_cov get smaller, and when changes in price of rice are positive then changes in for_cov get larger


# for_cov ~ rub_med
ggplot(dat_change,aes(x=rub_med, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=rub_med, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# slilght positive slope - when rubber price goes down, changes in for_cov are low, and when price of rub goes up, changes in for_cov get larger

# test a lag
# 1 year lag
ggplot(dat_change,aes(x=lag(rub_med), y=for_cov))+ geom_point()+ 
  stat_smooth(method="lm")
# 2 year lag
ggplot(dat_change,aes(x=lag(rub_med, n=2L), y=for_cov))+ geom_point()+ 
  stat_smooth(method="lm")
# interestingly the relationship is what I would expect when there is no lag, ie as prices in rubber go up, forest cover loss is high. But when I introduce a 1 year lag (ie the for_cov value against the previous years rub_med value) the relationship disappears, and then with a 2-year lag the slope reverses - so as prices in rubber go up, the changes in for_cov get smaller


# for_cov ~ corn_med
ggplot(dat_change,aes(x=corn_med, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=corn_med, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# no relationship

# try lag
ggplot(dat_change,aes(x=lag(corn_med), y=for_cov))+ geom_point()+ 
  stat_smooth(method="lm")
# 2 year lag
ggplot(dat_change,aes(x=lag(corn_med, n=2L), y=for_cov))+ geom_point()+ 
  stat_smooth(method="lm")
# same as rub_med.  With lag, for_cov changes get smaller as prices of corn increase, and this slope gets steeper with a 2-year lag


# for_cov ~ sug_med
ggplot(dat_change,aes(x=sug_med, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=sug_med, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# flat

# lag
ggplot(dat_change,aes(x=lag(sug_med), y=for_cov))+ geom_point()+ 
  stat_smooth(method="lm")
# 2 year lag
ggplot(dat_change,aes(x=lag(sug_med, n=2L), y=for_cov))+ geom_point()+ 
  stat_smooth(method="lm")
# slope is negative with both lags - as prices in sugar go up, changes in for_cov go down


## NOTE: these lags aren't to be taken too seriously just yet as I haven't accounted for time trends

