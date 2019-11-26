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
library('corrplot')
library('MuMIn')

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
write.csv(dat_change, file="dat_change.csv")

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


# for_cov ~ for_prod
ggplot(dat_change,aes(x=for_prod, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=for_prod, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# one massive outlier in 2000 - huge drop in forestry production between 1999 and 2000

# try remove the outlier
ggplot(dat_change,aes(x=for_prod, y=for_cov))+ 
  geom_point(data=subset(dat_change,for_prod > -740000))+ 
  stat_smooth(data=subset(dat_change,for_prod > -740000), method="lm")
# negative slope - as changes in forest production become positive and larger, changes in forest cover decrease. This is the opposite of what I would expect. But there may be a lag here. Bit tricky to say which way the lag would go, but I would assume that forestry production metric would go up the year after the trees are cut down (but that assumes it takes a year for the timber to be processed and to be counted in the metric)

# test lead (opposite direction to the lags above) with outlier removed
ggplot(dat_change,aes(x=lead(for_prod), y=for_cov))+ 
  geom_point(data=subset(dat_change,for_prod > -740000))+ 
  stat_smooth(data=subset(dat_change,for_prod > -740000), method="lm")
# similar slope to non-lead

# test lag in other direction
ggplot(dat_change,aes(x=lag(for_prod), y=for_cov))+ 
  geom_point(data=subset(dat_change,for_prod > -740000))+ 
  stat_smooth(data=subset(dat_change,for_prod > -740000), method="lm")
# trend mostly goes away

# I don't actually think there would be a lag/lead for this variable. The speed with which forest is cleared and timber is processed is high, and so trees cut down in one year will probably make it into the data for forestry production in the same year


# for_cov ~ prod_rice
ggplot(dat_change,aes(x=prod_rice, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=prod_rice, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# no real trend

# 1 year lag
ggplot(dat_change,aes(x=lag(prod_rice), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# negative slope with the lag - as changes in producer price of rice become positive and get larger,the changes in forest cover get smaller. 

# 2 year lag
ggplot(dat_change,aes(x=lag(prod_rice, n=2L), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# intercept changes but not the slope 


# for_cov ~ prod_rubber
ggplot(dat_change,aes(x=prod_rub, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=prod_rub, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# not real trend

# 1 year lag
ggplot(dat_change,aes(x=lag(prod_rub), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# steep negative slope - as rubber prices become positive and get larger, changes in for_cov get smaller

# 2 year lag
ggplot(dat_change,aes(x=lag(prod_rub, n=2L), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# slope gets steeper


# for_cov ~ prod_cass
ggplot(dat_change,aes(x=prod_cass, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=prod_cass, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# positive slope - as prod_cass get positive and larger, changes in for_cov get larger

# 1 year lag
ggplot(dat_change,aes(x=lag(prod_cass), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# slope gets less steep

# 2 year lag
ggplot(dat_change,aes(x=lag(prod_cass, n=2L), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# trend reverses

# for_cov ~ prod_corn
ggplot(dat_change,aes(x=prod_corn, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=prod_corn, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# slight negative slope - prod_corn gets larger and positive, changes in for_cov get smaller

# lag
ggplot(dat_change,aes(x=lag(prod_corn), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# steeper slope

# 2 year lag
ggplot(dat_change,aes(x=lag(prod_corn, n=2L), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# steepr slope again


# for_cov ~ prod_sug
ggplot(dat_change,aes(x=prod_sug, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=prod_sug, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# slight negative slope - prod_sug gets larger and positive, changes in for_cov get smaller

# lag
ggplot(dat_change,aes(x=lag(prod_sug), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# steeper slope

# 2 year lag
ggplot(dat_change,aes(x=lag(prod_sug, n=2L), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# steepr slope again


# for_cov ~ for_rem
ggplot(dat_change,aes(x=for_rem, y=for_cov))+ geom_point()
ggplot(dat_change,aes(x=for_rem, y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# positive slope - when there is more forest remaining, changes in for_cov are larger

# lag
ggplot(dat_change,aes(x=lag(for_rem), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# steeper slope

# 2 year lag
ggplot(dat_change,aes(x=lag(for_rem, n=2L), y=for_cov))+ geom_point()+ stat_smooth(method="lm")
# similar slope
#### Standardising & correlations ####

# test for correlations before standardising 

correlation <- cor(dat_change[2:26], use = "complete.obs")
write.csv(correlation, file="macrovars_corr.csv")

# gdp and gni are highly correlated (0.99)
# gdp and prod_rub are slightly correlated (0.63)
# gdp_gr and prod_corn are slightly correlated (0.6)
# gni and prod_rub are slightly correlated (0.6)
# gni and prod_corn are slightly correlated (0.62)
# ind_gdp and agr_gdp are slightly correlated (-0.61)
# pop_den and prod_rub are correlated (-0.8)
# pop_den and for_rem are correlated (0.8)
# armi and rub_med are highly correlated (0.9)
# rice_med and prod_rice are slightly correlated (0.67)
# prod_rice and prod_rub are slightly correlated (0.63)
# prod_rub and for_rem are correlated (-0.73)
# for_rem and gdp slightly correlated (-0.6)

# now I want to check what happens to the correlations after standardisation
dat_scale <- apply(dat_change[ , c(2:26)], 2, scale)
dat_scale <- as.data.frame(dat_scale)
str(dat_scale)
head(dat_scale)

# check the apply function has scaled and centered
scale_check <- scale(dat_change$for_cov, center = T, scale = T)
cbind(dat_scale$for_cov,scale_check)

# correlations on scaled variables
corr_scale <- cor(dat_scale, use = "complete.obs")
write.csv(correlation, file="macrovars_corr_scale.csv")

# The results are exactly the same. Using gdp or gni depends on whether I am interested in the theory that the development of a country's economy influences forest loss, or the theory that increases in the population's socioeconomic situation influences forest loss.  Because the second half of this chapter will look at spatially-explicit, fine-scale socioeconomics, I will stick with gdp for now. 
# pop_den and prod_rub, and pop_den and for_rem are correlated.  I think the pop_den ~ prod_rub correlation is chance - I can't see how they would be related in real life. The two variables are there to explain totally different drivers, and so I will keep them both.  You could argue that pop_den ~ for_rem correlation makes sense (ie the higher the popualtion density the less forest will remain), but the relationship is not like that, and is actually mostly positive. Therefore I think it's likely that this is chance too.  I will keep both for now.
# The armi ~ rub_med correlation is more worrying, as these are likely to be genuinely correlated. If you plot armi against all of the other x_med variables, they are all slightly correlated (which is expected).  The only one that isn't is rice_med.  Therefore there is an argument to just use armi and rice_med.  But I am quite interested in the individual commodities, and so I will keep them in for now, but will probably remove them later.  
# The correlation between for_rem and prod_rub is interesting, and suggests and interaction, and so I don't want to remove either of them.

# useful disucssion about correlated predictors here: https://bit.ly/2qCMt9b
# this is also cool: https://bit.ly/33k5TwD

#### Create working dataframes ####

# I have decided not to use standardised variables. The coefficients from non-standardised variables are easier to interpret, predictions will be easier to understand, and the discussion will be easier if the values are in the original scale/unit.  Also see: 
# https://bit.ly/34otX2J
# https://bit.ly/2OpFZn7

str(dat_change)

# remove for_cov_perc
dat_work <- dat_change[ ,-3]

# remove gni
dat_work <- dat_work[ ,-5]

# In order to account for the effect of time in all of the models, I will fit a lm of for_cov ~ year, and use the residuals as a predictor in the subsequent models
dat_change$year <- as.numeric(dat_change$year)
time_mod <- lm(for_cov ~ year, data=dat_change)
summary(time_mod)
hist(time_mod$residuals)

# have a look at the model fit
newtimex <- seq(2,23,length=100)
newtimey <- predict(time_mod, newdata = list(year=newtimex), int="c")
df.newvars <- data_frame(newx = newtimex,
                         newy = as.numeric(newtimey[,"fit"]),
                         newupr = as.numeric(newtimey[,"upr"]),
                         newlwr = as.numeric(newtimey[,"lwr"]))

ggplot(df.newvars, aes(x = newx, y = newy))+
  geom_line()+
  geom_point(data = dat_change, aes(x=year, y=for_cov))+
  geom_ribbon(aes(ymin = newlwr, ymax = newupr, alpha=0.25))
  
time_resid <- time_mod$residuals

# add time variable to dat_work
dat_work$time <- time_resid


## I will start by modelling sets of predictors at a time.  All sets will have the time variable, and amount of forest remaining. The sets will be:

# macroeconomic - gdp, gdp_gr, fdi, ind_gdp, agr_gdp, dev_agri, dev_env, pop_den
dat_me <- dat_work[ ,c(1:10,24:25)]
str(dat_me)

# commodities - armi, cpi, nfi, rice_med, rub_med, corn_med, sug_med, for_prod
dat_com <- dat_work[ ,c(1:2,11:18,24:25)]
str(dat_com)

# producer prices - prod_rice, prod_rub, prod_cass, prod_corn, prod_sug
dat_prod <- dat_work[ ,c(1:2,19:25)]
str(dat_prod)

#### Models of subsets ####
  ## Macroeconomic set ####

str(dat_me)
head(dat_me)

# if gdp, gdp_gr, ind_gdp, agr_gdp are used then rows will be removed

# saturated model
me.mod1 <- lm(for_cov ~ gdp+gdp_gr+fdi+ind_gdp+agr_gdp+dev_agri+dev_env+pop_den+time, data=dat_me)
summary(me.mod1)
me.mod1$residuals

# dredge
system.time(dredge.me <- dredge(me.mod1, beta = "none", evaluate = TRUE, rank = AIC))
