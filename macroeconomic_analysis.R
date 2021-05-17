### This script is the the analysis of macroeconomic predictors of forest cover loss in Cambodia, 1993-2015.This will constitute half of chapter 1 of my PhD. 

#### Load packages ####

library('nlme')
library('cowplot')
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
library('car')
library('boot')
library('wesanderson')
library('ggplot2')
library('patchwork')
library('tidyverse')

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
# for_rem = remaining absolute forest cover

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
#write.csv(dat_change, file="dat_change.csv")

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

# armi with 1 year lag
ggplot(dat_change,aes(x=lag(armi), y=for_cov))+ geom_point()+ stat_smooth(method="lm")


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

# remove for_cov_perc and gni
dat_work <- dat_change %>% select(-c(for_cov_perc,gni)) 

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


## I will start by modelling sets of predictors at a time.  All sets will have the time variable, and amount of forest remaining. This is because time and for_rem will be in the final model and so they need to be in the subset models so that the effects of the subset variables are partial effects, accounting for time and remaining forest. The sets will be:

# macroeconomic - gdp, gdp_gr, fdi, ind_gdp, agr_gdp, dev_agri, dev_env, pop_den
dat_me <- dat_work %>% select(for_cov, gdp, gdp_gr, fdi, ind_gdp, agr_gdp, dev_agri, dev_env, pop_den, 
                              time, for_rem)
str(dat_me)

# commodities - armi, cpi, nfi, rice_med, rub_med, corn_med, sug_med, for_prod
dat_com <- dat_work %>% select(for_cov, armi, cpi, nfi, rice_med, rub_med, corn_med, sug_med, for_prod, 
                               time, for_rem)
str(dat_com)
#write.csv(dat_com, file="Data/national/commodity_set/dat_com.csv")

# producer prices - prod_rice, prod_rub, prod_cass, prod_corn, prod_sug
dat_prod <- dat_work %>% select(for_cov, prod_rice, prod_rub, prod_cass, prod_corn, prod_sug, 
                                time, for_rem)
str(dat_prod)
#write.csv(dat_prod, file="Data/national/producer_set/dat_prod.csv")



#### Models of subsets ####
  ## Macroeconomic set ####

str(dat_me)
head(dat_me)

# if gdp, gdp_gr, ind_gdp, agr_gdp are used then rows will be removed

# It is difficutly to identify the correct response distribution with so few data points.  Based on the "real" data, a gamma might be appropraite as the data are non-negatve and continous.  However the response (change in forest cover) is a derived variable, and although in my sample the data are non-negative, it is only non-negative because I have made it that way.  In reality, I am measuring forest loss and so the response is negative. Theoretically the data could be both positive (afforestation) and negative (deforestation), potentially centered around a mean.  Therefore the response could theoretically be normal. With so few data, it will likley make little difference which distribution I use. Nevertheless, in order to try and identify the best distribution, I will use some model selection procedures on both gamma and gaussian GLMs. 

# My rule for correlations are that if there is a correlation coefficient of 0.6 (or -0.6) or more, then one variable will be excluded from the set.

# my model selection procedure will follow these steps: 
# 1) use dredge on the unlagged and lagged predictors using a gaussian and then a gamma glm. The distribution that provides the models with the lowest AICc will be selected
# 2) Using the selected distribution, I will include all unlagged and lagged predictors in separate dredges. I will select the top models (AICc<6) and use model averaging for the final predictions. Because I am using a information-theoretic approach with model averaging, I will include models with much higher dAICc that most people advocate.  This is based on Burnham & Anderson 2002.  The model weights for all of the top candiadate models from dredge() are very low, which suggests that none of the models have a high probability of being the "best" model, and this supports a IT and model averaging approach. 
# 3) 2 * SE will be used as confidence intervals, following from Burnham & ANderson 2002 (p. 176)

# For this set, ind_gdp and agr_gdp are correlated (-0.61) and so one must be removed. Conceptually, the agricultural sector is likely to have a more important relationship with deforesation, and so I will remove ind_gdp.

# remove NA rows
dat_me1 <- dat_me[c(3:22), ]

# save
write.csv(dat_me1, file="Data/national/macroecon_set/dat_me1.csv")


    # Unlagged #####

# load data
dat_me1 <- read.csv("Data/national/macroecon_set/dat_me1.csv", header = T, stringsAsFactors = T)
dat_me1 <- dat_me1[ ,-1]

## saturated model with a gaussian distribution for unlagged predictors
me.mod.gaus.1 <- glm(for_cov ~ gdp+gdp_gr+fdi+agr_gdp+dev_agri+dev_env+pop_den+time+for_rem, 
              na.action="na.fail", family=gaussian, data=dat_me1)
summary(me.mod.gaus.1)

# dredge
me.dredge.gaus.1 <- dredge(me.mod.gaus.1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(me.dredge.gaus.1, file="Results/Macroeconomics/Dredge/me.dredge.gaus.1.csv")


## saturated model with gamma distribution for unlagged predictors
#me.mod.gam.1 <- glm(for_cov ~ gdp+gdp_gr+fdi+ind_gdp+agr_gdp+dev_agri+dev_env+pop_den+time, 
 #             na.action="na.fail", family=Gamma, data=dat_me1)
#summary(me.mod.gam.1)

# dredge
#me.dredge.gam.1 <- dredge(me.mod.gam.1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(me.dredge.gam.1, file="Results/Macroeconomics/Dredge/me.dredge.gam.1.csv")

# gaussian distribution produces the best models for the unlagged predictors. Therefore the top candidate models from that dredge will be used for model averaging.


## Model averaging

# AICc < 6
me.modAv.aicc6 <- model.avg(me.dredge.gaus.1, subset = delta < 6, fit = TRUE)
summary(me.modAv.aicc6)
plot(me.modAv.aicc6)

# Predict with the AICc<6 set

# pop_den
pop_den.newdata <- expand.grid(pop_den = seq(min(dat_me1$pop_den), max(dat_me1$pop_den), length=100),
                          time = mean(dat_me1$time),
                          gdp = mean(dat_me1$gdp),
                          agr_gdp = mean(dat_me1$agr_gdp),
                          dev_agri = mean(dat_me1$dev_agri),
                          dev_env = mean(dat_me1$dev_env),
                          fdi = mean(dat_me1$fdi),
                          gdp_gr = mean(dat_me1$gdp_gr),
                          for_rem = mean(dat_me1$for_rem))
pop_den.predict <- predict(me.modAv.aicc6, newdata=pop_den.newdata, se.fit=TRUE)
pop_den.predict <- data.frame(pop_den.predict)
pop_den.predict$lwr <- pop_den.predict$fit-2*pop_den.predict$se.fit
pop_den.predict$upr <- pop_den.predict$fit+2*pop_den.predict$se.fit
pop_den.predict <- cbind(pop_den.predict, pop_den.newdata)

# plot
pop_den_plot<- ggplot(data=pop_den.predict, aes(x=pop_den, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              geom_point(data=dat_me1, aes(x=pop_den, y=for_cov))+
              ylim(0,1500)+
              xlab("Changes in population density (pax/km^2) at time t")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/pop_den_plot.png", pop_den_plot, width = 30, height = 20, 
 #      units = "cm", dpi=300)

# gdp
gdp.newdata <- expand.grid(gdp = seq(min(dat_me1$gdp), max(dat_me1$gdp), length=100),
                          time = mean(dat_me1$time),
                          pop_den = mean(dat_me1$pop_den),
                          agr_gdp = mean(dat_me1$agr_gdp),
                          dev_agri = mean(dat_me1$dev_agri),
                          dev_env = mean(dat_me1$dev_env),
                          fdi = mean(dat_me1$fdi),
                          gdp_gr = mean(dat_me1$gdp_gr),
                          for_rem = mean(dat_me1$for_rem))
gdp.predict <- predict(me.modAv.aicc6, newdata=gdp.newdata, se.fit=TRUE)
gdp.predict <- data.frame(gdp.predict)
gdp.predict$lwr <- gdp.predict$fit-2*gdp.predict$se.fit
gdp.predict$upr <- gdp.predict$fit+2*gdp.predict$se.fit
gdp.predict <- cbind(gdp.predict, gdp.newdata)

# plot
gdp_plot <- ggplot(data=gdp.predict, aes(x=gdp, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  geom_point(data=dat_me1, aes(x=gdp, y=for_cov))+
  ylim(0,1500)+
  xlab("Changes in GDP per capita (USD Billion) at time t")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/gdp_plot.png", gdp_plot, width = 30, height = 20, 
 #      units = "cm", dpi=300)

# agr_gdp
agr_gdp.newdata <- expand.grid(agr_gdp = seq(min(dat_me1$agr_gdp), max(dat_me1$agr_gdp), length=100),
                          time = mean(dat_me1$time),
                          pop_den = mean(dat_me1$pop_den),
                          gdp = mean(dat_me1$gdp),
                          dev_agri = mean(dat_me1$dev_agri),
                          dev_env = mean(dat_me1$dev_env),
                          fdi = mean(dat_me1$fdi),
                          gdp_gr = mean(dat_me1$gdp_gr),
                          for_rem = mean(dat_me1$for_rem))
agr_gdp.predict <- predict(me.modAv.aicc6, newdata=agr_gdp.newdata, se.fit=TRUE)
agr_gdp.predict <- data.frame(agr_gdp.predict)
agr_gdp.predict$lwr <- agr_gdp.predict$fit-2*agr_gdp.predict$se.fit
agr_gdp.predict$upr <- agr_gdp.predict$fit+2*agr_gdp.predict$se.fit
agr_gdp.predict <- cbind(agr_gdp.predict, agr_gdp.newdata)

# plot
agr_gdp_plot <- ggplot(data=agr_gdp.predict, aes(x=agr_gdp, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  geom_point(data=dat_me1, aes(x=agr_gdp, y=for_cov))+
  ylim(0,1500)+
  xlab("Changes in Agricultural sector proprtion of GDP (%) at time t")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/agr_gdp_plot.png", agr_gdp_plot, width = 30, height = 20, 
 #      units = "cm", dpi=300)


# dev_agri
dev_agri.newdata <- expand.grid(dev_agri = seq(min(dat_me1$dev_agri), max(dat_me1$dev_agri), length=100),
                          time = mean(dat_me1$time),
                          pop_den = mean(dat_me1$pop_den),
                          gdp = mean(dat_me1$gdp),
                          agr_gdp = mean(dat_me1$agr_gdp),
                          dev_env = mean(dat_me1$dev_env),
                          fdi = mean(dat_me1$fdi),
                          gdp_gr = mean(dat_me1$gdp_gr),
                          for_rem = mean(dat_me1$for_rem))
dev_agri.predict <- predict(me.modAv.aicc6, newdata=dev_agri.newdata, se.fit=TRUE)
dev_agri.predict <- data.frame(dev_agri.predict)
dev_agri.predict$lwr <- dev_agri.predict$fit-2*dev_agri.predict$se.fit
dev_agri.predict$upr <- dev_agri.predict$fit+2*dev_agri.predict$se.fit
dev_agri.predict <- cbind(dev_agri.predict, dev_agri.newdata)

# plot
dev_agri_plot <- ggplot(data=dev_agri.predict, aes(x=dev_agri, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  geom_point(data=dat_me1, aes(x=dev_agri, y=for_cov))+
  ylim(0,1500)+
  xlab("Development flows to Agricultural sector (USD Millions) at time t")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/dev_agri_plot.png", dev_agri_plot, width = 30, height = 20,
 #      units = "cm", dpi=300)


# fdi
fdi.newdata <- expand.grid(fdi = seq(min(dat_me1$fdi), max(dat_me1$fdi), length=100),
                          time = mean(dat_me1$time),
                          pop_den = mean(dat_me1$pop_den),
                          gdp = mean(dat_me1$gdp),
                          agr_gdp = mean(dat_me1$agr_gdp),
                          dev_env = mean(dat_me1$dev_env),
                          dev_agri = mean(dat_me1$dev_agri),
                          gdp_gr = mean(dat_me1$gdp_gr),
                          for_rem = mean(dat_me1$for_rem))
fdi.predict <- predict(me.modAv.aicc6, newdata=fdi.newdata, se.fit=TRUE)
fdi.predict <- data.frame(fdi.predict)
fdi.predict$lwr <- fdi.predict$fit-2*fdi.predict$se.fit
fdi.predict$upr <- fdi.predict$fit+2*fdi.predict$se.fit
fdi.predict <- cbind(fdi.predict, fdi.newdata)

# plot
fdi_plot <- ggplot(data=fdi.predict, aes(x=fdi, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  geom_point(data=dat_me1, aes(x=fdi, y=for_cov))+
  ylim(0,1500)+
  xlab("Foreign Direct Investment (USD Millions) at time t")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/fdi_plot.png", fdi_plot , width = 30, height = 20, 
 #      units = "cm", dpi=300)


# dev_env
dev_env.newdata <- expand.grid(dev_env = seq(min(dat_me1$dev_env), max(dat_me1$dev_env), length=100),
                          time = mean(dat_me1$time),
                          pop_den = mean(dat_me1$pop_den),
                          gdp = mean(dat_me1$gdp),
                          agr_gdp = mean(dat_me1$agr_gdp),
                          fdi = mean(dat_me1$fdi),
                          dev_agri = mean(dat_me1$dev_agri),
                          gdp_gr = mean(dat_me1$gdp_gr),
                          for_rem = mean(dat_me1$for_rem))
dev_env.predict <- predict(me.modAv.aicc6, newdata=dev_env.newdata, se.fit=TRUE)
dev_env.predict <- data.frame(dev_env.predict)
dev_env.predict$lwr <- dev_env.predict$fit-2*dev_env.predict$se.fit
dev_env.predict$upr <- dev_env.predict$fit+2*dev_env.predict$se.fit
dev_env.predict <- cbind(dev_env.predict, dev_env.newdata)

# plot
dev_env_plot <- ggplot(data=dev_env.predict, aes(x=dev_env, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  geom_point(data=dat_me1, aes(x=dev_env, y=for_cov))+
  ylim(0,1500)+
  xlab("Development flows to the Environment sector (USD Millions) at time t")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/dev_env_plot.png", dev_env_plot, width = 30, height = 20, 
 #      units = "cm", dpi=300)


# gdp_gr
gdp_gr.newdata <- expand.grid(gdp_gr = seq(min(dat_me1$gdp_gr), max(dat_me1$gdp_gr), length=100),
                          time = mean(dat_me1$time),
                          pop_den = mean(dat_me1$pop_den),
                          gdp = mean(dat_me1$gdp),
                          agr_gdp = mean(dat_me1$agr_gdp),
                          fdi = mean(dat_me1$fdi),
                          dev_agri = mean(dat_me1$dev_agri),
                          dev_env = mean(dat_me1$dev_env),
                          for_rem = mean(dat_me1$for_rem))
gdp_gr.predict <- predict(me.modAv.aicc6, newdata=gdp_gr.newdata, se.fit=TRUE)
gdp_gr.predict <- data.frame(gdp_gr.predict)
gdp_gr.predict$lwr <- gdp_gr.predict$fit-2*gdp_gr.predict$se.fit
gdp_gr.predict$upr <- gdp_gr.predict$fit+2*gdp_gr.predict$se.fit
gdp_gr.predict <- cbind(gdp_gr.predict, gdp_gr.newdata)

# plot
gdP_gr_plot <- ggplot(data=gdp_gr.predict, aes(x=gdp_gr, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  geom_point(data=dat_me1, aes(x=gdp_gr, y=for_cov))+
  ylim(0,1500)+
  xlab("% growth of GDP per capita at time t")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/gdP_gr_plot.png", gdP_gr_plot, width = 30, height = 20, 
 #      units = "cm", dpi=300)



### plot grid
pop_den_plot + gdp_plot + agr_gdp_plot + dev_agri_plot + fdi_plot + dev_env_plot + gdP_gr_plot




    # 1 year lag #####

## Test lagged predictors 

# create data
dat_me_lag <- data.frame(for_cov = dat_me$for_cov,
                         time = dat_me$time,
                         gdp.lag1 = lag(dat_me$gdp),
                         gdp.lag2 = lag(dat_me$gdp, n=2L),
                         gdp_gr.lag1 = lag(dat_me$gdp_gr),
                         gdp_gr.lag2 = lag(dat_me$gdp_gr, n=2L),
                         fdi.lag1 = lag(dat_me$fdi),
                         fdi.lag2 = lag(dat_me$fdi, n=2L),
                         ind_gdp.lag1 = lag(dat_me$ind_gdp),
                         ind_gdp.lag2 = lag(dat_me$ind_gdp, n=2L),
                         agr_gdp.lag1 = lag(dat_me$agr_gdp),
                         agr_gdp.lag2 = lag(dat_me$agr_gdp, n=2L),
                         dev_agr.lag1 = lag(dat_me$dev_agri),
                         dev_agr.lag2 = lag(dat_me$dev_agri, n=2L),
                         dev_env.lag1 = lag(dat_me$dev_env),
                         dev_env.lag2 = lag(dat_me$dev_env, n=2L),
                         pop_den.lag1 = lag(dat_me$pop_den),
                         pop_den.lag2 = lag(dat_me$pop_den, n=2L),
                         for_rem = dat_me$for_rem)

# remove the rows that have NAs. gdp_gr.lag2 is the only variable with 4 rows of NAs once lagged. I don't want to lose another year of data for all of the other variables just because of that one variable. Therefore I will not include gdp_gr.lag2 in the models
dat_me_lag_sub <- dat_me_lag[c(4:22), ]

# save data
#write.csv(dat_me_lag_sub, file="Data/national/macroecon_set/dat_me_lag_sub.csv")

# load lagged data
dat_me_lag_sub <- read.csv("Data/national/macroecon_set/dat_me_lag_sub.csv", header = T)
dat_me_lag_sub <- dat_me_lag_sub[ ,-1]



## saturated model with gaussian distribution for 1-year lagged predictors
me.mod.gaus.lag.1 <- glm(for_cov ~ gdp.lag1+ gdp_gr.lag1+fdi.lag1+agr_gdp.lag1+
                           dev_agr.lag1+dev_env.lag1+pop_den.lag1+time+for_rem, 
                           na.action="na.fail", family=gaussian, data=dat_me_lag_sub)
summary(me.mod.gaus.lag.1)

# dredge
me.dredge.gaus.lag.1 <- dredge(me.mod.gaus.lag.1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(me.dredge.gaus.lag.1, file="Results/Macroeconomics/Dredge/me.dredge.gaus.lag.1.csv")

  
## saturated model with gamma distribution for 1-year lagged predictors
#me.mod.gam.lag.1 <- glm(for_cov ~ gdp.lag1+ gdp_gr.lag1+fdi.lag1+ind_gdp.lag1+agr_gdp.lag1+
                           #dev_agr.lag1+dev_env.lag1+pop_den.lag1+time, 
                           #na.action="na.fail", family=Gamma, data=dat_me_lag_sub)
#summary(me.mod.gam.lag.1)

# dredge
#me.dredge.gam.lag.1 <- dredge(me.mod.gam.lag.1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(me.dredge.gam.lag.1, file="Results/Macroeconomics/Dredge/me.dredge.gam.lag.1.csv")

## The gaussian distribution is better than the gamma distribution for lagged predictors too.


## Model averaging

# AICc < 6
me.modAv.aicc6.lag1 <- model.avg(me.dredge.gaus.lag.1, subset = delta < 6, fit = TRUE)
summary(me.dredge.gaus.lag.1)


# Predict with the AICc<6 set

# gdp.lag1
gdp.lag1.newdata <- expand.grid(gdp.lag1 = seq(min(dat_me_lag_sub$gdp.lag1), 
                                               max(dat_me_lag_sub$gdp.lag1), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag1 = mean(dat_me_lag_sub$pop_den.lag1),
                          agr_gdp.lag1 = mean(dat_me_lag_sub$agr_gdp.lag1),
                          dev_agr.lag1 = mean(dat_me_lag_sub$dev_agr.lag1),
                          dev_env.lag1 = mean(dat_me_lag_sub$dev_env.lag1),
                          fdi.lag1 = mean(dat_me_lag_sub$fdi.lag1),
                          gdp_gr.lag1 = mean(dat_me_lag_sub$gdp_gr.lag1),
                          for_rem = mean(dat_me_lag_sub$for_rem))
gdp.lag1.predict <- predict(me.modAv.aicc6.lag1, newdata=gdp.lag1.newdata, se.fit=TRUE)
gdp.lag1.predict <- data.frame(gdp.lag1.predict)
gdp.lag1.predict$lwr <- gdp.lag1.predict$fit-2*gdp.lag1.predict$se.fit
gdp.lag1.predict$upr <- gdp.lag1.predict$fit+2*gdp.lag1.predict$se.fit
gdp.lag1.predict <- cbind(gdp.lag1.predict, gdp.lag1.newdata)

# plot
gdp.lag1_plot <- ggplot(data=gdp.lag1.predict, aes(x=gdp.lag1, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Changes in GDP per capita (USD Billions) at time t-1")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/gdp.lag1_plot.png", gdp.lag1_plot, width = 30, height = 20, units = "cm", dpi=300)


# gdp_gr.lag1
gdp_gr.lag1.newdata <- expand.grid(gdp_gr.lag1 = seq(min(dat_me_lag_sub$gdp_gr.lag1), 
                                               max(dat_me_lag_sub$gdp_gr.lag1), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag1 = mean(dat_me_lag_sub$pop_den.lag1),
                          agr_gdp.lag1 = mean(dat_me_lag_sub$agr_gdp.lag1),
                          dev_agr.lag1 = mean(dat_me_lag_sub$dev_agr.lag1),
                          dev_env.lag1 = mean(dat_me_lag_sub$dev_env.lag1),
                          fdi.lag1 = mean(dat_me_lag_sub$fdi.lag1),
                          gdp.lag1 = mean(dat_me_lag_sub$gdp.lag1),
                          for_rem = mean(dat_me_lag_sub$for_rem))
gdp_gr.lag1.predict <- predict(me.modAv.aicc6.lag1, newdata=gdp_gr.lag1.newdata, se.fit=TRUE)
gdp_gr.lag1.predict <- data.frame(gdp_gr.lag1.predict)
gdp_gr.lag1.predict$lwr <- gdp_gr.lag1.predict$fit-2*gdp_gr.lag1.predict$se.fit
gdp_gr.lag1.predict$upr <- gdp_gr.lag1.predict$fit+2*gdp_gr.lag1.predict$se.fit
gdp_gr.lag1.predict <- cbind(gdp_gr.lag1.predict, gdp_gr.lag1.newdata)

# plot
gdp_gr.lag1_plot <- ggplot(data=gdp_gr.lag1.predict, aes(x=gdp_gr.lag1, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Changes in GDP growth (%) per capita at time t-1")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/gdp_gr.lag1_plot.png", gdp_gr.lag1_plot, width = 30, height = 20, units = "cm", dpi=300)


# fdi.lag1
fdi.lag1.newdata <- expand.grid(fdi.lag1 = seq(min(dat_me_lag_sub$fdi.lag1), 
                                               max(dat_me_lag_sub$fdi.lag1), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag1 = mean(dat_me_lag_sub$pop_den.lag1),
                          agr_gdp.lag1 = mean(dat_me_lag_sub$agr_gdp.lag1),
                          dev_agr.lag1 = mean(dat_me_lag_sub$dev_agr.lag1),
                          dev_env.lag1 = mean(dat_me_lag_sub$dev_env.lag1),
                          gdp_gr.lag1 = mean(dat_me_lag_sub$gdp_gr.lag1),
                          gdp.lag1 = mean(dat_me_lag_sub$gdp.lag1),
                          for_rem = mean(dat_me_lag_sub$for_rem))
fdi.lag1.predict <- predict(me.modAv.aicc6.lag1, newdata=fdi.lag1.newdata, se.fit=TRUE)
fdi.lag1.predict <- data.frame(fdi.lag1.predict)
fdi.lag1.predict$lwr <- fdi.lag1.predict$fit-2*fdi.lag1.predict$se.fit
fdi.lag1.predict$upr <- fdi.lag1.predict$fit+2*fdi.lag1.predict$se.fit
fdi.lag1.predict <- cbind(fdi.lag1.predict,fdi.lag1.newdata)

# plot
fdi.lag1_plot <- ggplot(data=fdi.lag1.predict, aes(x=fdi.lag1, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Foreign Direct Investment (USD Millions) at time t-1")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/fdi.lag1_plot.png", fdi.lag1_plot, width = 30, height = 20, units = "cm", dpi=300)


# agr_gdp.lag1
agr_gdp.lag1.newdata <- expand.grid(agr_gdp.lag1 = seq(min(dat_me_lag_sub$agr_gdp.lag1), 
                                               max(dat_me_lag_sub$agr_gdp.lag1), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag1 = mean(dat_me_lag_sub$pop_den.lag1),
                          fdi.lag1 = mean(dat_me_lag_sub$fdi.lag1),
                          dev_agr.lag1 = mean(dat_me_lag_sub$dev_agr.lag1),
                          dev_env.lag1 = mean(dat_me_lag_sub$dev_env.lag1),
                          gdp_gr.lag1 = mean(dat_me_lag_sub$gdp_gr.lag1),
                          gdp.lag1 = mean(dat_me_lag_sub$gdp.lag1),
                          for_rem = mean(dat_me_lag_sub$for_rem))
agr_gdp.lag1.predict <- predict(me.modAv.aicc6.lag1, newdata=agr_gdp.lag1.newdata, se.fit=TRUE)
agr_gdp.lag1.predict <- data.frame(agr_gdp.lag1.predict)
agr_gdp.lag1.predict$lwr <- agr_gdp.lag1.predict$fit-2*agr_gdp.lag1.predict$se.fit
agr_gdp.lag1.predict$upr <- agr_gdp.lag1.predict$fit+2*agr_gdp.lag1.predict$se.fit
agr_gdp.lag1.predict <- cbind(agr_gdp.lag1.predict,agr_gdp.lag1.newdata)

# plot
agr_gdp.lag1_plot <- ggplot(data=agr_gdp.lag1.predict, aes(x=agr_gdp.lag1, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Contribution of agricultural sector to GDP as proportion (%) at time t-1")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/agr_gdp.lag1_plot.png", agr_gdp.lag1_plot, width = 30, height = 20, units = "cm", dpi=300)


# dev_agr.lag1
dev_agr.lag1.newdata <- expand.grid(dev_agr.lag1 = seq(min(dat_me_lag_sub$dev_agr.lag1), 
                                               max(dat_me_lag_sub$dev_agr.lag1), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag1 = mean(dat_me_lag_sub$pop_den.lag1),
                          fdi.lag1 = mean(dat_me_lag_sub$fdi.lag1),
                          agr_gdp.lag1 = mean(dat_me_lag_sub$agr_gdp.lag1),
                          dev_env.lag1 = mean(dat_me_lag_sub$dev_env.lag1),
                          gdp_gr.lag1 = mean(dat_me_lag_sub$gdp_gr.lag1),
                          gdp.lag1 = mean(dat_me_lag_sub$gdp.lag1),
                          for_rem = mean(dat_me_lag_sub$for_rem))
dev_agr.lag1.predict <- predict(me.modAv.aicc6.lag1, newdata=dev_agr.lag1.newdata, se.fit=TRUE)
dev_agr.lag1.predict <- data.frame(dev_agr.lag1.predict)
dev_agr.lag1.predict$lwr <- dev_agr.lag1.predict$fit-2*dev_agr.lag1.predict$se.fit
dev_agr.lag1.predict$upr <- dev_agr.lag1.predict$fit+2*dev_agr.lag1.predict$se.fit
dev_agr.lag1.predict <- cbind(dev_agr.lag1.predict,dev_agr.lag1.newdata)

# plot
dev_agr.lag1_plot <- ggplot(data=dev_agr.lag1.predict, aes(x=dev_agr.lag1, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Development flows to agricultural sector (USD Millions) at time t-1")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/dev_agr.lag1_plot.png", dev_agr.lag1_plot, width = 30, height = 20, units = "cm", dpi=300)


# dev_env.lag1
dev_env.lag1.newdata <- expand.grid(dev_env.lag1 = seq(min(dat_me_lag_sub$dev_env.lag1), 
                                               max(dat_me_lag_sub$dev_env.lag1), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag1 = mean(dat_me_lag_sub$pop_den.lag1),
                          fdi.lag1 = mean(dat_me_lag_sub$fdi.lag1),
                          agr_gdp.lag1 = mean(dat_me_lag_sub$agr_gdp.lag1),
                          dev_agr.lag1 = mean(dat_me_lag_sub$dev_agr.lag1),
                          gdp_gr.lag1 = mean(dat_me_lag_sub$gdp_gr.lag1),
                          gdp.lag1 = mean(dat_me_lag_sub$gdp.lag1),
                          for_rem = mean(dat_me_lag_sub$for_rem))
dev_env.lag1.predict <- predict(me.modAv.aicc6.lag1, newdata=dev_env.lag1.newdata, se.fit=TRUE)
dev_env.lag1.predict <- data.frame(dev_env.lag1.predict)
dev_env.lag1.predict$lwr <- dev_env.lag1.predict$fit-2*dev_env.lag1.predict$se.fit
dev_env.lag1.predict$upr <- dev_env.lag1.predict$fit+2*dev_env.lag1.predict$se.fit
dev_env.lag1.predict <- cbind(dev_env.lag1.predict,dev_env.lag1.newdata)

# plot
dev_env.lag1_plot <- ggplot(data=dev_env.lag1.predict, aes(x=dev_env.lag1, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Development flows to environment sector (USD Millions) at time t-1")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/dev_env.lag1_plot.png", dev_env.lag1_plot, width = 30, height = 20, units = "cm", dpi=300)


# pop_den.lag1
pop_den.lag1.newdata <- expand.grid(pop_den.lag1 = seq(min(dat_me_lag_sub$pop_den.lag1), 
                                               max(dat_me_lag_sub$pop_den.lag1), length=100),
                          time = mean(dat_me_lag_sub$time),
                          dev_env.lag1 = mean(dat_me_lag_sub$dev_env.lag1),
                          fdi.lag1 = mean(dat_me_lag_sub$fdi.lag1),
                          agr_gdp.lag1 = mean(dat_me_lag_sub$agr_gdp.lag1),
                          dev_agr.lag1 = mean(dat_me_lag_sub$dev_agr.lag1),
                          gdp_gr.lag1 = mean(dat_me_lag_sub$gdp_gr.lag1),
                          gdp.lag1 = mean(dat_me_lag_sub$gdp.lag1),
                          for_rem = mean(dat_me_lag_sub$for_rem))
pop_den.lag1.predict <- predict(me.modAv.aicc6.lag1, newdata=pop_den.lag1.newdata, se.fit=TRUE)
pop_den.lag1.predict <- data.frame(pop_den.lag1.predict)
pop_den.lag1.predict$lwr <- pop_den.lag1.predict$fit-2*pop_den.lag1.predict$se.fit
pop_den.lag1.predict$upr <- pop_den.lag1.predict$fit+2*pop_den.lag1.predict$se.fit
pop_den.lag1.predict <- cbind(pop_den.lag1.predict,pop_den.lag1.newdata)

# plot
pop_den.lag1_plot <- ggplot(data=pop_den.lag1.predict, aes(x=pop_den.lag1, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Changes in population density (pax/km^2) at time t-1")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/pop_den.lag1_plot.png", pop_den.lag1_plot, width = 30, height = 20, units = "cm", dpi=300)

pop_den.lag1_plot + gdp.lag1_plot + agr_gdp.lag1_plot + dev_agr.lag1_plot + fdi.lag1_plot + dev_env.lag1_plot+
  gdp_gr.lag1_plot

#
    # 2 year lag ####

## saturated model with gaussian distribution for 2-year lagged predictors
me.mod.gaus.lag.2 <- glm(for_cov ~ gdp.lag2 + fdi.lag2 + agr_gdp.lag2 +
                           dev_agr.lag2 + dev_env.lag2 + pop_den.lag2 + time + for_rem, 
                           na.action="na.fail", family=gaussian, data=dat_me_lag_sub)
summary(me.mod.gaus.lag.2)

# dredge
me.dredge.gaus.lag.2 <- dredge(me.mod.gaus.lag.2, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(me.dredge.gaus.lag.2, file="Results/Macroeconomics/Dredge/me.dredge.gaus.lag.2.csv")


# AICc < 6
me.modAv.aicc6.lag2 <- model.avg(me.dredge.gaus.lag.2, subset = delta < 6, fit = TRUE)
summary(me.dredge.gaus.lag.2)


# Predict with the AICc<6 set

# gdp.lag2
gdp.lag2.newdata <- expand.grid(gdp.lag2 = seq(min(dat_me_lag_sub$gdp.lag2), 
                                               max(dat_me_lag_sub$gdp.lag2), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag2 = mean(dat_me_lag_sub$pop_den.lag2),
                          agr_gdp.lag2 = mean(dat_me_lag_sub$agr_gdp.lag2),
                          dev_agr.lag2 = mean(dat_me_lag_sub$dev_agr.lag2),
                          dev_env.lag2 = mean(dat_me_lag_sub$dev_env.lag2),
                          fdi.lag2 = mean(dat_me_lag_sub$fdi.lag2),
                          for_rem = mean(dat_me_lag_sub$for_rem))
gdp.lag2.predict <- predict(me.modAv.aicc6.lag2, newdata=gdp.lag2.newdata, se.fit=TRUE)
gdp.lag2.predict <- data.frame(gdp.lag2.predict)
gdp.lag2.predict$lwr <- gdp.lag2.predict$fit-2*gdp.lag2.predict$se.fit
gdp.lag2.predict$upr <- gdp.lag2.predict$fit+2*gdp.lag2.predict$se.fit
gdp.lag2.predict <- cbind(gdp.lag2.predict, gdp.lag2.newdata)

# plot
gdp.lag2_plot <- ggplot(data=gdp.lag2.predict, aes(x=gdp.lag2, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Changes in GDP per capita (USD Billions) at time t-2")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/gdp.lag2_plot.png", gdp.lag2_plot, width = 30, height = 20, units = "cm", dpi=300)


# fdi.lag2
fdi.lag2.newdata <- expand.grid(fdi.lag2 = seq(min(dat_me_lag_sub$fdi.lag2), 
                                               max(dat_me_lag_sub$fdi.lag2), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag2 = mean(dat_me_lag_sub$pop_den.lag2),
                          agr_gdp.lag2 = mean(dat_me_lag_sub$agr_gdp.lag2),
                          dev_agr.lag2 = mean(dat_me_lag_sub$dev_agr.lag2),
                          dev_env.lag2 = mean(dat_me_lag_sub$dev_env.lag2),
                          gdp.lag2 = mean(dat_me_lag_sub$gdp.lag2),
                          for_rem = mean(dat_me_lag_sub$for_rem))
fdi.lag2.predict <- predict(me.modAv.aicc6.lag2, newdata=fdi.lag2.newdata, se.fit=TRUE)
fdi.lag2.predict <- data.frame(fdi.lag2.predict)
fdi.lag2.predict$lwr <- fdi.lag2.predict$fit-2*fdi.lag2.predict$se.fit
fdi.lag2.predict$upr <- fdi.lag2.predict$fit+2*fdi.lag2.predict$se.fit
fdi.lag2.predict <- cbind(fdi.lag2.predict,fdi.lag2.newdata)

# plot
fdi.lag2_plot <- ggplot(data=fdi.lag2.predict, aes(x=fdi.lag2, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Foreign Direct Investment (USD Millions) at time t-2")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/fdi.lag2_plot.png", fdi.lag2_plot, width = 30, height = 20, units = "cm", dpi=300)


# agr_gdp.lag2
agr_gdp.lag2.newdata <- expand.grid(agr_gdp.lag2 = seq(min(dat_me_lag_sub$agr_gdp.lag2), 
                                               max(dat_me_lag_sub$agr_gdp.lag2), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag2 = mean(dat_me_lag_sub$pop_den.lag2),
                          fdi.lag2 = mean(dat_me_lag_sub$fdi.lag2),
                          dev_agr.lag2 = mean(dat_me_lag_sub$dev_agr.lag2),
                          dev_env.lag2 = mean(dat_me_lag_sub$dev_env.lag2),
                          gdp.lag2 = mean(dat_me_lag_sub$gdp.lag2),
                          for_rem = mean(dat_me_lag_sub$for_rem))
agr_gdp.lag2.predict <- predict(me.modAv.aicc6.lag2, newdata=agr_gdp.lag2.newdata, se.fit=TRUE)
agr_gdp.lag2.predict <- data.frame(agr_gdp.lag2.predict)
agr_gdp.lag2.predict$lwr <- agr_gdp.lag2.predict$fit-2*agr_gdp.lag2.predict$se.fit
agr_gdp.lag2.predict$upr <- agr_gdp.lag2.predict$fit+2*agr_gdp.lag2.predict$se.fit
agr_gdp.lag2.predict <- cbind(agr_gdp.lag2.predict,agr_gdp.lag2.newdata)

# plot
agr_gdp.lag2_plot <- ggplot(data=agr_gdp.lag2.predict, aes(x=agr_gdp.lag2, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Contribution of agricultural sector to GDP as proportion (%) at time t-2")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/agr_gdp.lag2_plot.png", agr_gdp.lag2_plot, width = 30, height = 20, units = "cm", dpi=300)


# dev_agr.lag2
dev_agr.lag2.newdata <- expand.grid(dev_agr.lag2 = seq(min(dat_me_lag_sub$dev_agr.lag2), 
                                               max(dat_me_lag_sub$dev_agr.lag2), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag2 = mean(dat_me_lag_sub$pop_den.lag2),
                          fdi.lag2 = mean(dat_me_lag_sub$fdi.lag2),
                          agr_gdp.lag2 = mean(dat_me_lag_sub$agr_gdp.lag2),
                          dev_env.lag2 = mean(dat_me_lag_sub$dev_env.lag2),
                          gdp.lag2 = mean(dat_me_lag_sub$gdp.lag2),
                          for_rem = mean(dat_me_lag_sub$for_rem))
dev_agr.lag2.predict <- predict(me.modAv.aicc6.lag2, newdata=dev_agr.lag2.newdata, se.fit=TRUE)
dev_agr.lag2.predict <- data.frame(dev_agr.lag2.predict)
dev_agr.lag2.predict$lwr <- dev_agr.lag2.predict$fit-2*dev_agr.lag2.predict$se.fit
dev_agr.lag2.predict$upr <- dev_agr.lag2.predict$fit+2*dev_agr.lag2.predict$se.fit
dev_agr.lag2.predict <- cbind(dev_agr.lag2.predict,dev_agr.lag2.newdata)

# plot
dev_agr.lag2_plot <- ggplot(data=dev_agr.lag2.predict, aes(x=dev_agr.lag2, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Development flows to agricultural sector (USD Millions) at time t-2")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/dev_agr.lag2_plot.png", dev_agr.lag2_plot, width = 30, height = 20, units = "cm", dpi=300)


# dev_env.lag2
dev_env.lag2.newdata <- expand.grid(dev_env.lag2 = seq(min(dat_me_lag_sub$dev_env.lag2), 
                                               max(dat_me_lag_sub$dev_env.lag2), length=100),
                          time = mean(dat_me_lag_sub$time),
                          pop_den.lag2 = mean(dat_me_lag_sub$pop_den.lag2),
                          fdi.lag2 = mean(dat_me_lag_sub$fdi.lag2),
                          agr_gdp.lag2 = mean(dat_me_lag_sub$agr_gdp.lag2),
                          dev_agr.lag2 = mean(dat_me_lag_sub$dev_agr.lag2),
                          gdp.lag2 = mean(dat_me_lag_sub$gdp.lag2),
                          for_rem = mean(dat_me_lag_sub$for_rem))
dev_env.lag2.predict <- predict(me.modAv.aicc6.lag2, newdata=dev_env.lag2.newdata, se.fit=TRUE)
dev_env.lag2.predict <- data.frame(dev_env.lag2.predict)
dev_env.lag2.predict$lwr <- dev_env.lag2.predict$fit-2*dev_env.lag2.predict$se.fit
dev_env.lag2.predict$upr <- dev_env.lag2.predict$fit+2*dev_env.lag2.predict$se.fit
dev_env.lag2.predict <- cbind(dev_env.lag2.predict,dev_env.lag2.newdata)

# plot
dev_env.lag2_plot <- ggplot(data=dev_env.lag2.predict, aes(x=dev_env.lag2, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Development flows to environment sector (USD Millions) at time t-2")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/dev_env.lag2_plot.png", dev_env.lag2_plot, width = 30, height = 20, units = "cm", dpi=300)


# pop_den.lag2
pop_den.lag2.newdata <- expand.grid(pop_den.lag2 = seq(min(dat_me_lag_sub$pop_den.lag2), 
                                               max(dat_me_lag_sub$pop_den.lag2), length=100),
                          time = mean(dat_me_lag_sub$time),
                          dev_env.lag2 = mean(dat_me_lag_sub$dev_env.lag2),
                          fdi.lag2 = mean(dat_me_lag_sub$fdi.lag2),
                          agr_gdp.lag2 = mean(dat_me_lag_sub$agr_gdp.lag2),
                          dev_agr.lag2 = mean(dat_me_lag_sub$dev_agr.lag2),
                          gdp.lag2 = mean(dat_me_lag_sub$gdp.lag2),
                          for_rem = mean(dat_me_lag_sub$for_rem))
pop_den.lag2.predict <- predict(me.modAv.aicc6.lag2, newdata=pop_den.lag2.newdata, se.fit=TRUE)
pop_den.lag2.predict <- data.frame(pop_den.lag2.predict)
pop_den.lag2.predict$lwr <- pop_den.lag2.predict$fit-2*pop_den.lag2.predict$se.fit
pop_den.lag2.predict$upr <- pop_den.lag2.predict$fit+2*pop_den.lag2.predict$se.fit
pop_den.lag2.predict <- cbind(pop_den.lag2.predict,pop_den.lag2.newdata)

# plot
pop_den.lag2_plot <- ggplot(data=pop_den.lag2.predict, aes(x=pop_den.lag2, y=fit))+
  geom_line(color="#339900", size=1)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
  ylim(0,1500)+
  xlab("Changes in population density (pax/km^2) at time t-2")+
  ylab("Amount of forest lost (ha) at time t")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/pop_den.lag2_plot.png", pop_den.lag2_plot, width = 30, height = 20, units = "cm", dpi=300)





    # all plots ####

pop_den_plot + gdp_plot + agr_gdp_plot + dev_agri_plot + fdi_plot + dev_env_plot + gdP_gr_plot


pop_den.lag1_plot + gdp.lag1_plot + agr_gdp.lag1_plot + dev_agr.lag1_plot + fdi.lag1_plot + dev_env.lag1_plot+
  gdp_gr.lag1_plot

pop_den.lag2_plot + gdp.lag2_plot + agr_gdp.lag2_plot + dev_agr.lag2_plot + fdi.lag2_plot +  dev_env.lag2_plot


# all plots together, with one row for each variable
me_plot_all <- pop_den_plot+pop_den.lag1_plot+pop_den.lag2_plot+
               gdp_plot+gdp.lag1_plot+gdp.lag2_plot+
               agr_gdp_plot+agr_gdp.lag1_plot+agr_gdp.lag2_plot+
               dev_agri_plot+dev_agr.lag1_plot+dev_agr.lag2_plot+
               dev_env_plot+dev_env.lag1_plot+dev_env.lag2_plot+
               fdi_plot+fdi.lag1_plot+fdi.lag2_plot+
                plot_layout(ncol=3)

# remove y-axis labels for all plots except plots 1, 4, 7, 10, 13, 16, and change those to letters
me_plot_all[[1]] <- me_plot_all[[1]] + ylab("a") + 
                    theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                                      size=20))
me_plot_all[[2]] <- me_plot_all[[2]] + theme(axis.title.y = element_blank())
me_plot_all[[3]] <- me_plot_all[[3]] + theme(axis.title.y = element_blank())
me_plot_all[[4]] <- me_plot_all[[4]] + ylab("b") + 
                    theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                                      size=20))
me_plot_all[[5]] <- me_plot_all[[5]] + theme(axis.title.y = element_blank())
me_plot_all[[6]] <- me_plot_all[[6]] + theme(axis.title.y = element_blank())
me_plot_all[[7]] <- me_plot_all[[7]] + ylab("c") + 
                    theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                                      size=20))
me_plot_all[[8]] <- me_plot_all[[8]] + theme(axis.title.y = element_blank())
me_plot_all[[9]] <- me_plot_all[[9]] + theme(axis.title.y = element_blank())
me_plot_all[[10]] <- me_plot_all[[10]] + ylab("d") + 
                     theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                                       size=20))
me_plot_all[[11]] <- me_plot_all[[11]] + theme(axis.title.y = element_blank())
me_plot_all[[12]] <- me_plot_all[[12]] + theme(axis.title.y = element_blank())
me_plot_all[[13]] <- me_plot_all[[13]] + ylab("e") + 
                     theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                                       size=20))
me_plot_all[[14]] <- me_plot_all[[14]] + theme(axis.title.y = element_blank())
me_plot_all[[15]] <- me_plot_all[[15]] + theme(axis.title.y = element_blank())
me_plot_all[[16]] <- me_plot_all[[16]] + ylab("f") + 
                     theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                                       size=20))
me_plot_all[[17]] <- me_plot_all[[17]] + theme(axis.title.y = element_blank())
me_plot_all[[18]] <- me_plot_all[[18]] + theme(axis.title.y = element_blank())

# remove x axis labels for all plots
me_plot_all[[1]] <- me_plot_all[[1]] + theme(axis.title.x = element_blank())
me_plot_all[[2]] <- me_plot_all[[2]] + theme(axis.title.x = element_blank())
me_plot_all[[3]] <- me_plot_all[[3]] + theme(axis.title.x = element_blank())
me_plot_all[[4]] <- me_plot_all[[4]] + theme(axis.title.x = element_blank())
me_plot_all[[5]] <- me_plot_all[[5]] + theme(axis.title.x = element_blank())
me_plot_all[[6]] <- me_plot_all[[6]] + theme(axis.title.x = element_blank())
me_plot_all[[7]] <- me_plot_all[[7]] + theme(axis.title.x = element_blank())
me_plot_all[[8]] <- me_plot_all[[8]] + theme(axis.title.x = element_blank())
me_plot_all[[9]] <- me_plot_all[[9]] + theme(axis.title.x = element_blank())
me_plot_all[[10]] <- me_plot_all[[10]] + theme(axis.title.x = element_blank())
me_plot_all[[11]] <- me_plot_all[[11]] + theme(axis.title.x = element_blank())
me_plot_all[[12]] <- me_plot_all[[12]] + theme(axis.title.x = element_blank())
me_plot_all[[13]] <- me_plot_all[[13]] + theme(axis.title.x = element_blank())
me_plot_all[[14]] <- me_plot_all[[14]] + theme(axis.title.x = element_blank())
me_plot_all[[15]] <- me_plot_all[[15]] + theme(axis.title.x = element_blank())
me_plot_all[[16]] <- me_plot_all[[16]] + theme(axis.title.x = element_blank())
me_plot_all[[17]] <- me_plot_all[[17]] + theme(axis.title.x = element_blank())
me_plot_all[[18]] <- me_plot_all[[18]] + theme(axis.title.x = element_blank())

# remove y-axis ticks from all plots except 1, 4, 7, 10, 13, 16
me_plot_all[[2]] <- me_plot_all[[2]] + theme(axis.text.y = element_blank())
me_plot_all[[3]] <- me_plot_all[[3]] + theme(axis.text.y = element_blank())
me_plot_all[[5]] <- me_plot_all[[5]] + theme(axis.text.y = element_blank())
me_plot_all[[6]] <- me_plot_all[[6]] + theme(axis.text.y = element_blank())
me_plot_all[[8]] <- me_plot_all[[8]] + theme(axis.text.y = element_blank())
me_plot_all[[9]] <- me_plot_all[[9]] + theme(axis.text.y = element_blank())
me_plot_all[[11]] <- me_plot_all[[11]] + theme(axis.text.y = element_blank())
me_plot_all[[12]] <- me_plot_all[[12]] + theme(axis.text.y = element_blank())
me_plot_all[[14]] <- me_plot_all[[14]] + theme(axis.text.y = element_blank())
me_plot_all[[15]] <- me_plot_all[[15]] + theme(axis.text.y = element_blank())
me_plot_all[[17]] <- me_plot_all[[17]] + theme(axis.text.y = element_blank())
me_plot_all[[18]] <- me_plot_all[[18]] + theme(axis.text.y = element_blank())

# add titles for plots 1, 2, 3 to show the lags
me_plot_all[[1]] <- me_plot_all[[1]] + ggtitle ("Time t") + theme(plot.title = element_text(hjust=0.5))
me_plot_all[[2]] <- me_plot_all[[2]] + ggtitle ("Time t+1")+ theme(plot.title = element_text(hjust=0.5))
me_plot_all[[3]] <- me_plot_all[[3]] + ggtitle ("Time t+2")+ theme(plot.title = element_text(hjust=0.5))

ggsave("Results/Macroeconomics/Plots/ME_plot_grid.png", me_plot_all, 
       width = 30, height = 30, unit="cm", dpi=300)

#
  ## Commodity / production set ####

str(dat_com)
head(dat_com)


    # Unlagged ####

# load data
dat_com <- read.csv("Data/national/commodity_set/dat_com.csv", header = T, stringsAsFactors = T)
dat_com <- dat_com[ ,-1]


# REMINDER - armi and rub_med are highly correlated, and armi and the other commodity prices are slightly correlated (except rice_med), but by less than 0.6. I will first remove armi so that I can investigate the individual commodities, then re-run the model with armi and no individual commodities to see if there is a difference

# UPDATe - there is not effect for the individual commodities or for ARMI, so t doesn't really matter. I will stick with the individual commodities


## saturated model with a gaussian distribution for unlagged predictors
com.mod.gaus.1 <- glm(for_cov ~ cpi + nfi + rice_med + rub_med + corn_med + sug_med + for_prod + 
                        time + for_rem, 
              na.action="na.fail", family=gaussian, data=dat_com)
summary(com.mod.gaus.1)

# dredge
com.dredge.gaus.1 <- dredge(com.mod.gaus.1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(com.dredge.gaus.1, file="Results/Macroeconomics/Dredge/com.dredge.gaus.1.csv")


## saturated model with gamma distribution for unlagged predictors
#com.mod.gam.1 <- glm(for_cov ~ cpi + nfi + rice_med + rub_med + corn_med + sug_med + for_prod + time, 
 #             na.action="na.fail", family=Gamma, data=dat_com)
#summary(com.mod.gam.1)

# dredge
#com.dredge.gam.1 <- dredge(com.mod.gam.1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(com.dredge.gam.1, file="Results/Macroeconomics/Dredge/com.dredge.gam.1.csv")


## The gaussian distribution produces the better models

## model averaging
# AICc < 6
com.modAv.aicc6 <- model.avg(com.dredge.gaus.1, subset = delta < 6, fit = TRUE)
summary(com.modAv.aicc6)


# Predict with the AICc<6 set

# cpi
cpi.newdata <- expand.grid(cpi = seq(min(dat_com$cpi), max(dat_com$cpi), length=100),
                          nfi = mean(dat_com$nfi),
                          rice_med = mean(dat_com$rice_med),
                          rub_med = mean(dat_com$rub_med),
                          corn_med = mean(dat_com$corn_med),
                          sug_med = mean(dat_com$sug_med),
                          for_prod = mean(dat_com$for_prod),
                          time = mean(dat_com$time),
                          for_rem = mean(dat_com$for_rem))
cpi.predict <- predict(com.modAv.aicc6, newdata=cpi.newdata, se.fit=TRUE)
cpi.predict <- data.frame(cpi.predict)
cpi.predict$lwr <- cpi.predict$fit-2*cpi.predict$se.fit
cpi.predict$upr <- cpi.predict$fit+2*cpi.predict$se.fit
cpi.predict <- cbind(cpi.predict, cpi.newdata)

# plot
cpi_plot <- ggplot(data=cpi.predict, aes(x=cpi, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Crop Production Index at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/cpi_plot.png", cpi_plot, width = 30, height = 20, units = "cm", dpi=300)


# nfi
nfi.newdata <- expand.grid(nfi = seq(min(dat_com$nfi), max(dat_com$nfi), length=100),
                          cpi = mean(dat_com$cpi),
                          rice_med = mean(dat_com$rice_med),
                          rub_med = mean(dat_com$rub_med),
                          corn_med = mean(dat_com$corn_med),
                          sug_med = mean(dat_com$sug_med),
                          for_prod = mean(dat_com$for_prod),
                          time = mean(dat_com$time),
                          for_rem = mean(dat_com$for_rem))
nfi.predict <- predict(com.modAv.aicc6, newdata=nfi.newdata, se.fit=TRUE)
nfi.predict <- data.frame(nfi.predict)
nfi.predict$lwr <- nfi.predict$fit-2*nfi.predict$se.fit
nfi.predict$upr <- nfi.predict$fit+2*nfi.predict$se.fit
nfi.predict <- cbind(nfi.predict, nfi.newdata)

# plot
nfi_plot <- ggplot(data=nfi.predict, aes(x=nfi, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Non-food Production Index at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/nfi_plot.png", nfi_plot, width = 30, height = 20, units = "cm", dpi=300)


# rice_med
rice_med.newdata <- expand.grid(rice_med = seq(min(dat_com$rice_med), max(dat_com$rice_med), length=100),
                          cpi = mean(dat_com$cpi),
                          nfi = mean(dat_com$nfi),
                          rub_med = mean(dat_com$rub_med),
                          corn_med = mean(dat_com$corn_med),
                          sug_med = mean(dat_com$sug_med),
                          for_prod = mean(dat_com$for_prod),
                          time = mean(dat_com$time),
                          for_rem = mean(dat_com$for_rem))
rice_med.predict <- predict(com.modAv.aicc6, newdata=rice_med.newdata, se.fit=TRUE)
rice_med.predict <- data.frame(rice_med.predict)
rice_med.predict$lwr <- rice_med.predict$fit-2*rice_med.predict$se.fit
rice_med.predict$upr <- rice_med.predict$fit+2*rice_med.predict$se.fit
rice_med.predict <- cbind(rice_med.predict, rice_med.newdata)

# plot
rice_med_plot <- ggplot(data=rice_med.predict, aes(x=rice_med, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Median price of rice at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/rice_med_plot.png", rice_med_plot, width = 30, height = 20, units = "cm", dpi=300)


# rub_med
rub_med.newdata <- expand.grid(rub_med = seq(min(dat_com$rub_med), max(dat_com$rub_med), length=100),
                          cpi = mean(dat_com$cpi),
                          nfi = mean(dat_com$nfi),
                          rice_med = mean(dat_com$rice_med),
                          corn_med = mean(dat_com$corn_med),
                          sug_med = mean(dat_com$sug_med),
                          for_prod = mean(dat_com$for_prod),
                          time = mean(dat_com$time),
                          for_rem = mean(dat_com$for_rem))
rub_med.predict <- predict(com.modAv.aicc6, newdata=rub_med.newdata, se.fit=TRUE)
rub_med.predict <- data.frame(rub_med.predict)
rub_med.predict$lwr <- rub_med.predict$fit-2*rub_med.predict$se.fit
rub_med.predict$upr <- rub_med.predict$fit+2*rub_med.predict$se.fit
rub_med.predict <- cbind(rub_med.predict, rub_med.newdata)

# plot
rub_med_plot <- ggplot(data=rub_med.predict, aes(x=rub_med, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Median price of rubber at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/rub_med_plot.png", rub_med_plot, width = 30, height = 20, units = "cm", dpi=300)



# corn_med
corn_med.newdata <- expand.grid(corn_med = seq(min(dat_com$corn_med), max(dat_com$corn_med), length=100),
                          cpi = mean(dat_com$cpi),
                          nfi = mean(dat_com$nfi),
                          rice_med = mean(dat_com$rice_med),
                          rub_med = mean(dat_com$rub_med),
                          sug_med = mean(dat_com$sug_med),
                          for_prod = mean(dat_com$for_prod),
                          time = mean(dat_com$time),
                          for_rem = mean(dat_com$for_rem))
corn_med.predict <- predict(com.modAv.aicc6, newdata=corn_med.newdata, se.fit=TRUE)
corn_med.predict <- data.frame(corn_med.predict)
corn_med.predict$lwr <- corn_med.predict$fit-2*corn_med.predict$se.fit
corn_med.predict$upr <- corn_med.predict$fit+2*corn_med.predict$se.fit
corn_med.predict <- cbind(corn_med.predict, corn_med.newdata)

# plot
corn_med_plot <- ggplot(data=corn_med.predict, aes(x=corn_med, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Median price of corn at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/corn_med_plot.png", corn_med_plot, width = 30, height = 20, units = "cm", dpi=300)


# sug_med
sug_med.newdata <- expand.grid(sug_med = seq(min(dat_com$sug_med), max(dat_com$sug_med), length=100),
                          cpi = mean(dat_com$cpi),
                          nfi = mean(dat_com$nfi),
                          rice_med = mean(dat_com$rice_med),
                          rub_med = mean(dat_com$rub_med),
                          corn_med = mean(dat_com$corn_med),
                          for_prod = mean(dat_com$for_prod),
                          time = mean(dat_com$time),
                          for_rem = mean(dat_com$for_rem))
sug_med.predict <- predict(com.modAv.aicc6, newdata=sug_med.newdata, se.fit=TRUE)
sug_med.predict <- data.frame(sug_med.predict)
sug_med.predict$lwr <- sug_med.predict$fit-2*sug_med.predict$se.fit
sug_med.predict$upr <- sug_med.predict$fit+2*sug_med.predict$se.fit
sug_med.predict <- cbind(sug_med.predict, sug_med.newdata)

# plot
sug_med_plot <- ggplot(data=sug_med.predict, aes(x=sug_med, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Median price of sugar at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/sug_med_plot.png", sug_med_plot, width = 30, height = 20, units = "cm", dpi=300)


# for_prod
for_prod.newdata <- expand.grid(for_prod = seq(min(dat_com$for_prod), max(dat_com$for_prod), length=100),
                          cpi = mean(dat_com$cpi),
                          nfi = mean(dat_com$nfi),
                          rice_med = mean(dat_com$rice_med),
                          rub_med = mean(dat_com$rub_med),
                          corn_med = mean(dat_com$corn_med),
                          sug_med = mean(dat_com$sug_med),
                          time = mean(dat_com$time),
                          for_rem = mean(dat_com$for_rem))
for_prod.predict <- predict(com.modAv.aicc6, newdata=for_prod.newdata, se.fit=TRUE)
for_prod.predict <- data.frame(for_prod.predict)
for_prod.predict$lwr <- for_prod.predict$fit-2*for_prod.predict$se.fit
for_prod.predict$upr <- for_prod.predict$fit+2*for_prod.predict$se.fit
for_prod.predict <- cbind(for_prod.predict, for_prod.newdata)

# plot
for_prod_plot <- ggplot(data=for_prod.predict, aes(x=for_prod, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Total timber production (m3) at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/for_prod_plot.png", for_prod_plot, width = 30, height = 20, units = "cm", dpi=300)



####


# test model with armi instead of the individual commodities
com.mod.gaus.1a <- glm(for_cov ~ cpi + nfi + armi + for_prod + time, 
              na.action="na.fail", family=gaussian, data=dat_com)
summary(com.mod.gaus.1a)

# dredge
com.dredge.gaus.1a <- dredge(com.mod.gaus.1a, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(com.dredge.gaus.1a, file="Results/Macroeconomics/Dredge/com.dredge.gaus.1a.csv")

## model averaging
# AICc < 6
com.modAv1a.aicc6 <- model.avg(com.dredge.gaus.1a, subset = delta < 6, fit = TRUE)
summary(com.modAv1a.aicc6)


# cpi
cpi.newdata1a <- expand.grid(cpi = seq(min(dat_com$cpi), max(dat_com$cpi), length=100),
                          nfi = mean(dat_com$nfi),
                          armi = mean(dat_com$armi),
                          for_prod = mean(dat_com$for_prod),
                          time = mean(dat_com$time))
cpi.predict <- predict(com.modAv1a.aicc6, newdata=cpi.newdata1a, se.fit=TRUE)
cpi.predict <- data.frame(cpi.predict)
cpi.predict$lwr <- cpi.predict$fit-2*cpi.predict$se.fit
cpi.predict$upr <- cpi.predict$fit+2*cpi.predict$se.fit
cpi.predict <- cbind(cpi.predict, cpi.newdata1a)

# plot
cpi_plot1a <- ggplot(data=cpi.predict, aes(x=cpi, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Crop Production Index at time t")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/cpi_plot1a.png", cpi_plot1a, width = 30, height = 20, units = "cm", dpi=300)


# nfi
nfi.newdata1a <- expand.grid(nfi = seq(min(dat_com$nfi), max(dat_com$nfi), length=100),
                          cpi = mean(dat_com$cpi),
                          for_prod = mean(dat_com$for_prod),
                          armi = mean(dat_com$armi),
                          time = mean(dat_com$time))
nfi.predict <- predict(com.modAv1a.aicc6, newdata=nfi.newdata1a, se.fit=TRUE)
nfi.predict <- data.frame(nfi.predict)
nfi.predict$lwr <- nfi.predict$fit-2*nfi.predict$se.fit
nfi.predict$upr <- nfi.predict$fit+2*nfi.predict$se.fit
nfi.predict <- cbind(nfi.predict, nfi.newdata1a)

# plot
nfi_plot1a <- ggplot(data=nfi.predict, aes(x=nfi, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Non-food Production Index at time t")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/nfi_plot1a.png", nfi_plot1a, width = 30, height = 20, 
       units = "cm", dpi=300)


# armi
armi.newdata1a <- expand.grid(armi = seq(min(dat_com$armi), max(dat_com$armi), length=100),
                          cpi = mean(dat_com$cpi),
                          for_prod = mean(dat_com$for_prod),
                          nfi = mean(dat_com$nfi),
                          time = mean(dat_com$time))
armi.predict <- predict(com.modAv1a.aicc6, newdata=armi.newdata1a, se.fit=TRUE)
armi.predict <- data.frame(armi.predict)
armi.predict$lwr <- armi.predict$fit-2*armi.predict$se.fit
armi.predict$upr <- armi.predict$fit+2*armi.predict$se.fit
armi.predict <- cbind(armi.predict, armi.newdata1a)

# plot
armi_plot1a <- ggplot(data=armi.predict, aes(x=armi, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Agricultural Raw Materials Index at time t")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/armi_plot1a.png", armi_plot1a, width = 30, height = 20, 
       units = "cm", dpi=300)


# for_prod
for_prod.newdata1a <- expand.grid(for_prod = seq(min(dat_com$for_prod), max(dat_com$for_prod), length=100),
                          cpi = mean(dat_com$cpi),
                          armi = mean(dat_com$armi),
                          nfi = mean(dat_com$nfi),
                          time = mean(dat_com$time))
for_prod.predict <- predict(com.modAv1a.aicc6, newdata=for_prod.newdata1a, se.fit=TRUE)
for_prod.predict <- data.frame(for_prod.predict)
for_prod.predict$lwr <- for_prod.predict$fit-2*for_prod.predict$se.fit
for_prod.predict$upr <- for_prod.predict$fit+2*for_prod.predict$se.fit
for_prod.predict <- cbind(for_prod.predict, for_prod.newdata1a)

# plot
for_prod_plot1a <- ggplot(data=for_prod.predict, aes(x=for_prod, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Total timber production (m3) at time t")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/for_prod_plot1a.png", for_prod_plot1a, width = 30, height = 20, 
       units = "cm", dpi=300)



    # 1 year lag ####


# create data
dat_com_lag <- data.frame(for_cov = dat_com$for_cov,
                         time = dat_com$time,
                         armi.lag1 = lag(dat_com$armi),
                         armi.lag2 = lag(dat_com$armi, n=2L),
                         cpi.lag1 = lag(dat_com$cpi),
                         cpi.lag2 = lag(dat_com$cpi, n=2L),
                         nfi.lag1 = lag(dat_com$nfi),
                         nfi.lag2 = lag(dat_com$nfi, n=2L),
                         rice_med.lag1 = lag(dat_com$rice_med),
                         rice_med.lag2 = lag(dat_com$rice_med, n=2L),
                         rub_med.lag1 = lag(dat_com$rub_med),
                         rub_med.lag2 = lag(dat_com$rub_med, n=2L),
                         corn_med.lag1 = lag(dat_com$corn_med),
                         corn_med.lag2 = lag(dat_com$corn_med, n=2L),
                         sug_med.lag1 = lag(dat_com$sug_med),
                         sug_med.lag2 = lag(dat_com$sug_med, n=2L),
                         for_prod.lag1 = lag(dat_com$for_prod),
                         for_prod.lag2 = lag(dat_com$for_prod, n=2L),
                         for_rem = dat_com$for_rem)

# remove first row for 1-year lag
dat_com_lag1 <- dat_com_lag[2:22, ]

# remove next row for 2-year lag
dat_com_lag2 <- dat_com_lag1[2:21, ]


## saturated model with a gaussian distribution for 1-year lag predictors
com.mod.gaus.lag1 <- glm(for_cov ~ cpi.lag1 + nfi.lag1 + rice_med.lag1 + rub_med.lag1 + 
                           corn_med.lag1 + sug_med.lag1 + for_prod.lag1 + time + for_rem, 
              na.action="na.fail", family=gaussian, data=dat_com_lag1)
summary(com.mod.gaus.lag1)

# dredge
com.dredge.gaus.lag1 <- dredge(com.mod.gaus.lag1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(com.dredge.gaus.lag1, file="Results/Macroeconomics/Dredge/com.dredge.gaus.lag1.csv")


## saturated model with gamma distribution for 1 year lagged predictors
#com.mod.gam.lag1 <- glm(for_cov ~ cpi.lag1 + nfi.lag1 + rice_med.lag1 + rub_med.lag1 + 
 #                         corn_med.lag1 + sug_med.lag1 + for_prod.lag1 + time, 
  #            na.action="na.fail", family=Gamma, data=dat_com_lag1)
#summary(com.mod.gam.lag1)

# dredge
#com.dredge.gam.lag1 <- dredge(com.mod.gam.lag1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(com.dredge.gam.lag1, file="Results/Macroeconomics/Dredge/com.dredge.gam.lag1.csv")


# models with Gaussian distribution are the better models


## model averaging
# AICc < 6
com.modAv.aicc6.lag1 <- model.avg(com.dredge.gaus.lag1, subset = delta < 6, fit = TRUE)
summary(com.modAv.aicc6.lag1)


# Predict with the AICc<6 set

# cpi.lag1
cpi.newdata.lag1 <- expand.grid(cpi.lag1 = seq(min(dat_com_lag1$cpi.lag1), max(dat_com_lag1$cpi.lag1), 
                                               length=100),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          rice_med.lag1 = mean(dat_com_lag1$rice_med.lag1),
                          rub_med.lag1 = mean(dat_com_lag1$rub_med.lag1),
                          corn_med.lag1 = mean(dat_com_lag1$corn_med.lag1),
                          sug_med.lag1 = mean(dat_com_lag1$sug_med.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time),
                          for_rem = mean(dat_com_lag1$for_rem))
cpi.predict.lag1 <- predict(com.modAv.aicc6.lag1, newdata=cpi.newdata.lag1, se.fit=TRUE)
cpi.predict.lag1 <- data.frame(cpi.predict.lag1)
cpi.predict.lag1$lwr <- cpi.predict.lag1$fit-2*cpi.predict.lag1$se.fit
cpi.predict.lag1$upr <- cpi.predict.lag1$fit+2*cpi.predict.lag1$se.fit
cpi.predict.lag1 <- cbind(cpi.predict.lag1, cpi.newdata.lag1)

# plot
cpi_plot.lag1 <- ggplot(data=cpi.predict.lag1, aes(x=cpi.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Crop Production Index at time t-1")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/cpi_plot.lag1.png", cpi_plot.lag1, width = 30, height = 20, 
 #      units = "cm", dpi=300)


# nfi.lag1
nfi.newdata.lag1 <- expand.grid(nfi.lag1 = seq(min(dat_com_lag1$nfi.lag1), max(dat_com_lag1$nfi.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          rice_med.lag1 = mean(dat_com_lag1$rice_med.lag1),
                          rub_med.lag1 = mean(dat_com_lag1$rub_med.lag1),
                          corn_med.lag1 = mean(dat_com_lag1$corn_med.lag1),
                          sug_med.lag1 = mean(dat_com_lag1$sug_med.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time),
                          for_rem = mean(dat_com_lag1$for_rem))
nfi.predict.lag1 <- predict(com.modAv.aicc6.lag1, newdata=nfi.newdata.lag1, se.fit=TRUE)
nfi.predict.lag1 <- data.frame(nfi.predict.lag1)
nfi.predict.lag1$lwr <- nfi.predict.lag1$fit-2*nfi.predict.lag1$se.fit
nfi.predict.lag1$upr <- nfi.predict.lag1$fit+2*nfi.predict.lag1$se.fit
nfi.predict.lag1 <- cbind(nfi.predict.lag1, nfi.newdata.lag1)

# plot
nfi_plot.lag1 <- ggplot(data=nfi.predict.lag1, aes(x=nfi.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
             # xlab("Non-food Production Index at time t-1")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/nfi_plot.lag1.png", nfi_plot.lag1, width = 30, height = 20, 
   #    units = "cm", dpi=300)


# rice_med.lag1
rice_med.newdata.lag1 <- expand.grid(rice_med.lag1 = seq(min(dat_com_lag1$rice_med.lag1), 
                                                         max(dat_com_lag1$rice_med.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          rub_med.lag1 = mean(dat_com_lag1$rub_med.lag1),
                          corn_med.lag1 = mean(dat_com_lag1$corn_med.lag1),
                          sug_med.lag1 = mean(dat_com_lag1$sug_med.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time),
                          for_rem = mean(dat_com_lag1$for_rem))
rice_med.predict.lag1 <- predict(com.modAv.aicc6.lag1, newdata=rice_med.newdata.lag1, se.fit=TRUE)
rice_med.predict.lag1 <- data.frame(rice_med.predict.lag1)
rice_med.predict.lag1$lwr <- rice_med.predict.lag1$fit-2*rice_med.predict.lag1$se.fit
rice_med.predict.lag1$upr <- rice_med.predict.lag1$fit+2*rice_med.predict.lag1$se.fit
rice_med.predict.lag1 <- cbind(rice_med.predict.lag1, rice_med.newdata.lag1)

# plot
rice_med_plot.lag1 <- ggplot(data=rice_med.predict.lag1, aes(x=rice_med.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
             # xlab("Median price of rice at time t-1")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/rice_med_plot.lag1.png", rice_med_plot.lag1, width = 30, 
#       height = 20, units = "cm", dpi=300)



# rub_med.lag1
rub_med.newdata.lag1 <- expand.grid(rub_med.lag1 = seq(min(dat_com_lag1$rub_med.lag1), 
                                                         max(dat_com_lag1$rub_med.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          rice_med.lag1 = mean(dat_com_lag1$rice_med.lag1),
                          corn_med.lag1 = mean(dat_com_lag1$corn_med.lag1),
                          sug_med.lag1 = mean(dat_com_lag1$sug_med.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time),
                          for_rem = mean(dat_com_lag1$for_rem))
rub_med.predict.lag1 <- predict(com.modAv.aicc6.lag1, newdata=rub_med.newdata.lag1, se.fit=TRUE)
rub_med.predict.lag1 <- data.frame(rub_med.predict.lag1)
rub_med.predict.lag1$lwr <- rub_med.predict.lag1$fit-2*rub_med.predict.lag1$se.fit
rub_med.predict.lag1$upr <- rub_med.predict.lag1$fit+2*rub_med.predict.lag1$se.fit
rub_med.predict.lag1 <- cbind(rub_med.predict.lag1, rub_med.newdata.lag1)

# plot
rub_med_plot.lag1 <- ggplot(data=rub_med.predict.lag1, aes(x=rub_med.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Median price of rubber at time t-1")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/rub_med_plot.lag1.png", rub_med_plot.lag1, width = 30, 
#       height = 20, units = "cm", dpi=300)



# corn_med.lag1
corn_med.newdata.lag1 <- expand.grid(corn_med.lag1 = seq(min(dat_com_lag1$corn_med.lag1), 
                                                         max(dat_com_lag1$corn_med.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          rice_med.lag1 = mean(dat_com_lag1$rice_med.lag1),
                          rub_med.lag1 = mean(dat_com_lag1$rub_med.lag1),
                          sug_med.lag1 = mean(dat_com_lag1$sug_med.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time),
                          for_rem = mean(dat_com_lag1$for_rem))
corn_med.predict.lag1 <- predict(com.modAv.aicc6.lag1, newdata=corn_med.newdata.lag1, se.fit=TRUE)
corn_med.predict.lag1 <- data.frame(corn_med.predict.lag1)
corn_med.predict.lag1$lwr <- corn_med.predict.lag1$fit-2*corn_med.predict.lag1$se.fit
corn_med.predict.lag1$upr <- corn_med.predict.lag1$fit+2*corn_med.predict.lag1$se.fit
corn_med.predict.lag1 <- cbind(corn_med.predict.lag1, corn_med.newdata.lag1)

# plot
corn_med_plot.lag1 <- ggplot(data=corn_med.predict.lag1, aes(x=corn_med.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
             # xlab("Median price of corn at time t-1")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/corn_med_plot.lag1.png", corn_med_plot.lag1, width = 30, 
#       height = 20, units = "cm", dpi=300)



# sug_med.lag1
sug_med.newdata.lag1 <- expand.grid(sug_med.lag1 = seq(min(dat_com_lag1$sug_med.lag1), 
                                                         max(dat_com_lag1$sug_med.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          rice_med.lag1 = mean(dat_com_lag1$rice_med.lag1),
                          rub_med.lag1 = mean(dat_com_lag1$rub_med.lag1),
                          corn_med.lag1 = mean(dat_com_lag1$corn_med.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time),
                          for_rem = mean(dat_com_lag1$for_rem))
sug_med.predict.lag1 <- predict(com.modAv.aicc6.lag1, newdata=sug_med.newdata.lag1, se.fit=TRUE)
sug_med.predict.lag1 <- data.frame(sug_med.predict.lag1)
sug_med.predict.lag1$lwr <- sug_med.predict.lag1$fit-2*sug_med.predict.lag1$se.fit
sug_med.predict.lag1$upr <- sug_med.predict.lag1$fit+2*sug_med.predict.lag1$se.fit
sug_med.predict.lag1 <- cbind(sug_med.predict.lag1, sug_med.newdata.lag1)

# plot
sug_med_plot.lag1 <- ggplot(data=sug_med.predict.lag1, aes(x=sug_med.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
           #   xlab("Median price of sugar at time t-1")+
           #   ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/sug_med_plot.lag1.png", sug_med_plot.lag1, width = 30, 
  #     height = 20, units = "cm", dpi=300)



# for_prod.lag1
for_prod.newdata.lag1 <- expand.grid(for_prod.lag1 = seq(min(dat_com_lag1$for_prod.lag1), 
                                                         max(dat_com_lag1$for_prod.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          rice_med.lag1 = mean(dat_com_lag1$rice_med.lag1),
                          rub_med.lag1 = mean(dat_com_lag1$rub_med.lag1),
                          corn_med.lag1 = mean(dat_com_lag1$corn_med.lag1),
                          sug_med.lag1 = mean(dat_com_lag1$sug_med.lag1),
                          time = mean(dat_com_lag1$time),
                          for_rem = mean(dat_com_lag1$for_rem))
for_prod.predict.lag1 <- predict(com.modAv.aicc6.lag1, newdata=for_prod.newdata.lag1, se.fit=TRUE)
for_prod.predict.lag1 <- data.frame(for_prod.predict.lag1)
for_prod.predict.lag1$lwr <- for_prod.predict.lag1$fit-2*for_prod.predict.lag1$se.fit
for_prod.predict.lag1$upr <- for_prod.predict.lag1$fit+2*for_prod.predict.lag1$se.fit
for_prod.predict.lag1 <- cbind(for_prod.predict.lag1, for_prod.newdata.lag1)

# plot
for_prod_plot.lag1 <- ggplot(data=for_prod.predict.lag1, aes(x=for_prod.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
            #  xlab("Total timber production (m3) at time t-1")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#gsave(file="Results/Macroeconomics/Plots/for_prod_plot.lag1.png", for_prod_plot.lag1, width = 30, 
#       height = 20, units = "cm", dpi=300)





###


# test model with armi instead of the individual commodities
com.mod.gaus.lag1a <- glm(for_cov ~ cpi.lag1 + nfi.lag1 + armi.lag1 + for_prod.lag1 + time, 
              na.action="na.fail", family=gaussian, data=dat_com_lag1)
summary(com.mod.gaus.lag1a)

# dredge
com.dredge.gaus.lag1a <- dredge(com.mod.gaus.lag1a, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(com.dredge.gaus.lag1a, file="Results/Macroeconomics/Dredge/com.dredge.gaus.lag1a.csv")

## model averaging
# AICc < 6
com.modAv1a.aicc6.lag1 <- model.avg(com.dredge.gaus.lag1a, subset = delta < 6, fit = TRUE)
summary(com.modAv1a.aicc6.lag1)


# cpi.lag1a
cpi.newdata.lag1a <- expand.grid(cpi.lag1 = seq(min(dat_com_lag1$cpi.lag1), max(dat_com_lag1$cpi.lag1), 
                                               length=100),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          armi.lag1 = mean(dat_com_lag1$armi.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time))
cpi.predict.lag1a <- predict(com.modAv1a.aicc6.lag1, newdata=cpi.newdata.lag1a, se.fit=TRUE)
cpi.predict.lag1a <- data.frame(cpi.predict.lag1a)
cpi.predict.lag1a$lwr <- cpi.predict.lag1a$fit-2*cpi.predict.lag1a$se.fit
cpi.predict.lag1a$upr <- cpi.predict.lag1a$fit+2*cpi.predict.lag1a$se.fit
cpi.predict.lag1a <- cbind(cpi.predict.lag1a, cpi.newdata.lag1a)

# plot
cpi_plot.lag1a <- ggplot(data=cpi.predict.lag1a, aes(x=cpi.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Crop Production Index at time t-1")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/cpi_plot.lag1a.png", cpi_plot.lag1a, width = 30, height = 20, 
       units = "cm", dpi=300)



# nfi.lag1a
nfi.newdata.lag1a <- expand.grid(nfi.lag1 = seq(min(dat_com_lag1$nfi.lag1), max(dat_com_lag1$nfi.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          armi.lag1 = mean(dat_com_lag1$armi.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time))
nfi.predict.lag1a <- predict(com.modAv1a.aicc6.lag1, newdata=nfi.newdata.lag1a, se.fit=TRUE)
nfi.predict.lag1a <- data.frame(nfi.predict.lag1a)
nfi.predict.lag1a$lwr <- nfi.predict.lag1a$fit-2*nfi.predict.lag1a$se.fit
nfi.predict.lag1a$upr <- nfi.predict.lag1a$fit+2*nfi.predict.lag1a$se.fit
nfi.predict.lag1a <- cbind(nfi.predict.lag1a, nfi.newdata.lag1a)

# plot
nfi_plot.lag1a <- ggplot(data=nfi.predict.lag1a, aes(x=nfi.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Non-food Production Index at time t-1")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/nfi_plot.lag1a.png", nfi_plot.lag1a, width = 30, height = 20, 
       units = "cm", dpi=300)



# armi.lag1a
armi.newdata.lag1a <- expand.grid(armi.lag1 = seq(min(dat_com_lag1$armi.lag1), max(dat_com_lag1$armi.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          for_prod.lag1 = mean(dat_com_lag1$for_prod.lag1),
                          time = mean(dat_com_lag1$time))
armi.predict.lag1a <- predict(com.modAv1a.aicc6.lag1, newdata=armi.newdata.lag1a, se.fit=TRUE)
armi.predict.lag1a <- data.frame(armi.predict.lag1a)
armi.predict.lag1a$lwr <- armi.predict.lag1a$fit-2*armi.predict.lag1a$se.fit
armi.predict.lag1a$upr <- armi.predict.lag1a$fit+2*armi.predict.lag1a$se.fit
armi.predict.lag1a <- cbind(armi.predict.lag1a, armi.newdata.lag1a)

# plot
armi_plot.lag1a <- ggplot(data=armi.predict.lag1a, aes(x=armi.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Agricultural Raw Materials Index at time t-1")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/armi_plot.lag1a.png", armi_plot.lag1a, width = 30, height = 20, 
       units = "cm", dpi=300)



# for_prod.lag1a
for_prod.newdata.lag1a <- expand.grid(for_prod.lag1 = seq(min(dat_com_lag1$for_prod.lag1), 
                                                      max(dat_com_lag1$for_prod.lag1), 
                                               length=100),
                          cpi.lag1 = mean(dat_com_lag1$cpi.lag1),
                          nfi.lag1 = mean(dat_com_lag1$nfi.lag1),
                          armi.lag1 = mean(dat_com_lag1$armi.lag1),
                          time = mean(dat_com_lag1$time))
for_prod.predict.lag1a <- predict(com.modAv1a.aicc6.lag1, newdata=for_prod.newdata.lag1a, se.fit=TRUE)
for_prod.predict.lag1a <- data.frame(for_prod.predict.lag1a)
for_prod.predict.lag1a$lwr <- for_prod.predict.lag1a$fit-2*for_prod.predict.lag1a$se.fit
for_prod.predict.lag1a$upr <- for_prod.predict.lag1a$fit+2*for_prod.predict.lag1a$se.fit
for_prod.predict.lag1a <- cbind(for_prod.predict.lag1a, for_prod.newdata.lag1a)

# plot
for_prod_plot.lag1a <- ggplot(data=for_prod.predict.lag1a, aes(x=for_prod.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              xlab("Total timber production (m3) at time t-1")+
              ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
ggsave(file="Results/Macroeconomics/Plots/for_prod_plot.lag1a.png", for_prod_plot.lag1a, width = 30,
       height = 20, units = "cm", dpi=300)


    # 2 year lag ####


## saturated model with a gaussian distribution for 2-year lag predictors
com.mod.gaus.lag2 <- glm(for_cov ~ cpi.lag2 + nfi.lag2 + rice_med.lag2 + rub_med.lag2 + 
                           corn_med.lag2 + sug_med.lag2 + for_prod.lag2 + time + for_rem, 
              na.action="na.fail", family=gaussian, data=dat_com_lag2)
summary(com.mod.gaus.lag2)

# dredge
com.dredge.gaus.lag2 <- dredge(com.mod.gaus.lag2, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(com.dredge.gaus.lag2, file="Results/Macroeconomics/Dredge/com.dredge.gaus.lag2.csv")


## saturated model with gamma distribution for 2 year lagged predictors
#com.mod.gam.lag2 <- glm(for_cov ~ cpi.lag2 + nfi.lag2 + rice_med.lag2 + rub_med.lag2 + 
 #                         corn_med.lag2 + sug_med.lag2 + for_prod.lag2 + time, 
  #            na.action="na.fail", family=Gamma, data=dat_com_lag2)
#summary(com.mod.gam.lag2)

# dredge
#com.dredge.gam.lag2 <- dredge(com.mod.gam.lag2, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(com.dredge.gam.lag2, file="Results/Macroeconomics/Dredge/com.dredge.gam.lag2.csv")


# models with Gaussian distribution are the better models


## model averaging
# AICc < 6
com.modAv.aicc6.lag2 <- model.avg(com.dredge.gaus.lag2, subset = delta < 6, fit = TRUE)
summary(com.modAv.aicc6.lag2)


# Predict with the AICc<6 set

# cpi.lag2
cpi.newdata.lag2 <- expand.grid(cpi.lag2 = seq(min(dat_com_lag2$cpi.lag2), max(dat_com_lag2$cpi.lag2), 
                                               length=100),
                          nfi.lag2 = mean(dat_com_lag2$nfi.lag2),
                          rice_med.lag2 = mean(dat_com_lag2$rice_med.lag2),
                          rub_med.lag2 = mean(dat_com_lag2$rub_med.lag2),
                          corn_med.lag2 = mean(dat_com_lag2$corn_med.lag2),
                          sug_med.lag2 = mean(dat_com_lag2$sug_med.lag2),
                          for_prod.lag2 = mean(dat_com_lag2$for_prod.lag2),
                          time = mean(dat_com_lag2$time),
                          for_rem = mean(dat_com_lag2$for_rem))
cpi.predict.lag2 <- predict(com.modAv.aicc6.lag2, newdata=cpi.newdata.lag2, se.fit=TRUE)
cpi.predict.lag2 <- data.frame(cpi.predict.lag2)
cpi.predict.lag2$lwr <- cpi.predict.lag2$fit-2*cpi.predict.lag2$se.fit
cpi.predict.lag2$upr <- cpi.predict.lag2$fit+2*cpi.predict.lag2$se.fit
cpi.predict.lag2 <- cbind(cpi.predict.lag2, cpi.newdata.lag2)

# plot
cpi_plot.lag2 <- ggplot(data=cpi.predict.lag2, aes(x=cpi.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Crop Production Index at time t-2")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/cpi_plot.lag2.png", cpi_plot.lag2, width = 30, height = 20, 
 #      units = "cm", dpi=300)



# nfi.lag2
nfi.newdata.lag2 <- expand.grid(nfi.lag2 = seq(min(dat_com_lag2$nfi.lag2), max(dat_com_lag2$nfi.lag2), 
                                               length=100),
                          cpi.lag2 = mean(dat_com_lag2$cpi.lag2),
                          rice_med.lag2 = mean(dat_com_lag2$rice_med.lag2),
                          rub_med.lag2 = mean(dat_com_lag2$rub_med.lag2),
                          corn_med.lag2 = mean(dat_com_lag2$corn_med.lag2),
                          sug_med.lag2 = mean(dat_com_lag2$sug_med.lag2),
                          for_prod.lag2 = mean(dat_com_lag2$for_prod.lag2),
                          time = mean(dat_com_lag2$time),
                          for_rem = mean(dat_com_lag2$for_rem))
nfi.predict.lag2 <- predict(com.modAv.aicc6.lag2, newdata=nfi.newdata.lag2, se.fit=TRUE)
nfi.predict.lag2 <- data.frame(nfi.predict.lag2)
nfi.predict.lag2$lwr <- nfi.predict.lag2$fit-2*nfi.predict.lag2$se.fit
nfi.predict.lag2$upr <- nfi.predict.lag2$fit+2*nfi.predict.lag2$se.fit
nfi.predict.lag2 <- cbind(nfi.predict.lag2, nfi.newdata.lag2)

# plot
nfi_plot.lag2 <- ggplot(data=nfi.predict.lag2, aes(x=nfi.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
             # xlab("Non-food Production Index at time t-2")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/nfiplot.lag2.png", nfi_plot.lag2, width = 30, height = 20, 
#       units = "cm", dpi=300)



# rice_med.lag2
rice_med.newdata.lag2 <- expand.grid(rice_med.lag2 = seq(min(dat_com_lag2$rice_med.lag2),
                                                         max(dat_com_lag2$rice_med.lag2), 
                                               length=100),
                          cpi.lag2 = mean(dat_com_lag2$cpi.lag2),
                          nfi.lag2 = mean(dat_com_lag2$nfi.lag2),
                          rub_med.lag2 = mean(dat_com_lag2$rub_med.lag2),
                          corn_med.lag2 = mean(dat_com_lag2$corn_med.lag2),
                          sug_med.lag2 = mean(dat_com_lag2$sug_med.lag2),
                          for_prod.lag2 = mean(dat_com_lag2$for_prod.lag2),
                          time = mean(dat_com_lag2$time),
                          for_rem = mean(dat_com_lag2$for_rem))
rice_med.predict.lag2 <- predict(com.modAv.aicc6.lag2, newdata=rice_med.newdata.lag2, se.fit=TRUE)
rice_med.predict.lag2 <- data.frame(rice_med.predict.lag2)
rice_med.predict.lag2$lwr <- rice_med.predict.lag2$fit-2*rice_med.predict.lag2$se.fit
rice_med.predict.lag2$upr <- rice_med.predict.lag2$fit+2*rice_med.predict.lag2$se.fit
rice_med.predict.lag2 <- cbind(rice_med.predict.lag2, rice_med.newdata.lag2)

# plot
rice_med_plot.lag2 <- ggplot(data=rice_med.predict.lag2, aes(x=rice_med.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Median rice price at time t-2")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/rice_med_plot.lag2.png", rice_med_plot.lag2, 
#       width = 30, height = 20, units = "cm", dpi=300)



# rub_med.lag2
rub_med.newdata.lag2 <- expand.grid(rub_med.lag2 = seq(min(dat_com_lag2$rub_med.lag2),
                                                         max(dat_com_lag2$rub_med.lag2), 
                                               length=100),
                          cpi.lag2 = mean(dat_com_lag2$cpi.lag2),
                          nfi.lag2 = mean(dat_com_lag2$nfi.lag2),
                          rice_med.lag2 = mean(dat_com_lag2$rice_med.lag2),
                          corn_med.lag2 = mean(dat_com_lag2$corn_med.lag2),
                          sug_med.lag2 = mean(dat_com_lag2$sug_med.lag2),
                          for_prod.lag2 = mean(dat_com_lag2$for_prod.lag2),
                          time = mean(dat_com_lag2$time),
                          for_rem = mean(dat_com_lag2$for_rem))
rub_med.predict.lag2 <- predict(com.modAv.aicc6.lag2, newdata=rub_med.newdata.lag2, se.fit=TRUE)
rub_med.predict.lag2 <- data.frame(rub_med.predict.lag2)
rub_med.predict.lag2$lwr <- rub_med.predict.lag2$fit-2*rub_med.predict.lag2$se.fit
rub_med.predict.lag2$upr <- rub_med.predict.lag2$fit+2*rub_med.predict.lag2$se.fit
rub_med.predict.lag2 <- cbind(rub_med.predict.lag2, rub_med.newdata.lag2)

# plot
rub_med_plot.lag2 <- ggplot(data=rub_med.predict.lag2, aes(x=rub_med.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
             # xlab("Median rubber price at time t-2")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/rub_med_plot.lag2.png", rub_med_plot.lag2, 
#       width = 30, height = 20, units = "cm", dpi=300)



# corn_med.lag2
corn_med.newdata.lag2 <- expand.grid(corn_med.lag2 = seq(min(dat_com_lag2$corn_med.lag2),
                                                         max(dat_com_lag2$corn_med.lag2), 
                                               length=100),
                          cpi.lag2 = mean(dat_com_lag2$cpi.lag2),
                          nfi.lag2 = mean(dat_com_lag2$nfi.lag2),
                          rice_med.lag2 = mean(dat_com_lag2$rice_med.lag2),
                          rub_med.lag2 = mean(dat_com_lag2$rub_med.lag2),
                          sug_med.lag2 = mean(dat_com_lag2$sug_med.lag2),
                          for_prod.lag2 = mean(dat_com_lag2$for_prod.lag2),
                          time = mean(dat_com_lag2$time),
                          for_rem = mean(dat_com_lag2$for_rem))
corn_med.predict.lag2 <- predict(com.modAv.aicc6.lag2, newdata=corn_med.newdata.lag2, se.fit=TRUE)
corn_med.predict.lag2 <- data.frame(corn_med.predict.lag2)
corn_med.predict.lag2$lwr <- corn_med.predict.lag2$fit-2*corn_med.predict.lag2$se.fit
corn_med.predict.lag2$upr <- corn_med.predict.lag2$fit+2*corn_med.predict.lag2$se.fit
corn_med.predict.lag2 <- cbind(corn_med.predict.lag2, corn_med.newdata.lag2)

# plot
corn_med_plot.lag2 <- ggplot(data=corn_med.predict.lag2, aes(x=corn_med.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
             # xlab("Median corn price at time t-2")+
            #  ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/corn_med_plot.lag2.png", corn_med_plot.lag2, 
#       width = 30, height = 20, units = "cm", dpi=300)



# sug_med.lag2
sug_med.newdata.lag2 <- expand.grid(sug_med.lag2 = seq(min(dat_com_lag2$sug_med.lag2),
                                                         max(dat_com_lag2$sug_med.lag2), 
                                               length=100),
                          cpi.lag2 = mean(dat_com_lag2$cpi.lag2),
                          nfi.lag2 = mean(dat_com_lag2$nfi.lag2),
                          rice_med.lag2 = mean(dat_com_lag2$rice_med.lag2),
                          rub_med.lag2 = mean(dat_com_lag2$rub_med.lag2),
                          corn_med.lag2 = mean(dat_com_lag2$corn_med.lag2),
                          for_prod.lag2 = mean(dat_com_lag2$for_prod.lag2),
                          time = mean(dat_com_lag2$time),
                          for_rem = mean(dat_com_lag2$for_rem))
sug_med.predict.lag2 <- predict(com.modAv.aicc6.lag2, newdata=sug_med.newdata.lag2, se.fit=TRUE)
sug_med.predict.lag2 <- data.frame(sug_med.predict.lag2)
sug_med.predict.lag2$lwr <- sug_med.predict.lag2$fit-2*sug_med.predict.lag2$se.fit
sug_med.predict.lag2$upr <- sug_med.predict.lag2$fit+2*sug_med.predict.lag2$se.fit
sug_med.predict.lag2 <- cbind(sug_med.predict.lag2, sug_med.newdata.lag2)

# plot
sug_med_plot.lag2 <- ggplot(data=sug_med.predict.lag2, aes(x=sug_med.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Median sugar price at time t-2")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/sug_med_plot.lag2.png", sug_med_plot.lag2, 
#       width = 30, height = 20, units = "cm", dpi=300)



# for_prod.lag2
for_prod.newdata.lag2 <- expand.grid(for_prod.lag2 = seq(min(dat_com_lag2$for_prod.lag2),
                                                         max(dat_com_lag2$for_prod.lag2), 
                                               length=100),
                          cpi.lag2 = mean(dat_com_lag2$cpi.lag2),
                          nfi.lag2 = mean(dat_com_lag2$nfi.lag2),
                          rice_med.lag2 = mean(dat_com_lag2$rice_med.lag2),
                          rub_med.lag2 = mean(dat_com_lag2$rub_med.lag2),
                          corn_med.lag2 = mean(dat_com_lag2$corn_med.lag2),
                          sug_med.lag2 = mean(dat_com_lag2$sug_med.lag2),
                          time = mean(dat_com_lag2$time),
                          for_rem = mean(dat_com_lag2$for_rem))
for_prod.predict.lag2 <- predict(com.modAv.aicc6.lag2, newdata=for_prod.newdata.lag2, se.fit=TRUE)
for_prod.predict.lag2 <- data.frame(for_prod.predict.lag2)
for_prod.predict.lag2$lwr <- for_prod.predict.lag2$fit-2*for_prod.predict.lag2$se.fit
for_prod.predict.lag2$upr <- for_prod.predict.lag2$fit+2*for_prod.predict.lag2$se.fit
for_prod.predict.lag2 <- cbind(for_prod.predict.lag2, for_prod.newdata.lag2)

# plot
for_prod_plot.lag2 <- ggplot(data=for_prod.predict.lag2, aes(x=for_prod.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Forest production (m3) at time t-2")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#(file="Results/Macroeconomics/Plots/for_prod_plot.lag2.png", for_prod_plot.lag2, 
  #     width = 30, height = 20, units = "cm", dpi=300)

## Based on the results of the above, I am not going to bother with replacing the individual commodidie with armi - it will make no difference





    # all plots ####


# all plots together, with one row for each variable
com_plot_all <- cpi_plot + cpi_plot.lag1 + cpi_plot.lag2 +
                nfi_plot + nfi_plot.lag1 + nfi_plot.lag2 +
                rice_med_plot + rice_med_plot.lag1 + rice_med_plot.lag2 +
                rub_med_plot + rub_med_plot.lag1 + rub_med_plot.lag2 +
                corn_med_plot + corn_med_plot.lag1 + corn_med_plot.lag2 +
                sug_med_plot + sug_med_plot.lag1 + sug_med_plot.lag2 +
                for_prod_plot + for_prod_plot.lag1 + for_prod_plot.lag2 +
            plot_layout(ncol=3)

# remove y-axis labels for all plots except plots 1, 4, 7, 10, 13, 16, 19 and change those to letters
com_plot_all[[1]] <- com_plot_all[[1]] + ylab("a") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
com_plot_all[[2]] <- com_plot_all[[2]] + theme(axis.title.y = element_blank())
com_plot_all[[3]] <- com_plot_all[[3]] + theme(axis.title.y = element_blank())
com_plot_all[[4]] <- com_plot_all[[4]] + ylab("b") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
com_plot_all[[5]] <- com_plot_all[[5]] + theme(axis.title.y = element_blank())
com_plot_all[[6]] <- com_plot_all[[6]] + theme(axis.title.y = element_blank())
com_plot_all[[7]] <- com_plot_all[[7]] + ylab("c") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
com_plot_all[[8]] <- com_plot_all[[8]] + theme(axis.title.y = element_blank())
com_plot_all[[9]] <- com_plot_all[[9]] + theme(axis.title.y = element_blank())
com_plot_all[[10]] <- com_plot_all[[10]] + ylab("d") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
com_plot_all[[11]] <- com_plot_all[[11]] + theme(axis.title.y = element_blank())
com_plot_all[[12]] <- com_plot_all[[12]] + theme(axis.title.y = element_blank())
com_plot_all[[13]] <- com_plot_all[[13]] + ylab("e") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
com_plot_all[[14]] <- com_plot_all[[14]] + theme(axis.title.y = element_blank())
com_plot_all[[15]] <- com_plot_all[[15]] + theme(axis.title.y = element_blank())
com_plot_all[[16]] <- com_plot_all[[16]] + ylab("f") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
com_plot_all[[17]] <- com_plot_all[[17]] + theme(axis.title.y = element_blank())
com_plot_all[[18]] <- com_plot_all[[18]] + theme(axis.title.y = element_blank())
com_plot_all[[19]] <- com_plot_all[[19]] + ylab("g") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
com_plot_all[[20]] <- com_plot_all[[20]] + theme(axis.title.y = element_blank())
com_plot_all[[21]] <- com_plot_all[[21]] + theme(axis.title.y = element_blank())

# remove x axis labels for all plots
com_plot_all[[1]] <- com_plot_all[[1]] + theme(axis.title.x = element_blank())
com_plot_all[[2]] <- com_plot_all[[2]] + theme(axis.title.x = element_blank())
com_plot_all[[3]] <- com_plot_all[[3]] + theme(axis.title.x = element_blank())
com_plot_all[[4]] <- com_plot_all[[4]] + theme(axis.title.x = element_blank())
com_plot_all[[5]] <- com_plot_all[[5]] + theme(axis.title.x = element_blank())
com_plot_all[[6]] <- com_plot_all[[6]] + theme(axis.title.x = element_blank())
com_plot_all[[7]] <- com_plot_all[[7]] + theme(axis.title.x = element_blank())
com_plot_all[[8]] <- com_plot_all[[8]] + theme(axis.title.x = element_blank())
com_plot_all[[9]] <- com_plot_all[[9]] + theme(axis.title.x = element_blank())
com_plot_all[[10]] <- com_plot_all[[10]] + theme(axis.title.x = element_blank())
com_plot_all[[11]] <- com_plot_all[[11]] + theme(axis.title.x = element_blank())
com_plot_all[[12]] <- com_plot_all[[12]] + theme(axis.title.x = element_blank())
com_plot_all[[13]] <- com_plot_all[[13]] + theme(axis.title.x = element_blank())
com_plot_all[[14]] <- com_plot_all[[14]] + theme(axis.title.x = element_blank())
com_plot_all[[15]] <- com_plot_all[[15]] + theme(axis.title.x = element_blank())
com_plot_all[[16]] <- com_plot_all[[16]] + theme(axis.title.x = element_blank())
com_plot_all[[17]] <- com_plot_all[[17]] + theme(axis.title.x = element_blank())
com_plot_all[[18]] <- com_plot_all[[18]] + theme(axis.title.x = element_blank())
com_plot_all[[19]] <- com_plot_all[[19]] + theme(axis.title.x = element_blank())
com_plot_all[[20]] <- com_plot_all[[20]] + theme(axis.title.x = element_blank())
com_plot_all[[21]] <- com_plot_all[[21]] + theme(axis.title.x = element_blank())

# remove y-axis ticks from all plots except 1, 4, 7, 10, 13, 16, 19
com_plot_all[[2]] <- com_plot_all[[2]] + theme(axis.text.y = element_blank())
com_plot_all[[3]] <- com_plot_all[[3]] + theme(axis.text.y = element_blank())
com_plot_all[[5]] <- com_plot_all[[5]] + theme(axis.text.y = element_blank())
com_plot_all[[6]] <- com_plot_all[[6]] + theme(axis.text.y = element_blank())
com_plot_all[[8]] <- com_plot_all[[8]] + theme(axis.text.y = element_blank())
com_plot_all[[9]] <- com_plot_all[[9]] + theme(axis.text.y = element_blank())
com_plot_all[[11]] <- com_plot_all[[11]] + theme(axis.text.y = element_blank())
com_plot_all[[12]] <- com_plot_all[[12]] + theme(axis.text.y = element_blank())
com_plot_all[[14]] <- com_plot_all[[14]] + theme(axis.text.y = element_blank())
com_plot_all[[15]] <- com_plot_all[[15]] + theme(axis.text.y = element_blank())
com_plot_all[[17]] <- com_plot_all[[17]] + theme(axis.text.y = element_blank())
com_plot_all[[18]] <- com_plot_all[[18]] + theme(axis.text.y = element_blank())
com_plot_all[[20]] <- com_plot_all[[20]] + theme(axis.text.y = element_blank())
com_plot_all[[21]] <- com_plot_all[[21]] + theme(axis.text.y = element_blank())

# add titles for plots 1, 2, 3 to show the lags
com_plot_all[[1]] <- com_plot_all[[1]] + ggtitle ("Time t") + theme(plot.title = element_text(hjust=0.5))
com_plot_all[[2]] <- com_plot_all[[2]] + ggtitle ("Time t+1")+ theme(plot.title = element_text(hjust=0.5))
com_plot_all[[3]] <- com_plot_all[[3]] + ggtitle ("Time t+2")+ theme(plot.title = element_text(hjust=0.5))

ggsave("Results/Macroeconomics/Plots/COM_plot_grid.png", com_plot_all, 
       width = 30, height = 30, unit="cm", dpi=300)


  ##Producer price set ####

str(dat_prod)
head(dat_prod)

# prod_rice and prod_rub are slightly correlated (0.63).  In the context of deforestation, I think that rubber is much more likely to have an impact.  But let's have a quick look

rice <- glm(for_cov ~ prod_rice + time, family=gaussian, data=dat_prod)
summary(rice)
rub <- glm(for_cov ~ prod_rub + time, family=gaussian, data=dat_prod)
summary(rub)
# both are shit, but the prod_rub has a slightly larger effect size and smaller SE, so I will go with rub


    # Unlagged ####

# load data
dat_prod <- read.csv("Data/national/producer_set/dat_prod.csv", header = T, stringsAsFactors = T)
dat_prod <- dat_prod[ ,-1]


## saturated model with a gaussian distribution for unlagged predictors
prod.mod.gaus.1 <- glm(for_cov ~ prod_rub + prod_cass + prod_corn + prod_sug + time +for_rem, 
              na.action="na.fail", family=gaussian, data=dat_prod)
summary(prod.mod.gaus.1)

# dredge
prod.dredge.gaus.1 <- dredge(prod.mod.gaus.1, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(prod.dredge.gaus.1, file="Results/Macroeconomics/Dredge/prod.dredge.gaus.1.csv")


## saturated model with gamma distribution for unlagged predictors
#prod.mod.gam.1 <- glm(for_cov ~ prod_rub + prod_cass + prod_corn + prod_sug + time, 
 #             na.action="na.fail", family=Gamma, data=dat_prod)
#summary(prod.mod.gam.1)

# dredge
#prod.dredge.gam.1 <- dredge(prod.mod.gam.1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(prod.dredge.gam.1, file="Results/Macroeconomics/Dredge/prod.dredge.gam.1.csv")


## The gaussian distribution produces the better models

## model averaging
# AICc < 6
prod.modAv.aicc6 <- model.avg(prod.dredge.gaus.1, subset = delta < 6, fit = TRUE)
summary(prod.modAv.aicc6)


# Predict with the AICc<6 set

# prod_rub
prod_rub.newdata <- expand.grid(prod_rub = seq(min(dat_prod$prod_rub), max(dat_prod$prod_rub), 
                                                 length=100),
                          prod_cass = mean(dat_prod$prod_cass),
                          prod_corn = mean(dat_prod$prod_corn),
                          prod_sug = mean(dat_prod$prod_sug),
                          time = mean(dat_prod$time),
                          for_rem = mean(dat_prod$for_rem))
prod_rub.predict <- predict(prod.modAv.aicc6, newdata=prod_rub.newdata, se.fit=TRUE)
prod_rub.predict <- data.frame(prod_rub.predict)
prod_rub.predict$lwr <- prod_rub.predict$fit-2*prod_rub.predict$se.fit
prod_rub.predict$upr <- prod_rub.predict$fit+2*prod_rub.predict$se.fit
prod_rub.predict <- cbind(prod_rub.predict, prod_rub.newdata)


# plot
prod_rub_plot <- ggplot(data=prod_rub.predict, aes(x=prod_rub, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Rubber producer price at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_rub_plot.png", prod_rub_plot, width = 30, 
 #      height = 20, units = "cm", dpi=300)


# prod_cass
prod_cass.newdata <- expand.grid(prod_cass = seq(min(dat_prod$prod_cass), max(dat_prod$prod_cass), 
                                                 length=100),
                          prod_rub = mean(dat_prod$prod_rub),
                          prod_corn = mean(dat_prod$prod_corn),
                          prod_sug = mean(dat_prod$prod_sug),
                          time = mean(dat_prod$time),
                          for_rem = mean(dat_prod$for_rem))
prod_cass.predict <- predict(prod.modAv.aicc6, newdata=prod_cass.newdata, se.fit=TRUE)
prod_cass.predict <- data.frame(prod_cass.predict)
prod_cass.predict$lwr <- prod_cass.predict$fit-2*prod_cass.predict$se.fit
prod_cass.predict$upr <- prod_cass.predict$fit+2*prod_cass.predict$se.fit
prod_cass.predict <- cbind(prod_cass.predict, prod_cass.newdata)


# plot
prod_cass_plot <- ggplot(data=prod_cass.predict, aes(x=prod_cass, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Cassava producer price at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_cass_plot.png", prod_cass_plot, width = 30, 
#       height = 20, units = "cm", dpi=300)


# prod_corn
prod_corn.newdata <- expand.grid(prod_corn = seq(min(dat_prod$prod_corn), max(dat_prod$prod_corn), 
                                                 length=100),
                          prod_rub = mean(dat_prod$prod_rub),
                          prod_cass = mean(dat_prod$prod_cass),
                          prod_sug = mean(dat_prod$prod_sug),
                          time = mean(dat_prod$time),
                          for_rem = mean(dat_prod$for_rem))
prod_corn.predict <- predict(prod.modAv.aicc6, newdata=prod_corn.newdata, se.fit=TRUE)
prod_corn.predict <- data.frame(prod_corn.predict)
prod_corn.predict$lwr <- prod_corn.predict$fit-2*prod_corn.predict$se.fit
prod_corn.predict$upr <- prod_corn.predict$fit+2*prod_corn.predict$se.fit
prod_corn.predict <- cbind(prod_corn.predict, prod_corn.newdata)


# plot
prod_corn_plot <- ggplot(data=prod_corn.predict, aes(x=prod_corn, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Corn producer price at time t")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_corn_plot.png", prod_corn_plot, width = 30, 
#       height = 20, units = "cm", dpi=300)



# prod_sug
prod_sug.newdata <- expand.grid(prod_sug = seq(min(dat_prod$prod_sug), max(dat_prod$prod_sug), 
                                                 length=100),
                          prod_rub = mean(dat_prod$prod_rub),
                          prod_cass = mean(dat_prod$prod_cass),
                          prod_corn = mean(dat_prod$prod_corn),
                          time = mean(dat_prod$time),
                          for_rem = mean(dat_prod$for_rem))
prod_sug.predict <- predict(prod.modAv.aicc6, newdata=prod_sug.newdata, se.fit=TRUE)
prod_sug.predict <- data.frame(prod_sug.predict)
prod_sug.predict$lwr <- prod_sug.predict$fit-2*prod_sug.predict$se.fit
prod_sug.predict$upr <- prod_sug.predict$fit+2*prod_sug.predict$se.fit
prod_sug.predict <- cbind(prod_sug.predict, prod_sug.newdata)


# plot
prod_sug_plot <- ggplot(data=prod_sug.predict, aes(x=prod_sug, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Sugar producer price at time t")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_sug_plot.png", prod_sug_plot, width = 30, 
#       height = 20, units = "cm", dpi=300)



    # 1 year lag ####

# create data
dat_prod_lag <- data.frame(for_cov = dat_prod$for_cov,
                         for_rem = dat_prod$for_rem,
                         time = dat_prod$time,
                         prod_rub.lag1 = lag(dat_prod$prod_rub),
                         prod_rub.lag2 = lag(dat_prod$prod_rub, n=2L),
                         prod_corn.lag1 = lag(dat_prod$prod_corn),
                         prod_corn.lag2 = lag(dat_prod$prod_corn, n=2L),
                         prod_cass.lag1 = lag(dat_prod$prod_cass),
                         prod_cass.lag2 = lag(dat_prod$prod_cass, n=2L),
                         prod_sug.lag1 = lag(dat_prod$prod_sug),
                         prod_sug.lag2 = lag(dat_prod$prod_sug, n=2L))

# remove top row for 1 year lag
dat_prod_lag1 <- dat_prod_lag[2:22, ]

# remove top two rows for 2 year lag
dat_prod_lag2 <- dat_prod_lag[3:22, ]



## saturated model with a gaussian distribution for 1-year lag predictors
prod.mod.gaus.lag1 <- glm(for_cov ~ prod_rub.lag1 + prod_cass.lag1 + prod_corn.lag1 + prod_sug.lag1 + 
                          time + for_rem, na.action="na.fail", family=gaussian, data=dat_prod_lag1)
summary(prod.mod.gaus.lag1)

# dredge
prod.dredge.gaus.lag1 <- dredge(prod.mod.gaus.lag1, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(prod.dredge.gaus.lag1, file="Results/Macroeconomics/Dredge/prod.dredge.gaus.lag1.csv")


## saturated model with gamma distribution for 1 year lagged predictors
#prod.mod.gam.lag1 <- glm(for_cov ~ prod_rub.lag1 + prod_cass.lag1 + prod_corn.lag1 + prod_sug.lag1 + 
 #                         time, na.action="na.fail", family=Gamma, data=dat_prod_lag1)
#summary(prod.mod.gam.lag1)

# dredge
#prod.dredge.gam.lag1 <- dredge(prod.mod.gam.lag1, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(prod.dredge.gam.lag1, file="Results/Macroeconomics/Dredge/prod.dredge.gam.lag1.csv")


# models with Gaussian distribution are the better models


## model averaging
# AICc < 6
prod.modAv.aicc6.lag1 <- model.avg(prod.dredge.gaus.lag1, subset = delta < 6, fit = TRUE)
summary(prod.modAv.aicc6.lag1)


# Predict with the AICc<6 set

# prod_rub.lag1
prod_rub.newdata.lag1 <- expand.grid(prod_rub.lag1 = seq(min(dat_prod_lag1$prod_rub.lag1), 
                                                         max(dat_prod_lag1$prod_rub.lag1), 
                                               length=100),
                          prod_cass.lag1 = mean(dat_prod_lag1$prod_cass.lag1),
                          prod_corn.lag1 = mean(dat_prod_lag1$prod_corn.lag1),
                          prod_sug.lag1 = mean(dat_prod_lag1$prod_sug.lag1),
                          time = mean(dat_prod_lag1$time),
                          for_rem = mean(dat_prod_lag1$for_rem))
prod_rub.predict.lag1 <- predict(prod.modAv.aicc6.lag1, newdata=prod_rub.newdata.lag1, se.fit=TRUE)
prod_rub.predict.lag1 <- data.frame(prod_rub.predict.lag1)
prod_rub.predict.lag1$lwr <- prod_rub.predict.lag1$fit-2*prod_rub.predict.lag1$se.fit
prod_rub.predict.lag1$upr <- prod_rub.predict.lag1$fit+2*prod_rub.predict.lag1$se.fit
prod_rub.predict.lag1 <- cbind(prod_rub.predict.lag1, prod_rub.newdata.lag1)

# plot
prod_rub_plot.lag1 <- ggplot(data=prod_rub.predict.lag1, aes(x=prod_rub.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Producer price of rubber at time t-1")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_rub_plot.lag1.png", prod_rub_plot.lag1, width = 30,
#       height = 20, units = "cm", dpi=300)



# prod_cass.lag1
prod_cass.newdata.lag1 <- expand.grid(prod_cass.lag1 = seq(min(dat_prod_lag1$prod_cass.lag1), 
                                                         max(dat_prod_lag1$prod_cass.lag1), 
                                               length=100),
                          prod_rub.lag1 = mean(dat_prod_lag1$prod_rub.lag1),
                          prod_corn.lag1 = mean(dat_prod_lag1$prod_corn.lag1),
                          prod_sug.lag1 = mean(dat_prod_lag1$prod_sug.lag1),
                          time = mean(dat_prod_lag1$time),
                          for_rem = mean(dat_prod_lag1$for_rem))
prod_cass.predict.lag1 <- predict(prod.modAv.aicc6.lag1, newdata=prod_cass.newdata.lag1, se.fit=TRUE)
prod_cass.predict.lag1 <- data.frame(prod_cass.predict.lag1)
prod_cass.predict.lag1$lwr <- prod_cass.predict.lag1$fit-2*prod_cass.predict.lag1$se.fit
prod_cass.predict.lag1$upr <- prod_cass.predict.lag1$fit+2*prod_cass.predict.lag1$se.fit
prod_cass.predict.lag1 <- cbind(prod_cass.predict.lag1, prod_cass.newdata.lag1)

# plot
prod_cass_plot.lag1 <- ggplot(data=prod_cass.predict.lag1, aes(x=prod_cass.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Producer price of cassava at time t-1")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_cass_plot.lag1.png", prod_cass_plot.lag1, width = 30,
#       height = 20, units = "cm", dpi=300)



# prod_corn.lag1
prod_corn.newdata.lag1 <- expand.grid(prod_corn.lag1 = seq(min(dat_prod_lag1$prod_corn.lag1), 
                                                         max(dat_prod_lag1$prod_corn.lag1), 
                                               length=100),
                          prod_rub.lag1 = mean(dat_prod_lag1$prod_rub.lag1),
                          prod_cass.lag1 = mean(dat_prod_lag1$prod_cass.lag1),
                          prod_sug.lag1 = mean(dat_prod_lag1$prod_sug.lag1),
                          time = mean(dat_prod_lag1$time),
                          for_rem = mean(dat_prod_lag1$for_rem))
prod_corn.predict.lag1 <- predict(prod.modAv.aicc6.lag1, newdata=prod_corn.newdata.lag1, se.fit=TRUE)
prod_corn.predict.lag1 <- data.frame(prod_corn.predict.lag1)
prod_corn.predict.lag1$lwr <- prod_corn.predict.lag1$fit-2*prod_corn.predict.lag1$se.fit
prod_corn.predict.lag1$upr <- prod_corn.predict.lag1$fit+2*prod_corn.predict.lag1$se.fit
prod_corn.predict.lag1 <- cbind(prod_corn.predict.lag1, prod_corn.newdata.lag1)

# plot
prod_corn_plot.lag1 <- ggplot(data=prod_corn.predict.lag1, aes(x=prod_corn.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Producer price of corn at time t-1")+
             # ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_corn_plot.lag1.png", prod_corn_plot.lag1, width = 30,
   #    height = 20, units = "cm", dpi=300)




# prod_sug.lag1
prod_sug.newdata.lag1 <- expand.grid(prod_sug.lag1 = seq(min(dat_prod_lag1$prod_sug.lag1), 
                                                         max(dat_prod_lag1$prod_sug.lag1), 
                                               length=100),
                          prod_rub.lag1 = mean(dat_prod_lag1$prod_rub.lag1),
                          prod_cass.lag1 = mean(dat_prod_lag1$prod_cass.lag1),
                          prod_corn.lag1 = mean(dat_prod_lag1$prod_corn.lag1),
                          time = mean(dat_prod_lag1$time),
                          for_rem = mean(dat_prod_lag1$for_rem))
prod_sug.predict.lag1 <- predict(prod.modAv.aicc6.lag1, newdata=prod_sug.newdata.lag1, se.fit=TRUE)
prod_sug.predict.lag1 <- data.frame(prod_sug.predict.lag1)
prod_sug.predict.lag1$lwr <- prod_sug.predict.lag1$fit-2*prod_sug.predict.lag1$se.fit
prod_sug.predict.lag1$upr <- prod_sug.predict.lag1$fit+2*prod_sug.predict.lag1$se.fit
prod_sug.predict.lag1 <- cbind(prod_sug.predict.lag1, prod_sug.newdata.lag1)

# plot
prod_sug_plot.lag1 <- ggplot(data=prod_sug.predict.lag1, aes(x=prod_sug.lag1, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Producer price of sugar at time t-1")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_sug_plot.lag1.png", prod_sug_plot.lag1, width = 30,
#       height = 20, units = "cm", dpi=300)



    # 2 year lag ####


## saturated model with a gaussian distribution for 2-year lag predictors
prod.mod.gaus.lag2 <- glm(for_cov ~ prod_rub.lag2 + prod_cass.lag2 + prod_corn.lag2 + prod_sug.lag2 + 
                          time + for_rem, na.action="na.fail", family=gaussian, data=dat_prod_lag2)
summary(prod.mod.gaus.lag2)

# dredge
prod.dredge.gaus.lag2 <- dredge(prod.mod.gaus.lag2, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(prod.dredge.gaus.lag2, file="Results/Macroeconomics/Dredge/prod.dredge.gaus.lag2.csv")


## saturated model with gamma distribution for 1 year lagged predictors
#prod.mod.gam.lag2 <- glm(for_cov ~ prod_rub.lag2 + prod_cass.lag2 + prod_corn.lag2 + prod_sug.lag2 + 
 #                         time, na.action="na.fail", family=Gamma, data=dat_prod_lag2)
#summary(prod.mod.gam.lag2)

# dredge
#prod.dredge.gam.lag2 <- dredge(prod.mod.gam.lag2, beta = "none", evaluate = TRUE, rank = AICc)
#write.csv(prod.dredge.gam.lag2, file="Results/Macroeconomics/Dredge/prod.dredge.gam.lag2.csv")


# models with Gaussian distribution are the better models


## model averaging
# AICc < 6
prod.modAv.aicc6.lag2 <- model.avg(prod.dredge.gaus.lag2, subset = delta < 6, fit = TRUE)
summary(prod.modAv.aicc6.lag2)


# Predict with the AICc<6 set

# prod_rub.lag2
prod_rub.newdata.lag2 <- expand.grid(prod_rub.lag2 = seq(min(dat_prod_lag2$prod_rub.lag2), 
                                                         max(dat_prod_lag2$prod_rub.lag2), 
                                               length=100),
                          prod_cass.lag2 = mean(dat_prod_lag2$prod_cass.lag2),
                          prod_corn.lag2 = mean(dat_prod_lag2$prod_corn.lag2),
                          prod_sug.lag2 = mean(dat_prod_lag2$prod_sug.lag2),
                          time = mean(dat_prod_lag2$time),
                          for_rem = mean(dat_prod_lag2$for_rem))
prod_rub.predict.lag2 <- predict(prod.modAv.aicc6.lag2, newdata=prod_rub.newdata.lag2, se.fit=TRUE)
prod_rub.predict.lag2 <- data.frame(prod_rub.predict.lag2)
prod_rub.predict.lag2$lwr <- prod_rub.predict.lag2$fit-2*prod_rub.predict.lag2$se.fit
prod_rub.predict.lag2$upr <- prod_rub.predict.lag2$fit+2*prod_rub.predict.lag2$se.fit
prod_rub.predict.lag2 <- cbind(prod_rub.predict.lag2, prod_rub.newdata.lag2)

# plot
prod_rub_plot.lag2 <- ggplot(data=prod_rub.predict.lag2, aes(x=prod_rub.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
             # xlab("Producer price of rubber at time t-2")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_rub_plot.lag2.png", prod_rub_plot.lag2, width = 30,
 #      height = 20, units = "cm", dpi=300)


# prod_cass.lag2
prod_cass.newdata.lag2 <- expand.grid(prod_cass.lag2 = seq(min(dat_prod_lag2$prod_cass.lag2), 
                                                         max(dat_prod_lag2$prod_cass.lag2), 
                                               length=100),
                          prod_rub.lag2 = mean(dat_prod_lag2$prod_rub.lag2),
                          prod_corn.lag2 = mean(dat_prod_lag2$prod_corn.lag2),
                          prod_sug.lag2 = mean(dat_prod_lag2$prod_sug.lag2),
                          time = mean(dat_prod_lag2$time),
                          for_rem = mean(dat_prod_lag2$for_rem))
prod_cass.predict.lag2 <- predict(prod.modAv.aicc6.lag2, newdata=prod_cass.newdata.lag2, se.fit=TRUE)
prod_cass.predict.lag2 <- data.frame(prod_cass.predict.lag2)
prod_cass.predict.lag2$lwr <- prod_cass.predict.lag2$fit-2*prod_cass.predict.lag2$se.fit
prod_cass.predict.lag2$upr <- prod_cass.predict.lag2$fit+2*prod_cass.predict.lag2$se.fit
prod_cass.predict.lag2 <- cbind(prod_cass.predict.lag2, prod_cass.newdata.lag2)

# plot
prod_cass_plot.lag2 <- ggplot(data=prod_cass.predict.lag2, aes(x=prod_cass.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Producer price of cassava at time t-2")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_cass_plot.lag2.png", prod_cass_plot.lag2, width = 30,
#       height = 20, units = "cm", dpi=300)


# prod_corn.lag2
prod_corn.newdata.lag2 <- expand.grid(prod_corn.lag2 = seq(min(dat_prod_lag2$prod_corn.lag2), 
                                                         max(dat_prod_lag2$prod_corn.lag2), 
                                               length=100),
                          prod_rub.lag2 = mean(dat_prod_lag2$prod_rub.lag2),
                          prod_cass.lag2 = mean(dat_prod_lag2$prod_cass.lag2),
                          prod_sug.lag2 = mean(dat_prod_lag2$prod_sug.lag2),
                          time = mean(dat_prod_lag2$time),
                          for_rem = mean(dat_prod_lag2$for_rem))
prod_corn.predict.lag2 <- predict(prod.modAv.aicc6.lag2, newdata=prod_corn.newdata.lag2, se.fit=TRUE)
prod_corn.predict.lag2 <- data.frame(prod_corn.predict.lag2)
prod_corn.predict.lag2$lwr <- prod_corn.predict.lag2$fit-2*prod_corn.predict.lag2$se.fit
prod_corn.predict.lag2$upr <- prod_corn.predict.lag2$fit+2*prod_corn.predict.lag2$se.fit
prod_corn.predict.lag2 <- cbind(prod_corn.predict.lag2, prod_corn.newdata.lag2)

# plot
prod_corn_plot.lag2 <- ggplot(data=prod_corn.predict.lag2, aes(x=prod_corn.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Producer price of corn at time t-2")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_corn_plot.lag2.png", prod_corn_plot.lag2, width = 30,
#       height = 20, units = "cm", dpi=300)



# prod_sug.lag2
prod_sug.newdata.lag2 <- expand.grid(prod_sug.lag2 = seq(min(dat_prod_lag2$prod_sug.lag2), 
                                                         max(dat_prod_lag2$prod_sug.lag2), 
                                               length=100),
                          prod_rub.lag2 = mean(dat_prod_lag2$prod_rub.lag2),
                          prod_cass.lag2 = mean(dat_prod_lag2$prod_cass.lag2),
                          prod_corn.lag2 = mean(dat_prod_lag2$prod_corn.lag2),
                          time = mean(dat_prod_lag2$time),
                          for_rem = mean(dat_prod_lag2$for_rem))
prod_sug.predict.lag2 <- predict(prod.modAv.aicc6.lag2, newdata=prod_sug.newdata.lag2, se.fit=TRUE)
prod_sug.predict.lag2 <- data.frame(prod_sug.predict.lag2)
prod_sug.predict.lag2$lwr <- prod_sug.predict.lag2$fit-2*prod_sug.predict.lag2$se.fit
prod_sug.predict.lag2$upr <- prod_sug.predict.lag2$fit+2*prod_sug.predict.lag2$se.fit
prod_sug.predict.lag2 <- cbind(prod_sug.predict.lag2, prod_sug.newdata.lag2)

# plot
prod_sug_plot.lag2 <- ggplot(data=prod_sug.predict.lag2, aes(x=prod_sug.lag2, y=fit))+
              geom_line(color="#339900", size=1)+
              geom_ribbon(aes(ymin=lwr, ymax=upr), alpha = 0.4, fill="#339900")+
              ylim(0,1500)+
              #xlab("Producer price of sugar at time t-2")+
              #ylab("Amount of forest lost (ha) at time t")+
              theme(text = element_text(size=15))+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),axis.line = element_line(colour = "black"))
#ggsave(file="Results/Macroeconomics/Plots/prod_sug_plot.lag2.png", prod_sug_plot.lag2, width = 30,
   #    height = 20, units = "cm", dpi=300)

    # all plots ####

# all plots together, with one row for each variable
prod_plot_all <- prod_rub_plot + prod_rub_plot.lag1 + prod_rub_plot.lag2 +
                 prod_cass_plot + prod_cass_plot.lag1 + prod_cass_plot.lag2 +
                 prod_corn_plot + prod_corn_plot.lag1 + prod_corn_plot.lag2 +
                 prod_sug_plot + prod_sug_plot.lag1 + prod_sug_plot.lag2 +
  plot_layout(ncol=3)

# remove y-axis labels for all plots except plots 1, 4, 7, 10, and change those to letters
prod_plot_all[[1]] <- prod_plot_all[[1]] + ylab("a") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
prod_plot_all[[2]] <- prod_plot_all[[2]] + theme(axis.title.y = element_blank())
prod_plot_all[[3]] <- prod_plot_all[[3]] + theme(axis.title.y = element_blank())
prod_plot_all[[4]] <- prod_plot_all[[4]] + ylab("b") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
prod_plot_all[[5]] <- prod_plot_all[[5]] + theme(axis.title.y = element_blank())
prod_plot_all[[6]] <- prod_plot_all[[6]] + theme(axis.title.y = element_blank())
prod_plot_all[[7]] <- prod_plot_all[[7]] + ylab("c") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
prod_plot_all[[8]] <- prod_plot_all[[8]] + theme(axis.title.y = element_blank())
prod_plot_all[[9]] <- prod_plot_all[[9]] + theme(axis.title.y = element_blank())
prod_plot_all[[10]] <- prod_plot_all[[10]] + ylab("d") + 
  theme(axis.title.y = element_text(margin=unit(c(0,7,0,0),"mm"), angle=0, vjust = 0.5,
                                    size=20))
prod_plot_all[[11]] <- prod_plot_all[[11]] + theme(axis.title.y = element_blank())
prod_plot_all[[12]] <- prod_plot_all[[12]] + theme(axis.title.y = element_blank())


# remove x axis labels for all plots
prod_plot_all[[1]] <- prod_plot_all[[1]] + theme(axis.title.x = element_blank())
prod_plot_all[[2]] <- prod_plot_all[[2]] + theme(axis.title.x = element_blank())
prod_plot_all[[3]] <- prod_plot_all[[3]] + theme(axis.title.x = element_blank())
prod_plot_all[[4]] <- prod_plot_all[[4]] + theme(axis.title.x = element_blank())
prod_plot_all[[5]] <- prod_plot_all[[5]] + theme(axis.title.x = element_blank())
prod_plot_all[[6]] <- prod_plot_all[[6]] + theme(axis.title.x = element_blank())
prod_plot_all[[7]] <- prod_plot_all[[7]] + theme(axis.title.x = element_blank())
prod_plot_all[[8]] <- prod_plot_all[[8]] + theme(axis.title.x = element_blank())
prod_plot_all[[9]] <- prod_plot_all[[9]] + theme(axis.title.x = element_blank())
prod_plot_all[[10]] <- prod_plot_all[[10]] + theme(axis.title.x = element_blank())
prod_plot_all[[11]] <- prod_plot_all[[11]] + theme(axis.title.x = element_blank())
prod_plot_all[[12]] <- prod_plot_all[[12]] + theme(axis.title.x = element_blank())


# remove y-axis ticks from all plots except 1, 4, 7, 10, 
prod_plot_all[[2]] <- prod_plot_all[[2]] + theme(axis.text.y = element_blank())
prod_plot_all[[3]] <- prod_plot_all[[3]] + theme(axis.text.y = element_blank())
prod_plot_all[[5]] <- prod_plot_all[[5]] + theme(axis.text.y = element_blank())
prod_plot_all[[6]] <- prod_plot_all[[6]] + theme(axis.text.y = element_blank())
prod_plot_all[[8]] <- prod_plot_all[[8]] + theme(axis.text.y = element_blank())
prod_plot_all[[9]] <- prod_plot_all[[9]] + theme(axis.text.y = element_blank())
prod_plot_all[[11]] <- prod_plot_all[[11]] + theme(axis.text.y = element_blank())
prod_plot_all[[12]] <- prod_plot_all[[12]] + theme(axis.text.y = element_blank())


# add titles for plots 1, 2, 3 to show the lags
prod_plot_all[[1]] <- prod_plot_all[[1]] + ggtitle ("Time t") + theme(plot.title = element_text(hjust=0.5))
prod_plot_all[[2]] <- prod_plot_all[[2]] + ggtitle ("Time t+1")+ theme(plot.title = element_text(hjust=0.5))
prod_plot_all[[3]] <- prod_plot_all[[3]] + ggtitle ("Time t+2")+ theme(plot.title = element_text(hjust=0.5))

ggsave("Results/Macroeconomics/Plots/PROD_plot_grid.png", prod_plot_all, 
       width = 30, height = 30, unit="cm", dpi=300)

#### ELCs as response ####

# load data
elc <- read.csv("elc_years.csv", header = T)
dat <- read.csv("dat_work.csv", header = T)

head(elc)
head(dat)

  ## macroecon set ####
    # no lag ####

years <- c(1995:2015)

# subset dat
dat <- dat %>% filter(year %in% years)

# subset elc
elc <- elc %>% filter(year %in% years) %>% select(count)

# attach elc to dat
dat$elc <- elc$count

str(dat)


# gdp, gdp_gr, fdi, ind_gdp, agr_gdp, dev_agri, dev_env, pop_den

# model no time lag (exclude gdp_gr as missing data)
m.econ <- glm(elc ~  gdp+fdi+agr_gdp+dev_agri+dev_env+pop_den+time+for_rem,
            na.action = "na.fail", family = poisson, data=dat)


m.econ.d <- dredge(m.econ, beta = "none", evaluate = TRUE, rank = AICc)

# The only models that are dAIC < 6 from the top model are models that have 1 extra parameter compared with the top model (i.e. slightly ore complex versions). Therefore the extra parameters are likely redundant. Therefore I will just select the top model for inference.

m.econ.top <- glm(elc ~ agr_gdp + dev_env + for_rem + pop_den + time, family=poisson, data=dat)
summary(m.econ.top)
plot(m.econ.top)

# plot predicted (fully conditional) versus observed
m.econ.d <- dat
m.econ.d$pred <- predict(m.econ.top, type="response")
plot(m.econ.d$elc, m.econ.d$pred)

# plot residuals versus predicted
m.econ.d$resid <- residuals(m.econ.top)
plot(m.econ.d$pred, m.econ.d$resid)

# histogram of residuals
hist(resid(m.econ.top))

  ### predict

## new data
m.econ.agrgdp <- data.frame(agr_gdp = seq(min(dat$agr_gdp), max(dat$agr_gdp), length.out=100),
                            dev_env = mean(dat$dev_env),
                            for_rem = mean(dat$for_rem),
                            pop_den = mean(dat$pop_den),
                            time = mean(dat$time))

m.econ.devenv <- data.frame(dev_env = seq(min(dat$dev_env), max(dat$dev_env), length.out=100),
                            agr_gdp = mean(dat$agr_gdp),
                            for_rem = mean(dat$for_rem),
                            pop_den = mean(dat$pop_den),
                            time = mean(dat$time))

m.econ.popden <- data.frame(pop_den = seq(min(dat$pop_den), max(dat$pop_den), length.out=100),
                            agr_gdp = mean(dat$agr_gdp),
                            for_rem = mean(dat$for_rem),
                            dev_env = mean(dat$dev_env),
                            time = mean(dat$time))

# predict
m.econ.agrgdp$pred <- predict(m.econ.top, newdata = m.econ.agrgdp, type="response")
m.econ.devenv$pred <- predict(m.econ.top, newdata = m.econ.devenv, type="response")
m.econ.popden$pred <- predict(m.econ.top, newdata = m.econ.popden, type="response")

# plots
agrgdp.plot <- ggplot()+
                geom_line(data=m.econ.agrgdp, aes(x=agr_gdp, y=pred))+
                geom_point(data=dat, aes(x=agr_gdp, y=elc))+
                xlab("Agricultural proportion of GDP")+
                ylab("Number of ELC allocations")+
                ggtitle("No time lag")+
                theme_classic()

devenv.plot <- ggplot()+
                geom_line(data=m.econ.devenv, aes(x=dev_env, y=pred))+
                geom_point(data=dat, aes(x=dev_env, y=elc))+
                xlab("Development flows to environment sector")+
                ylab("")+
                ggtitle("No time lag")+
                theme_classic()

popden.plot <- ggplot()+
                geom_line(data=m.econ.popden, aes(x=pop_den, y=pred))+
                geom_point(data=dat, aes(x=pop_den, y=elc))+
                xlab("Population density")+
                ylab("")+
                ggtitle("No time lag")+
                theme_classic()

agrgdp.plot + devenv.plot + popden.plot + plot_layout(ncol=2)


    # 1 year lag ####

# load data
elc <- read.csv("elc_years.csv", header = T)
dat <- read.csv("dat_work.csv", header = T)

# 1994 data missing from main data so subset to start at 1995
years <- c(1995:2015)
dat <- dat %>% filter(year %in% years)

# subset elc data to cut it off at 2016. So now the elc data is one year ahead (i.e. data from 1995 is aligned with elc data from 1996)
elcyears <- c(1996:2016)
elc <- elc %>% filter(year %in% elcyears) %>% select(count)

# add elc data to main dat
dat$elc <- elc$count

# model with 1 year time lag
m.econ.lag1 <- glm(elc ~  gdp+fdi+agr_gdp+dev_agri+dev_env+pop_den+time+for_rem,
                na.action = "na.fail", family = poisson, data=dat)


m.econ.lag1.d <- dredge(m.econ.lag1, beta = "none", evaluate = TRUE, rank = AICc)

# I will use just the top model as the other models within 6 dAIC are just adding one more variable, so they are reduntant. However, I will add for_rem as a control. So actually I am using the second model from the top which includes for_rem

m.econ.lag1.top <- glm(elc ~ agr_gdp + fdi + gdp + pop_den + time + for_rem, family=poisson, data=dat)
summary(m.econ.lag1.top)

# plot predicted (fully conditional) versus observed
m.econ.lag.d <- dat
m.econ.lag.d$pred <- predict(m.econ.lag1.top, type="response")
plot(m.econ.lag.d$elc, m.econ.lag.d$pred)

# plot residuals versus predicted
m.econ.lag.d$resid <- residuals(m.econ.lag1.top)
plot(m.econ.lag.d$pred, m.econ.lag.d$resid)

### predict

## new data
m.econ.lag1.agrgdp <- data.frame(agr_gdp = seq(min(dat$agr_gdp), max(dat$agr_gdp), length.out=100),
                                 fdi = mean(dat$fdi),
                                 gdp = mean(dat$gdp),
                                 pop_den = mean(dat$pop_den),
                                 time = mean(dat$time),
                                 for_rem = mean(dat$for_rem))

m.econ.lag1.fdi <- data.frame(fdi = seq(min(dat$fdi), max(dat$fdi), length.out=100),
                              agr_gdp = mean(dat$agr_gdp),
                              gdp = mean(dat$gdp),
                              pop_den = mean(dat$pop_den),
                              time = mean(dat$time),
                              for_rem = mean(dat$for_rem))

m.econ.lag1.gdp <- data.frame(gdp = seq(min(dat$gdp), max(dat$gdp), length.out=100),
                              agr_gdp = mean(dat$agr_gdp),
                              fdi = mean(dat$fdi),
                              pop_den = mean(dat$pop_den),
                              time = mean(dat$time),
                              for_rem = mean(dat$for_rem))

m.econ.lag1.pop_den <- data.frame(pop_den = seq(min(dat$pop_den), max(dat$pop_den), length.out=100),
                                  agr_gdp = mean(dat$agr_gdp),
                                  fdi = mean(dat$fdi),
                                  gdp = mean(dat$gdp),
                                  time = mean(dat$time),
                                  for_rem = mean(dat$for_rem))

# predict
agrgdp.pred <- predict(m.econ.lag1.top, newdata=m.econ.lag1.agrgdp, type="response", se.fit = T)
m.econ.lag1.agrgdp$pred <- agrgdp.pred$fit
m.econ.lag1.agrgdp$pred.se <- agrgdp.pred$se.fit

fdi.pred <- predict(m.econ.lag1.top, newdata=m.econ.lag1.fdi, type="response", se.fit = T)
m.econ.lag1.fdi$pred <- fdi.pred$fit
m.econ.lag1.fdi$pred.se <- fdi.pred$se.fit

gdp.pred <- predict(m.econ.lag1.top, newdata=m.econ.lag1.gdp, type="response", se.fit = T)
m.econ.lag1.gdp$pred <- gdp.pred$fit
m.econ.lag1.gdp$pred.se <- gdp.pred$fit

popden.pred <- predict(m.econ.lag1.top, newdata=m.econ.lag1.pop_den, type="response", se.fit = T)
m.econ.lag1.pop_den$pred <- popden.pred$fit
m.econ.lag1.pop_den$pred.se <- popden.pred$se.fit

# plots

agrgdp.L1.plot <- ggplot()+
                    geom_line(data=m.econ.lag1.agrgdp, aes(x=agr_gdp, y=pred))+
                    geom_ribbon(data=m.econ.lag1.agrgdp, aes(x= agr_gdp,ymin=pred.se,ymax=pred.se),alpha=0.3)+
                    geom_point(data=dat, aes(x=agr_gdp, y=elc))+
                    ylab("Number of ELC allocations")+
                    xlab("Agricultural proportion of GDP")+
                    ggtitle("1 year lag")+
                    theme_classic()

fdi.L1.plot <- ggplot()+
                geom_line(data=m.econ.lag1.fdi, aes(x=fdi, y=pred))+
                geom_ribbon(data=m.econ.lag1.fdi, aes(x= fdi,ymin=pred.se,ymax=pred.se),alpha=0.3)+
                geom_point(data=dat, aes(x=fdi, y=elc))+
                ylab("")+
                xlab("Foreign direct investment (USD Millions)")+
                ggtitle("1 year lag")+
                theme_classic()

gdp.L1.plot <- ggplot()+
                geom_line(data=m.econ.lag1.gdp, aes(x=gdp, y=pred))+
                geom_ribbon(data=m.econ.lag1.gdp, aes(x= gdp,ymin=pred.se,ymax=pred.se),alpha=0.3)+
                geom_point(data=dat, aes(x=gdp, y=elc))+
                ylab("")+
                xlab("Per capita GDP")+
                ggtitle("1 year lag")+
                theme_classic()

popden.L1.plot <- ggplot()+
                geom_line(data=m.econ.lag1.pop_den, aes(x=pop_den, y=pred))+
                geom_ribbon(data=m.econ.lag1.pop_den, aes(x= pop_den,ymin=pred.se,ymax=pred.se),alpha=0.3)+
                geom_point(data=dat, aes(x=pop_den, y=elc))+
                ylab("")+
                xlab("Population density")+
                ggtitle("1 year lag")+
                theme_classic()

(agrgdp.plot + devenv.plot) / (agrgdp.L1.plot + fdi.L1.plot + gdp.L1.plot )


    # 2 year lag ####

# load data
elc <- read.csv("elc_years.csv", header = T)
dat <- read.csv("dat_work.csv", header = T)

# 1994 data missing from main data so subset to start at 1995
years <- c(1995:2015)
dat <- dat %>% filter(year %in% years)

# subset elc data to cut it off at 2017. So now the elc data is two years ahead (i.e. data from 1995 is aligned with elc data from 1997)
elcyears <- c(1997:2017)
elc <- elc %>% filter(year %in% elcyears) %>% select(count)

# add elc data to main dat
dat$elc <- elc$count

# model with 2 year time lag
m.econ.lag2 <- glm(elc ~  gdp+fdi+agr_gdp+dev_agri+dev_env+pop_den+time+for_rem,
                   na.action = "na.fail", family = poisson, data=dat)


m.econ.lag2.d <- dredge(m.econ.lag2, beta = "none", evaluate = TRUE, rank = AICc)
# there are a lot of models now within dAIC of 6. So I will model average

m.econ.modAv.aicc6 <- model.avg(m.econ.lag2.d, subset = delta < 6, fit = TRUE)
summary(m.econ.modAv.aicc6)
# only really gdp and pop_den, and pop_den is more of a control and doesn't really make sense to plot I don't think. I can't rally get my head around why changes in population density would affect ELC allocation

### predict

# new data
m.econ.lag2.gdp <- data.frame(gdp = seq(min(dat$gdp), max(dat$gdp), length.out=100),
                              pop_den = mean(dat$pop_den),
                              agr_gdp = mean(dat$agr_gdp),
                              dev_env = mean(dat$dev_env),
                              fdi = mean(dat$fdi),
                              dev_agri = mean(dat$dev_agri),
                              for_rem = mean(dat$for_rem),
                              time = mean(dat$time))

# predict
m.econ.lag2.gdp$pred <- predict(m.econ.modAv.aicc6, newdata = m.econ.lag2.gdp, type="response")

# plot 
gdp.L2.plot <- ggplot()+
                geom_line(data=m.econ.lag2.gdp, aes(x=gdp, y=pred))+
                geom_point(data=dat, aes(x=gdp, y=elc))+
                ylab("Number of ELC allocations")+
                xlab("Per capita GDP")+
                ggtitle("2 year lag")+
                theme_classic()

(agrgdp.plot | devenv.plot) / (agrgdp.L1.plot | fdi.L1.plot | gdp.L1.plot) / (gdp.L2.plot | grid::textGrob(""))


  ## Commodity / production set ####
    # no lag ####

# load data
elc <- read.csv("elc_years.csv", header = T)
dat <- read.csv("dat_work.csv", header = T)

years <- c(1995:2015)

# subset dat
dat <- dat %>% filter(year %in% years)

# subset elc
elc <- elc %>% filter(year %in% years) %>% select(count)

# attach elc to dat
dat$elc <- elc$count

str(dat)


# cpi + nfi + rice_med + rub_med + corn_med + sug_med + for_prod + time + for_rem

# model no time lag 
m.comm <- glm(elc ~  cpi + nfi + rice_med + rub_med + corn_med + sug_med + for_prod + time + for_rem,
              na.action = "na.fail", family = poisson, data=dat)


m.comm.d <- dredge(m.comm, beta = "none", evaluate = TRUE, rank = AICc)

# Only 3 models within dAIC of 6. The second model from the top only differs from the top in the lack of time, which has to be included and so the second model is not useable. The 3rd model differs from the top only by one additional variable. Therefore this additional variable is likely to be redundant. I will use the top model for inference

m.comm.top <- glm(elc ~ corn_med + nfi + rice_med + rub_med + sug_med + for_rem + time,
                  family=poisson, data=dat)
summary(m.comm.top)
plot(m.comm.top)


# plot predicted (fully conditional) versus observed
m.comm.d <- dat
m.comm.d$pred <- predict(m.comm.top, type="response")
plot(m.comm.d$elc, m.comm.d$pred)

# plot residuals versus predicted
m.comm.d$resid <- residuals(m.comm.top)
plot(m.comm.d$pred, m.comm.d$resid)

# histogram of residuals
hist(resid(m.comm.top))

### predict

## new data
m.comm.corn <- data.frame(corn_med = seq(min(dat$corn_med), max(dat$corn_med), length.out=100),
                            rice_med = mean(dat$rice_med),
                            rub_med = mean(dat$rub_med),
                            sug_med = mean(dat$sug_med),
                            nfi = mean(dat$nfi),
                            for_rem = mean(dat$for_rem),
                            time = mean(dat$time))

m.comm.rice <- data.frame(rice_med = seq(min(dat$rice_med), max(dat$rice_med), length.out=100),
                          corn_med = mean(dat$corn_med),
                          rub_med = mean(dat$rub_med),
                          sug_med = mean(dat$sug_med),
                          nfi = mean(dat$nfi),
                          for_rem = mean(dat$for_rem),
                          time = mean(dat$time))

m.comm.rub <- data.frame(rub_med = seq(min(dat$rub_med), max(dat$rub_med), length.out=100),
                          corn_med = mean(dat$corn_med),
                          rice_med = mean(dat$rice_med),
                          sug_med = mean(dat$sug_med),
                          nfi = mean(dat$nfi),
                          for_rem = mean(dat$for_rem),
                          time = mean(dat$time))

m.comm.sug <- data.frame(sug_med = seq(min(dat$sug_med), max(dat$sug_med), length.out=100),
                         corn_med = mean(dat$corn_med),
                         rice_med = mean(dat$rice_med),
                         rub_med = mean(dat$rub_med),
                         nfi = mean(dat$nfi),
                         for_rem = mean(dat$for_rem),
                         time = mean(dat$time))

m.comm.nfi <- data.frame(nfi = seq(min(dat$nfi), max(dat$nfi), length.out=100),
                         corn_med = mean(dat$corn_med),
                         rice_med = mean(dat$rice_med),
                         rub_med = mean(dat$rub_med),
                         sug_med = mean(dat$sug_med),
                         for_rem = mean(dat$for_rem),
                         time = mean(dat$time))


# predict with SE's (in link units)
corn_pred <- as.data.frame(predict(m.comm.top, newdata = m.comm.corn, type="link", se=TRUE))
rice_pred <- as.data.frame(predict(m.comm.top, newdata = m.comm.rice, type="link", se=TRUE))
rub_pred  <- as.data.frame(predict(m.comm.top, newdata = m.comm.rub, type="link", se=TRUE))
sug_pred  <- as.data.frame(predict(m.comm.top, newdata = m.comm.sug, type="link", se=TRUE))
nfi_pred  <- as.data.frame(predict(m.comm.top, newdata = m.comm.nfi, type="link", se=TRUE))

# exponentiate the fit onto the dataframes
m.comm.corn$pred <- exp(corn_pred$fit)
m.comm.rice$pred <- exp(rice_pred$fit)
m.comm.rub$pred <- exp(rub_pred$fit)
m.comm.sug$pred <- exp(sug_pred$fit)
m.comm.nfi$pred <- exp(nfi_pred$fit)

# add confidence intervals onto dataframes
m.comm.corn$lcl <- exp(corn_pred$fit-1.96*corn_pred$se.fit)
m.comm.corn$ucl <- exp(corn_pred$fit+1.96*corn_pred$se.fit)
m.comm.rice$lcl <- exp(rice_pred$fit-1.96*rice_pred$se.fit)
m.comm.rice$ucl <- exp(rice_pred$fit+1.96*rice_pred$se.fit)
m.comm.rub$lcl  <- exp(rub_pred$fit-1.96*rub_pred$se.fit)
m.comm.rub$ucl  <- exp(rub_pred$fit+1.96*rub_pred$se.fit)
m.comm.sug$lcl  <- exp(sug_pred$fit-1.96*sug_pred$se.fit)
m.comm.sug$ucl  <- exp(sug_pred$fit+1.96*sug_pred$se.fit)
m.comm.nfi$lcl  <- exp(nfi_pred$fit-1.96*nfi_pred$se.fit)
m.comm.nfi$ucl  <- exp(nfi_pred$fit+1.96*nfi_pred$se.fit)



# plots
corn.plot <- ggplot()+
             geom_line(data=m.comm.corn, aes(x=corn_med, y=pred))+
             geom_point(data=dat, aes(x=corn_med, y=elc))+
             geom_ribbon(data=m.comm.corn, aes(ymin=lcl, ymax=ucl, x=corn_med), alpha=0.3)+
             xlab("Median price of corn (USD/ton)")+
             ylab("Number of ELC allocations")+
             ggtitle("No time lag")+
             theme_classic()

devenv.plot <- ggplot()+
  geom_line(data=m.econ.devenv, aes(x=dev_env, y=pred))+
  geom_point(data=dat, aes(x=dev_env, y=elc))+
  xlab("Development flows to environment sector")+
  ylab("")+
  ggtitle("No time lag")+
  theme_classic()

popden.plot <- ggplot()+
  geom_line(data=m.econ.popden, aes(x=pop_den, y=pred))+
  geom_point(data=dat, aes(x=pop_den, y=elc))+
  xlab("Population density")+
  ylab("")+
  ggtitle("No time lag")+
  theme_classic()

agrgdp.plot + devenv.plot + popden.plot + plot_layout(ncol=2)
