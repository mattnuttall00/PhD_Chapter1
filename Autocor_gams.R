#===== Load data ================================================================================
library('nlme')
library('ggplot2')
library('mgcv')
library('voxel')
library('cowplot')
library('lme4')
library('gamm4')
library('itsadug')
setwd("C://Users/mnn1/Box Sync/Objective 1/Analysis/Principal component regression/text files")
setwd("C://Users/Matt&Kez/Box Sync/Objective 1/Analysis/Principal component regression/text files")

dat_master <- read.table("ForCov_LU_econVars_PCs.txt", header = T)
attach(dat_master)

# dataframe for forest cover rate of change
dat_for_cov_roc <- data.frame(for_cov_roc_red = for_cov_roc[2:23],
                              econ_PC1_red = econ_PC1[2:23],
                              econ_PC2_red = econ_PC2[2:23],
                              econ_PC3_red = econ_PC3[2:23])
attach(dat_for_cov_roc)

#===== Detrending & autocorrelation =============================================
 
## Method 1 (Crawley) ####

### Response variables

# Forest cover
dt_lm_forCov <- lm(for_cov_area~I(1:length(for_cov_area)))
detrend_forCov <- for_cov_area - predict(lm(for_cov_area~I(1:length(for_cov_area))))
summary(dt_lm_forCov)

# Forest cover rate of change
dt_lm_forCov_roc <- lm(for_cov_roc~I(1:length(for_cov_roc)))
detrend_forCov_roc <- for_cov_roc[2:23] - predict(lm(for_cov_roc~I(1:length(for_cov_roc))))
summary(dt_lm_forCov_roc)

# Urban cover
dt_lm_urban <- lm(urban_area~I(1:length(urban_area)))
detrend_urban <- urban_area - predict(lm(urban_area~I(1:length(urban_area))))
summary(dt_lm_urban)

# Urban cover rate of change
dt_lm_urban_roc <- lm(urban_roc~I(1:length(urban_roc)))
detrend_urban_roc <- urban_roc[2:23] - predict(lm(urban_roc~I(1:length(urban_roc))))
summary(dt_lm_urban_roc)

# Agriculture cover
dt_lm_agric <- lm(agric_area~I(1:length(agric_area)))
detrend_agric <- agric_area - predict(lm(agric_area~I(1:length(agric_area))))
summary(dt_lm_agric)

# Agriculture rate of change
dt_lm_agric_roc <- lm(agric_roc~I(1:length(agric_roc)))
detrend_agric_roc <- agric_roc[2:23] - predict(lm(agric_roc~I(1:length(agric_roc))))
summary(dt_lm_agric_roc)

  ## Plotting (method 1) ####

## Before detrend

# predictor variables
par(mfrow=c(3,3))
plot(econ_PC1)
plot(econ_PC2)
plot(econ_PC3)
plot(com_PC1)
plot(com_PC2)
plot(com_PC3)
plot(prod_PC1)
plot(prod_PC2)
plot(prod_PC3)

# autocorrelation plots
acf(econ_PC1)
acf(econ_PC2)
acf(econ_PC3)
acf(com_PC1)
acf(com_PC2)
acf(com_PC3)
acf(prod_PC1)
acf(prod_PC2)
acf(prod_PC3)

# response variables
par(mfrow=c(3,2))
plot(for_cov_area)
plot(for_cov_roc)
plot(urban_area)
plot(urban_roc)
plot(agric_area)
plot(agric_roc)

# autocorrelation plots
acf(for_cov_area)
acf(for_cov_roc[2:23])
acf(urban_area)
acf(urban_roc[2:23])
acf(agric_area)
acf(agric_roc[2:23])

# partial autocorrelation plots
acf(for_cov_area, type = "p")
acf(for_cov_roc[2:23], type = "p")
acf(urban_area, type = "p")
acf(urban_roc[2:23], type = "p")
acf(agric_area, type = "p")
acf(agric_roc[2:23], type = "p")

## After detrend

# time series plots
par(mfrow=c(3,2))
ts.plot(detrend_forCov)
ts.plot(detrend_forCov_roc)
ts.plot(detrend_urban)
ts.plot(detrend_urban_roc)
ts.plot(detrend_agric)
ts.plot(detrend_agric_roc)

# autocorrelation plots
acf(detrend_forCov)
acf(detrend_forCov_roc)
acf(detrend_urban)
acf(detrend_urban_roc)
acf(detrend_agric)
acf(detrend_agric_roc)

# Plots of detrended response variables against predictor variables

# forest cover area
plot(econ_PC1, detrend_forCov)
plot(econ_PC2, detrend_forCov)
plot(econ_PC3, detrend_forCov)
plot(com_PC1, detrend_forCov)
plot(com_PC2, detrend_forCov)
plot(com_PC3, detrend_forCov)
plot(prod_PC1, detrend_forCov)
plot(prod_PC2, detrend_forCov)
plot(prod_PC3, detrend_forCov)

# forest cover rate of change
plot(econ_PC1[2:23], detrend_forCov_roc)
plot(econ_PC2[2:23], detrend_forCov_roc)
plot(econ_PC3[2:23], detrend_forCov_roc)
plot(com_PC1[2:23], detrend_forCov_roc)
plot(com_PC2[2:23], detrend_forCov_roc)
plot(com_PC3[2:23], detrend_forCov_roc)
plot(prod_PC1[2:23], detrend_forCov_roc)
plot(prod_PC2[2:23], detrend_forCov_roc)
plot(prod_PC3[2:23], detrend_forCov_roc)

# urban area
plot(econ_PC1, detrend_urban)
plot(econ_PC2, detrend_urban)
plot(econ_PC3, detrend_urban)
plot(com_PC1, detrend_urban)
plot(com_PC2, detrend_urban)
plot(com_PC3, detrend_urban)
plot(prod_PC1, detrend_urban)
plot(prod_PC2, detrend_urban)
plot(prod_PC3, detrend_urban)

# urban rate of change
plot(econ_PC1[2:23], detrend_urban_roc)
plot(econ_PC2[2:23], detrend_urban_roc)
plot(econ_PC3[2:23], detrend_urban_roc)
plot(com_PC1[2:23], detrend_urban_roc)
plot(com_PC2[2:23], detrend_urban_roc)
plot(com_PC3[2:23], detrend_urban_roc)
plot(prod_PC1[2:23], detrend_urban_roc)
plot(prod_PC2[2:23], detrend_urban_roc)
plot(prod_PC3[2:23], detrend_urban_roc)

# agriculture area
plot(econ_PC1, detrend_agric)
plot(econ_PC2, detrend_agric)
plot(econ_PC3, detrend_agric)
plot(com_PC1, detrend_agric)
plot(com_PC2, detrend_agric)
plot(com_PC3, detrend_agric)
plot(prod_PC1, detrend_agric)
plot(prod_PC2, detrend_agric)
plot(prod_PC3, detrend_agric)

# agriculture rate of change
plot(econ_PC1[2:23], detrend_agric_roc)
plot(econ_PC2[2:23], detrend_agric_roc)
plot(econ_PC3[2:23], detrend_agric_roc)
plot(com_PC1[2:23], detrend_agric_roc)
plot(com_PC2[2:23], detrend_agric_roc)
plot(com_PC3[2:23], detrend_agric_roc)
plot(prod_PC1[2:23], detrend_agric_roc)
plot(prod_PC2[2:23], detrend_agric_roc)
plot(prod_PC3[2:23], detrend_agric_roc)


## Method 2 - "year" covariate ####

# scale year variable ((x - mean(x))/sd(x))
yearScale <- scale(year)
yearnum <- c(1:23)

## Method 3 - Add autocorrelation covariance structure ####

## Null models
null.mod.ML  <- gls(for_cov_area ~ econ_PC1, method = "ML") 
summary(null.mod.ML)
acf(null.mod.ML)

null.mod.REML <- gls(for_cov_area ~ econ_PC1, method="REML")
plot(residuals(null.mod.REML, type = "normalized") ~ year)
summary(null.mod.REML)
acf(residuals(null.mod.REML, type = "normalized"))
acf(residuals(null.mod.REML, type = "normalized"), type = "p")

null.mod.year <- gls(for_cov_area ~ econ_PC1 + year, method = "REML")
summary(null.mod.year)
plot(residuals(null.mod.year, type = "normalized") ~ year)
acf(residuals(null.mod.year, type = "normalized"))
acf(residuals(null.mod.year, type = "normalized"), type = "p")

## Compound symmetry structure (Zuuer et al 2009, p.148)

# no "year"
comSym.test1 <- gls(for_cov_area ~ econ_PC1, correlation = corCompSymm())
summary(comSym.test1)
plot(residuals(comSym.test1, type = "normalized") ~ year)
acf(residuals(comSym.test1, type = "normalized"))
acf(residuals(comSym.test1, type = "normalized"), type = "p")

# with "year"
comSym.test2 <- gls(for_cov_area ~ econ_PC1 + year, correlation = corCompSymm())
summary(comSym.test2)
plot(residuals(comSym.test2, type = "normalized") ~ year)
acf(residuals(comSym.test2, type = "normalized"))
acf(residuals(comSym.test2, type = "normalized"), type = "p")

## 1st order autoregressive covariance structure

# Using reduced maximum liklihood (method = "REML"). Use this method when comparing models with different covariance structures

ar1.test1 <- gls(for_cov_area ~ econ_PC1, method="REML", correlation = corAR1())
ar1.test2 <- gls(for_cov_area ~ econ_PC1, method="REML", correlation = corAR1(form = ~year))
ar1.test3 <- gls(for_cov_area ~ econ_PC1+year, method="REML", correlation = corAR1(form = ~year))
summary(ar1.test1)
summary(ar1.test2)
summary(ar1.test3)
plot(residuals(ar1.test1, type = "normalized") ~ year)
plot(residuals(ar1.test2, type = "normalized") ~ year)
plot(residuals(ar1.test3, type = "normalized") ~ year)
acf(residuals(ar1.test1, type = "normalized"), type = "p")
acf(residuals(ar1.test2, type = "normalized"))
acf(residuals(ar1.test3, type = "normalized"))

## Auto-regressive moving-average error structures (corARMA())

# set values and starting values.  p = autoregressive parameter, q = number of moving average parameters

# ARMA(1,0)
cs1 <- corARMA(c(0.2), p = 1, q = 0, form = ~year)
# ARMA(0,1)
cs2 <- corARMA(c(0.2), p = 0, q = 1, form = ~year)
# ARMA(1,1)
cs3 <- corARMA(c(0.2, 0.2), p = 1, q = 1, form = ~year) 
# ARMA(2,0)
cs4 <- corARMA(c(0.2,0.2), p = 2, q = 0, form = ~year)

# models with no "year"
arma.1 <- gls(for_cov_area ~ econ_PC1+year, correlation = cs1)
arma.2 <- gls(for_cov_area ~ econ_PC1+year, correlation = cs2)
arma.3 <- gls(for_cov_area ~ econ_PC1+year, correlation = cs3)
arma.4 <- gls(for_cov_area ~ econ_PC1+year, correlation = cs4)

summary(arma.1)
summary(arma.2)
summary(arma.3)
summary(arma.4)
acf(residuals(arma.1, type = "normalized"))
acf(residuals(arma.1, type = "normalized"), type = "p")
acf(residuals(arma.2, type = "normalized"))
acf(residuals(arma.2, type = "normalized"), type = "p")
acf(residuals(arma.3, type = "normalized"))
acf(residuals(arma.3, type = "normalized"), type = "p")
acf(residuals(arma.4, type = "normalized"))
acf(residuals(arma.4, type = "normalized"), type = "p")


#===== Linear and non-linear modelling with detrended data==============

#### Forest cover (area) ~ Econ_PC1 ####

## Econ_PC1

# Plots
plot(econ_PC1, for_cov_area)
acf(econ_PC1) # significant autocorrelation at lags 1:4
acf(econ_PC1, type = "p") # signficant partial AC at lag 1
plot(econ_PC1, detrend_forCov)

  
  ## Testing autocorrelation structures ####

# Null model
m0.epc1.fca <- gls(for_cov_area ~ econ_PC1)
summary(m0.epc1.fca)
plot(residuals(m0.epc1.fca, type = "normalized") ~ year)
acf(residuals(m0.epc1.fca, type = "normalized"))
acf(residuals(m0.epc1.fca, type = "normalized"), type = "p")

# Null model with year
m1.epc1.fca <- gls(for_cov_area ~ econ_PC1 + year)
summary(m1.epc1.fca)
plot(residuals(m1.epc1.fca, type = "normalized") ~ year)
acf(residuals(m1.epc1.fca, type = "normalized"))
acf(residuals(m1.epc1.fca, type = "normalized"), type = "p")

## Compound symmetry structure
m2.epc1.fca <- gls(for_cov_area ~ econ_PC1 + year, correlation = corCompSymm(form = ~year))
summary(m2.epc1.fca)
plot(residuals(m2.epc1.fca, type = "normalized") ~ year)
acf(residuals(m2.epc1.fca, type = "normalized"))
acf(residuals(m2.epc1.fca, type = "normalized"), type = "p")

## 1st order autoregressive covariance structure

# Using reduced maximum liklihood (method = "REML"). Use this method when comparing models with different covariance structures

m3.epc1.fca <- gls(for_cov_area ~ econ_PC1+year, method="REML", correlation = corAR1(form = ~year))
summary(m3.epc1.fca)
plot(residuals(m3.epc1.fca, type = "normalized") ~ year)
acf(residuals(m3.epc1.fca, type = "normalized"))
acf(residuals(m3.epc1.fca, type = "normalized"), type = "p")

## Auto-regressive moving-average error structures (corARMA())

# set values and starting values.  p = number of autoregressive parameters, q = number of moving average parameters

# ARMA(1,0) Should be the same as the corAR1 above. Starting values based on original acf plots
epc1.fca.cs1 <- corARMA(0.9, p = 1, q = 0, form = ~year)
# ARMA(2,0). 
epc1.fca.cs2 <- corARMA(c(0.4, 0.4), p = 2, q = 0, form = ~year)
# ARMA(3,0)
epc1.fca.cs3 <- corARMA(c(0.3, 0.3, 0.3), p = 3, q = 0, form = ~year)
# ARMA(0,1)
epc1.fca.cs4 <- corARMA(c(0.2), p = 0, q = 1, form = ~year)
# ARMA(0,2)
epc1.fca.cs5 <- corARMA(c(0.2, 0.2), p = 0, q = 2, form = ~year) 
# ARMA(0,3)
epc1.fca.cs6 <- corARMA(c(0.2,0.2,0.2), p = 0, q = 3, form = ~year)
# ARMA(1,1)
epc1.fca.cs7 <- corARMA(c(0.9,0.2), p = 1, q = 1, form = ~year)
# ARMA(2,1)
epc1.fca.cs8 <- corARMA(c(0.4,0.4,0.2), p = 2, q = 1, form = ~year)
# ARMA(1,2)
epc1.fca.cs9 <- corARMA(c(0.9,0.2,0.2), p = 1, q = 2, form = ~year)
#ARMA(2,2)
epc1.fca.cs10 <- corARMA(c(0.4,0.4,0.2,0.2), p = 2, q = 2, form = ~year)

# ARMA models
m4.epc1.fca <- gls(for_cov_area ~ econ_PC1+year, 
                   correlation = epc1.fca.cs1)
summary(m4.epc1.fca)
plot(residuals(m4.epc1.fca, type = "normalized") ~ year)
acf(residuals(m4.epc1.fca, type = "normalized"))
acf(residuals(m4.epc1.fca, type = "normalized"), type = "p")

m5.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs2)
summary(m5.epc1.fca)
plot(residuals(m5.epc1.fca, type = "normalized") ~ year)
acf(residuals(m5.epc1.fca, type = "normalized"))
acf(residuals(m5.epc1.fca, type = "normalized"), type = "p")

m6.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs3)
summary(m6.epc1.fca)
plot(residuals(m6.epc1.fca, type = "normalized") ~ year)
acf(residuals(m6.epc1.fca, type = "normalized"))
acf(residuals(m6.epc1.fca, type = "normalized"), type = "p")

m7.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs4)
summary(m7.epc1.fca)
plot(residuals(m7.epc1.fca, type = "normalized") ~ year)
acf(residuals(m7.epc1.fca, type = "normalized"))
acf(residuals(m7.epc1.fca, type = "normalized"), type = "p")

m8.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs5)
summary(m8.epc1.fca)
plot(residuals(m8.epc1.fca, type = "normalized") ~ year)
acf(residuals(m8.epc1.fca, type = "normalized"))
acf(residuals(m8.epc1.fca, type = "normalized"), type = "p")

m9.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs6)
summary(m9.epc1.fca)
plot(residuals(m9.epc1.fca, type = "normalized") ~ year)
acf(residuals(m9.epc1.fca, type = "normalized"))
acf(residuals(m9.epc1.fca, type = "normalized"), type = "p")

m10.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs7)
summary(m10.epc1.fca)
plot(residuals(m10.epc1.fca, type = "normalized") ~ year)
acf(residuals(m10.epc1.fca, type = "normalized"))
acf(residuals(m10.epc1.fca, type = "normalized"), type = "p")

m11.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs8)
summary(m11.epc1.fca)
plot(residuals(m11.epc1.fca, type = "normalized") ~ year)
acf(residuals(m11.epc1.fca, type = "normalized"))
acf(residuals(m11.epc1.fca, type = "normalized"), type = "p")

m12.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs9)
summary(m12.epc1.fca)
plot(residuals(m12.epc1.fca, type = "normalized") ~ year)
acf(residuals(m12.epc1.fca, type = "normalized"))
acf(residuals(m12.epc1.fca, type = "normalized"), type = "p")

m13.epc1.fca <- gls(for_cov_area ~ econ_PC1+year,
                   correlation = epc1.fca.cs10)
summary(m13.epc1.fca)
plot(residuals(m13.epc1.fca, type = "normalized") ~ year)
acf(residuals(m13.epc1.fca, type = "normalized"))
acf(residuals(m13.epc1.fca, type = "normalized"), type = "p")


  ## GAMs ####

m14.epc1.fca <- gam(for_cov_area ~ s(econ_PC1,bs='cr', k=22) +year, 
                    gamma = 2)
summary(m14.epc1.fca)
acf(residuals.gam(m14.epc1.fca, type = "response"))
pacf(residuals.gam(m14.epc1.fca, type = "response"))
plot(m14.epc1.fca, residuals = T)
gam.check(m14.epc1.fca)
# predicted over new data
nd <- data.frame(econ_PC1 = seq(-4, 4, length = 23),
                 year = seq(1993,2015,1))
m14.pred.nd <- predict(m14.epc1.fca, newdata=nd, type="terms", se.fit=T)
m14.upr.nd <- m14.pred.nd$fit[,2] + (2 * m14.pred.nd$se.fit[,2])
m14.lwr.nd <- m14.pred.nd$fit[,2] - (2 * m14.pred.nd$se.fit[,2])
m14.pred.pc1 <- m14.pred.nd$fit[,2]
m14.pred.yr  <- m14.pred.nd$fit[,1]
# predicted over original data
m14.pred <- predict(m14.epc1.fca, se.fit = T)
m14.upr <- m14.pred$fit + (2 * m14.pred$se.fit)
m14.lwr <- m14.pred$fit - (2 * m14.pred$se.fit)


m14a.epc1.fca <- gam(for_cov_area ~ s(econ_PC1))
acf(residuals.gam(m14a.epc1.fca, type = "response"))
pacf(residuals.gam(m14a.epc1.fca, type = "response"))
summary(m14a.epc1.fca)
plot(m14a.epc1.fca)
#predicted over orignal data
m14a.pred <- predict(m14a.epc1.fca, se.fit=T)
m14a.upr <- m14a.pred$fit + (2 * m14a.pred$se.fit)
m14a.lwr <- m14a.pred$fit - (2 * m14a.pred$se.fit)
#predicted over new data
ndm14a <- data.frame(econ_PC1 = seq(-4,4,length=23))
m14a.pred.nd <- predict(m14a.epc1.fca, newdata = ndm14a, se.fit = T)
m14a.upr.nd <- m14a.pred.nd$fit + (2 * m14a.pred.nd$se.fit)
m14a.lwr.nd <- m14a.pred.nd$fit - (2 * m14a.pred.nd$se.fit)


m14b.epc1.fca <- gam(for_cov_area ~ s(econ_PC1)+ s(year))
acf(residuals.gam(m14b.epc1.fca, type = "response"))
pacf(residuals.gam(m14b.epc1.fca, type = "response"))
summary(m14b.epc1.fca)
m14b.pred <- predict(m14b.epc1.fca, se.fit=T)
m14b.upr <- m14b.pred$fit + (2 * m14b.pred$se.fit)
m14b.lwr <- m14b.pred$fit - (2 * m14b.pred$se.fit)

m14c.epc1.fca <- gam(for_cov_area ~ s(year))
acf(residuals.gam(m14c.epc1.fca, type = "response"))
pacf(residuals.gam(m14c.epc1.fca, type = "response"))
summary(m14c.epc1.fca)

m14d.epc1.fca <- gam(for_cov_area ~ s(econ_PC1, bs='cr', k=19))
acf(residuals.gam(m14c.epc1.fca, type = "response"))
pacf(residuals.gam(m14c.epc1.fca, type = "response"))
summary(m14d.epc1.fca)


m15.epc1.fca <- gam(for_cov_area ~ s(econ_PC1, bs='cr', k=19)+
                      year, correlation = epc1.fca.cs3)
summary(m15.epc1.fca)

m16.epc1.fca <- gam(for_cov_area ~ s(econ_PC1, bs='bs', k=19)+
                      year, correlation = epc1.fca.cs3)
summary(m16.epc1.fca)

m17.epc1.fca <- gamm(for_cov_area ~ s(econ_PC1, bs='cr', k=19) +year, 
                     correlation = corAR1(form = ~year))
acf(resid(m17.epc1.fca$gam, type = "response"))
pacf(resid(m17.epc1.fca$gam, type = "response"))

m17a.epc1.fca <- gamm(for_cov_area ~ s(econ_PC1, bs='cr', k=19) +year)
acf(resid(m17a.epc1.fca$gam, type = "response"))
pacf(resid(m17a.epc1.fca$gam, type = "response"))

m17b.epc1.fca <- gamm4(for_cov_area ~ s(econ_PC1, bs='cr', k=19),
                       correlation = corAR1(form = ~econ_PC1))
acf(resid(m17b.epc1.fca$gam, type = "response"))
pacf(resid(m17b.epc1.fca$gam, type = "response"))

m17c.epc1.fca <- gamm4(for_cov_area ~ s(econ_PC1, bs='cr', k=19))
acf(resid(m17c.epc1.fca$gam, type = "response"))
pacf(resid(m17c.epc1.fca$gam, type = "response"))


p1 <- plotGAM(m14.epc1.fca, smooth.cov = "econ_PC1", rawOrFitted = "raw", 
        plotCI = T)
p2 <- plotGAM(m14a.epc1.fca, smooth.cov = "econ_PC1", rawOrFitted ="raw", 
        plotCI = T)
p3 <- plotGAM(m14b.epc1.fca, smooth.cov = "econ_PC1", rawOrFitted ="raw", 
        plotCI = T)
p4 <- plotGAM(m14c.epc1.fca, smooth.cov = "year", rawOrFitted ="raw", 
        plotCI = T)
p5 <- plotGAMM(m17.epc1.fca, smooth.cov = "econ_PC1", rawOrFitted = F, 
        plotCI = T)
p6 <- plotGAMM(m17.epc1.fca, smooth.cov = "econ_PC1", rawOrFitted = "raw",
        plotCI = T)
p7 <- plotGAMM(m17a.epc1.fca, smooth.cov = "econ_PC1", rawOrFitted = "raw"         ,plotCI = T)

plot_grid(p8,p9,p10,p11)

plot.gam(m17.epc1.fca, se=T)

# for_cov ~ s(econ_PC1) ##predicted over original data
p8 <- ggplot(dat_master, aes(x=econ_PC1, y=for_cov_area))+
      geom_point(aes(x=econ_PC1, y=for_cov_area))+
      geom_ribbon(aes(ymin=m14a.lwr, ymax=m14a.upr, fill="red"),
                  alpha=0.4)+
      theme(legend.position = "none")+
      geom_line(aes(y=m14a.pred$fit), col="blue")+
      ggtitle("A")

# for_cov ~ s(econ_PC1) + year ##predicted over original data
p9 <- ggplot(dat_master, aes(x=econ_PC1, y=for_cov_area))+
      geom_point(aes(x=econ_PC1, y=for_cov_area))+
      geom_ribbon(aes(ymin=m14.lwr, ymax=m14.upr, fill="red"),alpha=0.4)+
      theme(legend.position = "none")+
      geom_line(aes(y=m14.pred$fit), col="blue")+
      ggtitle("B")

# for_cov ~ s(econ_PC1) + year ## predicted over new data
p10 <- ggplot(dat_master, aes(x=econ_PC1, y=for_cov_area))+
      geom_point(aes(x=econ_PC1, y=for_cov_area))+
      geom_ribbon(aes(ymin=m14.lwr.nd, ymax=m14.upr.nd, 
                      fill="red"),alpha=0.4)+
      theme(legend.position = "none")+
      geom_line(aes(y=m14.pred.pc1), col="blue")+
      geom_line(aes(y=m14.pred.yr), col="green")+
      ggtitle("C")

# for_cov ~ s(econ_PC1) ##predicted over new data
p11 <- ggplot(dat_master, aes(x=econ_PC1, y=for_cov_area))+
      geom_point(aes(x=econ_PC1, y=for_cov_area))+
      geom_ribbon(aes(ymin=m14a.lwr.nd, ymax=m14a.upr.nd, 
                      fill="red"),alpha=0.4)+
      theme(legend.position = "none")+
      geom_line(aes(y=m14a.pred.nd$fit), col="blue")+
      ggtitle("D")

# for_cov ~s(econ_PC1) + s(year)
ggplot(dat_master, aes(x=econ_PC1, y=for_cov_area))+
  geom_point(aes(x=econ_PC1, y=for_cov_area))+
  geom_ribbon(aes(ymin=m14b.lwr, ymax=m14b.upr, fill="red"),alpha=0.4)+
  theme(legend.position = "none")+
  geom_line(aes(y=m14b.pred$fit), col="blue")+
  ggtitle("C")



plot(for_cov_roc, econ_PC1)

plot(year)


#### Forest cover (RoC) ~ Econ_PC1 ####

  ## Testing autocorrelation structures ####

# remove first observation
for_cov_roc_red <- for_cov_roc[2:23]
econ_PC1_red <- econ_PC1[2:23]

# Null model
m0.epc1.fcr <- gls(for_cov_roc_red ~ econ_PC1_red)
summary(m0.epc1.fcr)
plot(residuals(m0.epc1.fcr, type = "normalized") ~ year_red)
acf(residuals(m0.epc1.fcr, type = "normalized"))
acf(residuals(m0.epc1.fcr, type = "normalized"), type = "p")

# Null model with year
m1.epc1.fcr <- gls(for_cov_roc_red ~ econ_PC1_red + year_red)
summary(m1.epc1.fcr)
plot(residuals(m1.epc1.fcr, type = "normalized") ~ year_red)
acf(residuals(m1.epc1.fcr, type = "normalized"))
acf(residuals(m1.epc1.fcr, type = "normalized"), type = "p")

## Compound symmetry structure
m2.epc1.fcr <- gls(for_cov_roc ~ econ_PC1 + year, correlation = corCompSymm(form = ~year))
summary(m2.epc1.fcr)
plot(residuals(m2.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m2.epc1.fcr, type = "normalized"))
acf(residuals(m2.epc1.fcr, type = "normalized"), type = "p")

## 1st order autoregressive covariance structure

# Using reduced maximum liklihood (method = "REML"). Use this method when comparing models with different covariance structures

m3.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year, method="REML", correlation = corAR1(form = ~year))
summary(m3.epc1.fcr)
plot(residuals(m3.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m3.epc1.fcr, type = "normalized"))
acf(residuals(m3.epc1.fcr, type = "normalized"), type = "p")

## Auto-regressive moving-average error structures (corARMA())

# set values and starting values.  p = number of autoregressive parameters, q = number of moving average parameters

# ARMA(1,0) Should be the same as the corAR1 above. Starting values based on original acf plots
epc1.fcr.cs1 <- corARMA(0.9, p = 1, q = 0, form = ~year)
# ARMA(2,0). 
epc1.fcr.cs2 <- corARMA(c(0.4, 0.4), p = 2, q = 0, form = ~year)
# ARMA(3,0)
epc1.fcr.cs3 <- corARMA(c(0.3, 0.3, 0.3), p = 3, q = 0, form = ~year)
# ARMA(0,1)
epc1.fcr.cs4 <- corARMA(c(0.2), p = 0, q = 1, form = ~year)
# ARMA(0,2)
epc1.fcr.cs5 <- corARMA(c(0.2, 0.2), p = 0, q = 2, form = ~year) 
# ARMA(0,3)
epc1.fcr.cs6 <- corARMA(c(0.2,0.2,0.2), p = 0, q = 3, form = ~year)
# ARMA(1,1)
epc1.fcr.cs7 <- corARMA(c(0.9,0.2), p = 1, q = 1, form = ~year)
# ARMA(2,1)
epc1.fcr.cs8 <- corARMA(c(0.4,0.4,0.2), p = 2, q = 1, form = ~year)
# ARMA(1,2)
epc1.fcr.cs9 <- corARMA(c(0.9,0.2,0.2), p = 1, q = 2, form = ~year)
#ARMA(2,2)
epc1.fcr.cs10 <- corARMA(c(0.4,0.4,0.2,0.2), p = 2, q = 2, form = ~year)

# ARMA models
m4.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year, 
                   correlation = epc1.fcr.cs1)
summary(m4.epc1.fcr)
plot(residuals(m4.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m4.epc1.fcr, type = "normalized"))
acf(residuals(m4.epc1.fcr, type = "normalized"), type = "p")

m5.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                   correlation = epc1.fcr.cs2)
summary(m5.epc1.fcr)
plot(residuals(m5.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m5.epc1.fcr, type = "normalized"))
acf(residuals(m5.epc1.fcr, type = "normalized"), type = "p")

m6.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                   correlation = epc1.fcr.cs3)
summary(m6.epc1.fcr)
plot(residuals(m6.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m6.epc1.fcr, type = "normalized"))
acf(residuals(m6.epc1.fcr, type = "normalized"), type = "p")

m7.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                   correlation = epc1.fcr.cs4)
summary(m7.epc1.fcr)
plot(residuals(m7.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m7.epc1.fcr, type = "normalized"))
acf(residuals(m7.epc1.fcr, type = "normalized"), type = "p")

m8.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                   correlation = epc1.fcr.cs5)
summary(m8.epc1.fcr)
plot(residuals(m8.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m8.epc1.fcr, type = "normalized"))
acf(residuals(m8.epc1.fcr, type = "normalized"), type = "p")

m9.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                   correlation = epc1.fcr.cs6)
summary(m9.epc1.fcr)
plot(residuals(m9.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m9.epc1.fcr, type = "normalized"))
acf(residuals(m9.epc1.fcr, type = "normalized"), type = "p")

m10.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                    correlation = epc1.fcr.cs7)
summary(m10.epc1.fcr)
plot(residuals(m10.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m10.epc1.fcr, type = "normalized"))
acf(residuals(m10.epc1.fcr, type = "normalized"), type = "p")

m11.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                    correlation = epc1.fcr.cs8)
summary(m11.epc1.fcr)
plot(residuals(m11.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m11.epc1.fcr, type = "normalized"))
acf(residuals(m11.epc1.fcr, type = "normalized"), type = "p")

m12.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                    correlation = epc1.fcr.cs9)
summary(m12.epc1.fcr)
plot(residuals(m12.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m12.epc1.fcr, type = "normalized"))
acf(residuals(m12.epc1.fcr, type = "normalized"), type = "p")

m13.epc1.fcr <- gls(for_cov_roc ~ econ_PC1+year,
                    correlation = epc1.fcr.cs10)
summary(m13.epc1.fcr)
plot(residuals(m13.epc1.fcr, type = "normalized") ~ year)
acf(residuals(m13.epc1.fcr, type = "normalized"))
acf(residuals(m13.epc1.fcr, type = "normalized"), type = "p")

  ## GAMs ####

# default spline is penalised thin plate regression (bs="tp")
m14.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red), data = dat_for_cov_roc)
summary(m14.epc1.fcr)
plot(residuals(m14.epc1.fcr, type = "response") ~ year_red)
acf(residuals.gam(m14.epc1.fcr, type = "response"))
pacf(residuals.gam(m14.epc1.fcr, type = "response"))
plot(m14.epc1.fcr, residuals = T)
gam.check(m14.epc1.fcr)
plotGAM(m14.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", 
        plotCI = T)

# "cr" = cubic regression spline
m15.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr"), data = dat_for_cov_roc)
summary(m15.epc1.fcr)
plot(residuals(m15.epc1.fcr, type = "response") ~ year_red)
acf(residuals.gam(m15.epc1.fcr, type = "response"))
pacf(residuals.gam(m15.epc1.fcr, type = "response"))
plot(m15.epc1.fcr, residuals = T)
gam.check(m15.epc1.fcr)
plotGAM(m15.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", 
        plotCI = T)

# number of knots based on below tests
m16.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=5),data = dat_for_cov_roc)
summary(m16.epc1.fcr)
plot(residuals(m16.epc1.fcr, type = "response") ~ year_red)
acf(residuals.gam(m16.epc1.fcr, type = "response"))
pacf(residuals.gam(m16.epc1.fcr, type = "response"))
plot(m16.epc1.fcr, residuals = T)
gam.check(m16.epc1.fcr)
plotGAM(m16.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", 
        plotCI = T)

# testing number of knots for above model m16
m16a.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=4))
m16b.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=5))
m16c.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=6))
m16d.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=7))
m16e.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=8))
m16f.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=9))
m16g.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=10))
m16h.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=11))
m16i.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=12))
m16j.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=13))
m16k.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=14))
m16l.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=15))
m16m.epc1.fcr <- gam(for_cov_roc_red ~ s(econ_PC1_red, bs="cr", k=16))

p1 <- plotGAM(m16a.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p2 <- plotGAM(m16b.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p3 <- plotGAM(m16c.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p4 <- plotGAM(m16d.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p5 <- plotGAM(m16e.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p6 <- plotGAM(m16f.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p7 <- plotGAM(m16g.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p8 <- plotGAM(m16h.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p9 <- plotGAM(m16i.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p10 <- plotGAM(m16j.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p11 <- plotGAM(m16k.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p12 <- plotGAM(m16l.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
p13 <- plotGAM(m16m.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)

# Plots of two best models
pa <- plotGAM(m14.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted = "raw", plotCI = T)
pb <- plotGAM(m16.epc1.fcr, smooth.cov = "econ_PC1_red", rawOrFitted =  "raw", plotCI = T)
plot_grid(pa,pb)

### predicting with two best models

## m14
# predicted over new data
nd <- data.frame(econ_PC1_red = seq(-2.5, 4, length = 22))
m14r.pred.nd <- predict(m14.epc1.fcr, newdata=nd, se.fit=T)
m14r.upr.nd <- m14r.pred.nd$fit + (2 * m14r.pred.nd$se.fit)
m14r.lwr.nd <- m14r.pred.nd$fit - (2 * m14r.pred.nd$se.fit)

# create datafame for plotting
df.plot <- data.frame(x = numeric(22), y = numeric(22), x.pred = numeric(22), y.pred = numeric(22))
df.plot$x <- dat_for_cov_roc$econ_PC1_red
df.plot$y <- dat_for_cov_roc$for_cov_roc_red
df.plot$x.pred <- nd$econ_PC1_red
df.plot$y.pred <- m14r.pred.nd$fit
df.plot$m14r.upr.nd <- m14r.upr.nd
df.plot$m14r.lwr.nd <- m14r.lwr.nd

# plot predicted over new data
p14 <- ggplot(df.plot, aes(x=x, y=y))+
  geom_point(aes(x=x.pred, y=y.pred))+
  geom_ribbon(aes(ymin=m14r.lwr.nd, ymax=m14r.upr.nd, 
                  fill="red"),alpha=0.4)+
  theme(legend.position = "none")+
  geom_line(aes(x=x.pred, y=y.pred), col="blue")+
  ggtitle("m14 new data")

# predicted over original data
m14r.pred <- predict(m14.epc1.fcr, se.fit = T)
m14r.upr <- m14r.pred$fit + (2 * m14r.pred$se.fit)
m14r.lwr <- m14r.pred$fit - (2 * m14r.pred$se.fit)

# plot predicted over original data
p15 <- ggplot(dat_for_cov_roc, aes(x=econ_PC1_red, y=for_cov_roc_red))+
  geom_point(aes(x=econ_PC1_red, y=for_cov_roc_red))+
  geom_ribbon(aes(ymin=m14r.lwr, ymax=m14r.upr, fill="red"),
              alpha=0.4)+
  theme(legend.position = "none")+
  geom_line(aes(y=m14r.pred$fit), col="blue")+
  ggtitle("m14 orig data")

plot_grid(p14,p15)

## m16
# predicted over new data
ndm16 <- data.frame(econ_PC1_red = seq(-2.5, 4, length = 22))
m16r.pred.nd <- predict(m16.epc1.fcr, newdata=ndm16, se.fit=T)
m16r.upr.nd <- m16r.pred.nd$fit + (2 * m16r.pred.nd$se.fit)
m16r.lwr.nd <- m16r.pred.nd$fit - (2 * m16r.pred.nd$se.fit)

# create datafame for plotting
df.plot1 <- data.frame(x = numeric(22), y = numeric(22), x.pred = numeric(22), y.pred = numeric(22))
df.plot1$x <- dat_for_cov_roc$econ_PC1_red
df.plot1$y <- dat_for_cov_roc$for_cov_roc_red
df.plot1$x.pred <- ndm16$econ_PC1_red
df.plot1$y.pred <- m16r.pred.nd$fit
df.plot1$m16r.upr.nd <- m16r.upr.nd
df.plot1$m16r.lwr.nd <- m16r.lwr.nd

# plot predicted over new data
p16 <- ggplot(df.plot1, aes(x=x, y=y))+
  geom_point(aes(x=x.pred, y=y.pred))+
  geom_ribbon(aes(ymin=m16r.lwr.nd, ymax=m16r.upr.nd, 
                  fill="red"),alpha=0.4)+
  theme(legend.position = "none")+
  geom_line(aes(y=y.pred), col="blue")+
  ggtitle("m16 new data")

# predicted over original data
m16r.pred <- predict(m16.epc1.fcr, se.fit = T)
m16r.upr <- m16r.pred$fit + (2 * m16r.pred$se.fit)
m16r.lwr <- m16r.pred$fit - (2 * m16r.pred$se.fit)

# plot predicted over original data
p17 <- ggplot(dat_for_cov_roc, aes(x=econ_PC1_red, y=for_cov_roc_red))+
  geom_point(aes(x=econ_PC1_red, y=for_cov_roc_red))+
  geom_ribbon(aes(ymin=m14r.lwr, ymax=m14r.upr, fill="red"),
              alpha=0.4)+
  theme(legend.position = "none")+
  geom_line(aes(y=m14r.pred$fit), col="blue")+
  ggtitle("m14 orig data")

plot_grid(p14,p15)










