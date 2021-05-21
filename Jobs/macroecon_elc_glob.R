library('MuMIn')
library('tidyverse')

# load data (pre-made in Excel)
dat <- read.csv("dat_work_glob.csv", header = TRUE)

time_mod <- lm(elc ~ year, data=dat)
time_mod$residuals
resids <- c(NA,time_mod$residuals)
dat$time <- resids

# drop gdp_gr because there are too many missing values - it's reducing the size of the data
dat <- dat %>% select(!c(gdp_gr, gdp_gr.L1, gdp_gr.L2))


# drop rows with NAs
dat <- dat %>% drop_na()

# full model - gdp, gdp_gr, fdi, ind_gdp, agr_gdp, dev_agri, dev_env, pop_den
m.macroecon.glob <- glm(elc ~ gdp.L1+gdp.L2+fdi.L1+
                              agr_gdp+agr_gdp.L1+
                              dev_env+
                              pop_den+pop_den.L1+pop_den.L2+for_rem+for_rem.L1+for_rem.L2+time,
                        na.action = "na.fail", family=poisson, data=dat)


# dredge
m.econ.glob.d <- dredge(m.macroecon.glob, beta = "none", evaluate = TRUE, rank = AICc)