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
m.macroecon.glob <- glm(elc ~ gdp+gdp.L1+gdp.L2+fdi+fdi.L1+fdi.L2+
                          ind_gdp+ind_gdp.L1+ind_gdp.L2+agr_gdp+agr_gdp.L1+agr_gdp.L2+
                          dev_agri+dev_agri.L1+dev_agri.L2+dev_env+dev_env.L1+dev_env.L2+
                          pop_den+pop_den.L1+pop_den.L2+for_rem+for_rem.L1+for_rem.L2+time,
                        na.action = "na.fail", family=poisson, data=dat)

# dredge
m.econ.glob.d <- dredge(m.macroecon.glob, beta = "none", evaluate = TRUE, rank = AICc)