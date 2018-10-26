library('nlme')
library('ggplot2')
library('cowplot')
library('tidyverse')
library('gamm4')
library('mgcv')
library('voxel')
library('nlstools')

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


## Scatter plots of raw economic predictors
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

## Scatter plots of raw economic predictors with log-transformed Y
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

# Remove the first two years (due to NA in for_cov_roc and the outlier), and the last year (due to outlier)
dat_sub <- dat_work %>% 
  filter(.,!year %in% c("1993","1994","2015"))

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

plot_grid(p17,p18,p19,p20,p21,p22)


## Scatter plots of raw commodity predictors and raw Y
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


plot_grid(p23,p24,p25,p26,p27)

## Scatter plots of raw commodity predictors with log-transformed Y with subsetted data
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

plot_grid(p28,p29,p30,p31,p32)
