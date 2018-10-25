library('nlme')
library('ggplot2')
library('cowplot')
library('tidyverse')
library('gamm4')
library('mgcv')
library('voxel')

### Load data ####
dat_econ <- read_csv("macroeconomic_vars.csv")
dat_econ <- mutate(dat_econ, year = as.character(year))
str(dat_econ)
dat_resp <- read_csv("ForCov_LU_econVars_PCs.csv")

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

# scatter plot for_cov_roc ~ agr_gdp
ggplot(dat_work, aes(x=agr_gdp, y=for_cov_roc))+
  geom_point()+
  xlim(20,45)

# scatter plot for for_cov_roc ~ ind_gdp
ggplot(dat_work, aes(x=ind_gdp, y=for_cov_roc))+
  geom_point()

# scatter plot for for_cov_roc ~ fdi
ggplot(dat_work, aes(x=fdi, y=for_cov_roc))+
  geom_point()
  
# exploring shapes using GAMS
gam.agr.gdp <- gam(for_cov_roc ~ s(agr_gdp), data=dat_work)
plotGAM(gam.agr.gdp,smooth.cov = "agr_gdp",  rawOrFitted = "raw", plotCI = T)

gam.ind.gdp <- gam(for_cov_roc ~ s(ind_gdp), data = dat_work)
plotGAM(gam.ind.gdp,smooth.cov = "ind_gdp",  rawOrFitted = "raw", plotCI = T)

gam.fdi <- gam(for_cov_roc ~ s(fdi), data = dat_work)
plotGAM(gam.fdi,smooth.cov = "fdi",  rawOrFitted = "raw", plotCI = T)
