#' ---
#' title: Mixed models analysis - socioeconomic predictors of forest cover
#' author: Matt Nuttall
#' date: 16/06/20
#' output:
#'    html_document:
#'      toc: true
#' ---
#' 

#+ include=F, eval=T
library(tidyverse)
library(arm)
library(sjPlot)
library(DHARMa)
library(lme4)
library(Matrix)
library(psych)
library(visreg)
library(car)
library(ggeffects)
library(plotly)
library(patchwork)
library(lattice)
library(reshape2)
library(pbkrtest)
library(MuMIn)
library(RColorBrewer)

# load data
dat <- read.csv("Data/commune/dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

dat1 <- dat %>% 
  mutate_at(c("year","tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec","propSecSec","Les1_R_Land",
              "pig_fam","dist_sch","garbage","KM_Comm","land_confl","crim_case","Pax_migt_in",
              "Pax_migt_out","mean_elev","dist_border","dist_provCap"), ~(scale(.) %>% as.vector))


# merge Province and Commune into a new unique variable (to remove issue of communes with the same name)
dat1 <- dat1 %>% mutate(Provcomm = paste(dat1$Province, dat1$Commune, sep = "_"))

# add original year (i.e. not scaled)
dat1$year.orig <- dat$year

#' # Population demographics
#' 
rmarkdown::render('ongoing_analysis.R')
