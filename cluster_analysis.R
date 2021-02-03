### Load libraries and data ####

library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(pvclust)
library(RColorBrewer)
library(labdsv)
library(rioja)
library(indicspecies)
library(mvpart)
library(MVPARTwrap)
library(dendextend)
library(vegclust)
library(colorspace)
library(agricolae)
library(picante)
library(tidyverse)

# Function to compute a binary dissimilarity matrix from clusters
grpdist <- function(X) {
require(cluster)
gr <- as.data.frame(as.factor(X))
distgr <- daisy(gr, "gower")
distgr
}


# load dat2 (provincial level data that is scaled already)
dat2 <- read.csv("Data/commune/dat2.csv", header = TRUE, stringsAsFactors = TRUE) 
socioeconDat <- dat2 %>% select(Province,tot_pop,land_confl,Pax_migt_in,Pax_migt_out,prop_ind,pop_den,M6_24_sch,
                               propPrimSec,propSecSec,Les1_R_Land,pig_fam,dist_sch,garbage,KM_Comm,crim_case)


# load raw data (unscaled, province level). I want to take the provincial means across all years for all variables from the raw data, and then scale, rather than trying to take the mean across all years from the scaled data
dat_prov <- read.csv("Data/commune/dat_prov.csv", header = TRUE, stringsAsFactors = TRUE) 
dat_prov <- dat_prov[, -1]

# summarise variables for all years - take the mean for each variable for each province across all years. If I leave all the years in, there are too many rows to deal with. I can explore keeping them all in later
mean.dat <- dat_prov %>% group_by(Province) %>% summarise_all(mean) 
mean.dat <- mean.dat %>% select(Province,tot_pop,land_confl,Pax_migt_in,Pax_migt_out,prop_ind,pop_den,M6_24_sch,
                               propPrimSec,propSecSec,Les1_R_Land,pig_fam,dist_sch,garbage,KM_Comm,crim_case)

# scale all vars
mean.dat <- mean.dat %>% mutate_at(c("tot_pop", "land_confl", "Pax_migt_in","Pax_migt_out","prop_ind", 
                             "pop_den", "M6_24_sch", "propPrimSec", "propSecSec","Les1_R_Land", 
                             "pig_fam", "dist_sch", "garbage", "KM_Comm", "crim_case"),
                             ~(scale(.) %>% as.vector))



socio.dist <- vegdist(socioeconDat,"euc")
attr(socio.dist, "labels") <- socioeconDat$Province.yr
single.link.aglom <- hclust(socio.dist, method="single")

plot(single.link.aglom, labels=socioeconDat$Province.yr, main = "Single linkage")
