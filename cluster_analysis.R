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
dat <- read.csv("Data/commune/dat2.csv", header = TRUE, stringsAsFactors = TRUE) 
socioeconDat <- dat %>% select(Province,tot_pop,land_confl,Pax_migt_in,Pax_migt_out,prop_ind,pop_den,M6_24_sch,propPrimSec,propSecSec,
                       Les1_R_Land,pig_fam,dist_sch,garbage,KM_Comm,crim_case)

socioeconDat$year <- rep(c(1,2,3,4,5,6), times=24)
socioeconDat$Province.yr <- paste(socioeconDat$Province, socioeconDat$year, sep="_")
socioeconDat <- socioeconDat[ , c(-1, -17)]
socioeconDat <- socioeconDat[ ,-16]


socio.dist <- vegdist(socioeconDat,"euc")
attr(socio.dist, "labels") <- socioeconDat$Province.yr
single.link.aglom <- hclust(socio.dist, method="single")

plot(single.link.aglom, labels=socioeconDat$Province.yr, main = "Single linkage")
