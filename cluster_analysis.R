### Load libraries and data preparation ####

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

# Function to compute a binary dissimilarity matrix from clusters (from book)
grpdist <- function(X) {
require(cluster)
gr <- as.data.frame(as.factor(X))
distgr <- daisy(gr, "gower")
distgr
}


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



### Single linkage agglomerative clusering (nearest neighbour sorting) ####

# from Borcard 2018 cluster analysis chapter

# remove Province column and save as vector
provs <- mean.dat$Province
mean.dat <- mean.dat[ ,-1]

# compute matrix of distance
dist <- vegdist(mean.dat,"euc")

# attach province names
attr(dist, "labels") <- provs

# compute single linkage clustering
single.link.aglom <- hclust(dist, method="single")

# plot dendrogram using default options
plot(single.link.aglom, labels=provs, main = "Single linkage")

# phnom penh is clearly an outlier, and in terms of provinces, I am not that interested in where it sits as it is not really a true province

# remove PP
mean.dat <- mean.dat[-15,]
provs <- provs[! provs %in% "Phnom Penh"]

# compute matrix of distance
dist <- vegdist(mean.dat,"euc")

# attach province names
attr(dist, "labels") <- provs

# compute single linkage clustering
single.link.aglom <- hclust(dist, method="single")

# plot dendrogram using default options
plot(single.link.aglom, labels=provs, main = "Single linkage")

### Complete linkage agglomerative clusering (furthest neighour sorting) ####


# compute complete linkage clustering
complete.link.aglom <- hclust(dist, method="complete")

# plot dendrogram using default options
plot(complete.link.aglom, labels=provs, main = "Complete linkage")


### Average agglomerative clustering ####
  ## Unweighted pair-group using arithmetic averages (UPGMA) ####

# compute UPGMA clustering
UPGMA.aglom <- hclust(dist, method="average")

# plot dendrogram using default options
plot(UPGMA.aglom, labels=provs, main = "UPGMA")

  ## Unweighted pair-group using centroids (UPGMC) ####


# compute UPGMC clustering
UPGMC.aglom <- hclust(dist, method="centroid")

# plot dendrogram using default options
plot(UPGMC.aglom, labels=provs, main = "UPGMC")

### Ward's minimum variance clustering ####

# ward.D2 - with minimum variance clustering criterion 

# compute Ward's clustering
ward2.aglom <- hclust(dist, method="ward.D2")

# plot dendrogram using default options
plot(ward2.aglom, labels=provs, main = "Ward2")
