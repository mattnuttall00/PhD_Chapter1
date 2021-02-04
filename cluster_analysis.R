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



# remove Province column and save as vector
provs <- mean.dat$Province
mean.dat <- mean.dat[ ,-1]

# remove PP
mean.dat <- mean.dat[-15,]
provs <- provs[! provs %in% "Phnom Penh"]

# compute matrix of distance
dist <- vegdist(mean.dat,"euc")

# attach province names
attr(dist, "labels") <- provs




## Single linkage agglomerative clusering (nearest neighbour sorting) ####

# from Borcard 2018 cluster analysis chapter

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

## Complete linkage agglomerative clusering (furthest neighour sorting) ####


# compute complete linkage clustering
complete.link.aglom <- hclust(dist, method="complete")

# plot dendrogram using default options
plot(complete.link.aglom, labels=provs, main = "Complete linkage")


## Average agglomerative clustering ####
  # Unweighted pair-group using arithmetic averages (UPGMA) ####

# compute UPGMA clustering
UPGMA.aglom <- hclust(dist, method="average")

# plot dendrogram using default options
plot(UPGMA.aglom, labels=provs, main = "UPGMA")

  # Unweighted pair-group using centroids (UPGMC) ####


# compute UPGMC clustering
UPGMC.aglom <- hclust(dist, method="centroid")

# plot dendrogram using default options
plot(UPGMC.aglom, labels=provs, main = "UPGMC")

## Ward's minimum variance clustering ####

# ward.D2 - with minimum variance clustering criterion 

# compute Ward's clustering
ward2.aglom <- hclust(dist, method="ward.D2")

# plot dendrogram using default options
plot(ward2.aglom, labels=provs, main = "Ward2")

## Flexible clustering ####

# using the cluster package and agnes() function

# try a beta-flexible clustering, where you set only the beta value (set to -0.25)

flex.b <- agnes(dist, method="flexible", par.method = 0.625)

# Change the class of agnes object
class(flex.b) # [1] "agnes" "twins"

flex.b <- as.hclust(flex.b)

plot(flex.b,
     labels = provs,
     main = "Beta-flexible (beta=-0.25)")


# try change the beta value to -0.5 to see what happens
flex.b2 <- agnes(dist, method="flexible", par.method = 0.75)
flex.b2 <- as.hclust(flex.b2)

plot(flex.b2,
     labels = provs,
     main = "Beta-flexible (beta=-0.5)")

summary(flex.b2)

### Cophenetic correlation ####

# The cophenetic distance between two objects in a dendrogram is the distance where the two objects become members of the same group. Locate any two objects, start from one, and “climb up the tree” to the first node leading down to the second object:  the level of that node along the distance scale is the cophenetic distance between the two objects. A cophenetic matrix is a matrix representing the cophenetic distances among all pairs of objects.

# A Pearson’s r correlation, called the cophenetic correlation in this context, can be computed between the original dissimilarity matrix and the cophenetic matrix. The method with the highest cophenetic correlation may be seen as the one that produces the clustering model that retains most of the information contained in the dissimilarity matrix. This does not necessarily mean, however, that this clustering model is the most adequate for the researcher’s goal.

# compute cophenetic matrix and correlation between all the above methods
single.coph <- cophenetic(single.link.aglom)
cor(dist,single.coph)

comp.coph <- cophenetic(complete.link.aglom)
cor(dist,comp.coph)

UPGMA.coph <- cophenetic(UPGMA.aglom)
cor(dist,UPGMA.coph)

UPGMC.coph <- cophenetic(UPGMC.aglom)
cor(dist,UPGMC.coph)

ward.coph <- cophenetic(ward2.aglom)
cor(dist,ward.coph)

flex.coph <- cophenetic(flex.b)
cor(dist,flex.coph)

# plot original distances versus cophenetic distances

# Shepard-like diagrams
par(mfrow = c(2, 3))

plot(
  dist,
  single.coph,
  xlab = "distance (matrix)",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, max(dist)),
  ylim = c(0, max(single.link.aglom$height)),
  main = c("Single linkage", paste("Cophenetic correlation =",
                                   round(cor(dist, single.coph), 3)))
)
abline(0, 1)
lines(lowess(dist, single.coph), col = "red")

plot(
  dist,
  comp.coph,
  xlab = "distance (matrix)",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, max(dist)),
  ylim = c(0, max(complete.link.aglom$height)),
  main = c("Complete linkage", paste("Cophenetic correlation =",
                                     round(cor(dist, comp.coph), 3)))
)
abline(0, 1)
lines(lowess(dist, comp.coph), col = "red")

plot(
  dist,
  UPGMA.coph,
  xlab = "distance (matrix)",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, max(dist)),
  ylim = c(0, max(UPGMA.aglom$height)),
  main = c("UPGMA", paste("Cophenetic correlation =",
                          round(cor(dist, UPGMA.coph), 3)))
)
abline(0, 1)
lines(lowess(dist, UPGMA.coph), col = "red")

plot(
  dist,
  UPGMC.coph,
  xlab = "distance (matrix)",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, max(dist)),
  ylim = c(0, max(UPGMC.aglom$height)),
  main = c("UPGMC", paste("Cophenetic correlation =",
                         round(cor(dist, UPGMC.coph), 3)))
)
abline(0, 1)
lines(lowess(dist, UPGMC.coph), col = "red")

plot(
  dist,
  ward.coph,
  xlab = "distance (matrix)",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, max(dist)),
  ylim = c(0, max(ward2.aglom$height)),
  main = c("Ward", paste("Cophenetic correlation =",
                          round(cor(dist, ward.coph), 3)))
)
abline(0, 1)
lines(lowess(dist, ward.coph), col = "red")


plot(
  dist,
  flex.coph,
  xlab = "distance (matrix)",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, max(dist)),
  ylim = c(0, max(flex.b$height)),
  main = c("Flexible", paste("Cophenetic correlation =",
                         round(cor(dist, flex.coph), 3)))
)
abline(0, 1)
lines(lowess(dist, flex.coph), col = "red")