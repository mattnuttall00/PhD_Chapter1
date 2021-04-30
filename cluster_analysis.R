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
library(sf)
library(viridis)
library(patchwork)
library(reshape2)

# Function to compute a binary dissimilarity matrix from clusters (from book)
grpdist <- function(X) {
require(cluster)
gr <- as.data.frame(as.factor(X))
distgr <- daisy(gr, "gower")
distgr
}


## For the agglomerative clustering, and K-means clustering load the data below and produce "dist".  For the dataset with the k-means clusters already attached, skip down to the "comparing typology of provinces" section where you can load the data in


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

# set row names to be provinces, which means they will be included in later plots as the labels
mean.dat <- as.data.frame(mean.dat)
row.names(mean.dat) <- provs


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

# UPGMA produces best correlation


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

# UPGMA produces best correlation



### Gower distance ####

# Another possible statistic for the comparison of clustering results is the Gower (1983) distance, computed as the sum of squared differences between the original dissimilarities and cophenetic distances. The clustering method that produces the smallest Gower distance may be seen as the one that provides the best clustering model of the dissimilarity matrix.

# note - need to have run the cophenetic() calls in the secion above

# Gower (1983) distance
(gow.dist.single <- sum((dist - single.coph) ^ 2))
(gow.dist.comp <- sum((dist - comp.coph) ^ 2))
(gow.dist.UPGMA <- sum((dist - UPGMA.coph) ^ 2))
(gow.dist.UPGMC <- sum((dist - UPGMC.coph) ^ 2))
(gow.dist.ward <- sum((dist - ward.coph) ^ 2))
(gow.dist.flex <- sum((dist - flex.coph) ^ 2))

# UPGMA produces the lowest Gower score

### Graphing fusion levels ####

par(mfrow=c(2,3))

# Plot the fusion level values of the sinlge linkage clustering
plot(
  single.link.aglom$height,
  nrow(mean.dat):2,
  type = "S",
  main = "Fusion levels - single linkage",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(single.link.aglom$height,
     nrow(mean.dat):2,
     nrow(mean.dat):2,
     col ="red",
     cex=0.8)

# Plot the fusion level values of the complete linkage clustering
plot(
  complete.link.aglom$height,
  nrow(mean.dat):2,
  type = "S",
  main = "Fusion levels - Complete linkage",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(complete.link.aglom$height,
     nrow(mean.dat):2,
     nrow(mean.dat):2,
     col ="red",
     cex=0.8)

# Plot the fusion level values of the UPGMA clustering
plot(
  UPGMA.aglom$height,
  nrow(mean.dat):2,
  type = "S",
  main = "Fusion levels - UPGMA",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(UPGMA.aglom$height,
     nrow(mean.dat):2,
     nrow(mean.dat):2,
     col ="red",
     cex=0.8)

# Plot the fusion level values of the UPGMC clustering
plot(
  UPGMC.aglom$height,
  nrow(mean.dat):2,
  type = "S",
  main = "Fusion levels - UPGMC",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(UPGMC.aglom$height,
     nrow(mean.dat):2,
     nrow(mean.dat):2,
     col ="red",
     cex=0.8)

# Plot the fusion level values of the Ward clustering
plot(
  ward2.aglom$height,
  nrow(mean.dat):2,
  type = "S",
  main = "Fusion levels - Ward",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(ward2.aglom$height,
     nrow(mean.dat):2,
     nrow(mean.dat):2,
     col ="red",
     cex=0.8)


# Plot the fusion level values of the flexible clustering
plot(
  flex.b$height,
  nrow(mean.dat):2,
  type = "S",
  main = "Fusion levels - beta flexible",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(flex.b$height,
     nrow(mean.dat):2,
     nrow(mean.dat):2,
     col ="red",
     cex=0.8)

# methods suggest 2, 3 or 4 clusters would be a good cut off

### Compare contingency tables ####

# we can cut off all the trees at the same point and compare
k3 <- 3
k4 <- 4
k5 <- 5

# 3 clusters
single.link.aglom.g3   <- cutree(single.link.aglom, k=k3)
complete.link.aglom.g3 <- cutree(complete.link.aglom, k=k3)
UPGMA.aglom.g3         <- cutree(UPGMA.aglom, k=k3)
UPGMC.aglom.g3         <- cutree(UPGMC.aglom, k=k3)
ward2.aglom.g3         <- cutree(ward2.aglom, k=k3)
flex.b.g3              <- cutree(flex.b, k=k3)

# 4 cluster
single.link.aglom.g4   <- cutree(single.link.aglom, k=k4)
complete.link.aglom.g4 <- cutree(complete.link.aglom, k=k4)
UPGMA.aglom.g4         <- cutree(UPGMA.aglom, k=k4)
UPGMC.aglom.g4         <- cutree(UPGMC.aglom, k=k4)
ward2.aglom.g4         <- cutree(ward2.aglom, k=k4)
flex.b.g4              <- cutree(flex.b, k=k4)

# 5 cluster
single.link.aglom.g5   <- cutree(single.link.aglom, k=k5)
complete.link.aglom.g5 <- cutree(complete.link.aglom, k=k5)
UPGMA.aglom.g5         <- cutree(UPGMA.aglom, k=k5)
UPGMC.aglom.g5         <- cutree(UPGMC.aglom, k=k5)
ward2.aglom.g5         <- cutree(ward2.aglom, k=k5)
flex.b.g5              <- cutree(flex.b, k=k5)


# contingency tables for 3 clusters. I will only compare UPGMA (the best) to single and UPGMC (the next best using coph correlations and gower distances)

# 3 clusters
table(UPGMA.aglom.g3, single.link.aglom.g3)
# not a bad match. Both agree that most provinces fall into group 1

table(UPGMA.aglom.g3, UPGMC.aglom.g3)
# perfect agreement


# 4 clusters
table(UPGMA.aglom.g4, single.link.aglom.g4)
# high agreement - 16 provs in group 1

table(UPGMA.aglom.g4, UPGMC.aglom.g4)
# exactly the same as above


# 5 clusters
table(UPGMA.aglom.g5, single.link.aglom.g5)
# not much agreement

table(UPGMA.aglom.g5, UPGMC.aglom.g5)
# not much agreement


### Tangelgrams ####

# Objects of class "hclust" must be first converted into objects of
# class "dendrogram"
UPGMA.dend  <- as.dendrogram(UPGMA.aglom)
UPGMC.dend  <- as.dendrogram(UPGMC.aglom)
single.dend <- as.dendrogram(single.link.aglom)

dend1 <- dendlist(UPGMA.dend, UPGMC.dend)
dend2 <- dendlist(UPGMA.dend, single.dend)


tanglegram(
untangle(dend1),
sort = TRUE,
common_subtrees_color_branches = TRUE,
main_left = "UPGMA",
main_right = "UPGMC"
)


## UPGMA and UPGMC agree on the following groupings:
# Rattankiri, Mondulkiri
# Takeo, Kandal
# Prey veng, Kampong Cham
# Preah Sihanouk, Svay Rieng
# Kampot, Kep

# Pursat, Kampong Chhnang    these two pairs are close together
# Siem Reap, Kampong Thom

# Otdar Meanchey, Kracheh
# Battambang, Banteay Meanchey


tanglegram(
untangle(dend2),
sort = TRUE,
common_subtrees_color_branches = TRUE,
main_left = "UPGMA",
main_right = "Single"
)

## UPGMA and single agree on the following groups:
# Mondulkiri, Pailin,
# Takeo, Kandal
# Prey Veng, Kampong Cham
# Svay Rieng, Kampot
# Pursat, Kampong Chhnang, Siem Reap, Kampong Thom, Kampong Speu
# Stung Treng, Preah Vihear
# Otdar Meanchey, Kracheh
# Battambang, Banteay Meanchey





### Matrix correlation ####

# Optimal number of clusters according to matrix correlation
# statistic (Pearson)
hc <- UPGMA.dend
kt <- data.frame(k = 1:nrow(mean.dat), r = 0)
for (i in 2:(nrow(mean.dat) - 1)) {
gr <- cutree(hc, i)
distgr <- grpdist(gr)
mt <- cor(dist, distgr, method = "pearson")
kt[i, 2] <- mt
}
k.best <- which.max(kt$r)
plot(
kt$k,
kt$r,
type = "h",
main = "Matrix correlation-optimal number of clusters",
xlab = "k (number of clusters)",
ylab = "Pearson's correlation"
)
axis(
1,
k.best,
paste("optimum", k.best, sep = "\n"),
col = "red",
font = 2,
col.axis = "red"
)
points(k.best,
max(kt$r),
pch = 16,
col = "red",
cex = 1.5)

# The plot shows that a partition of 3 to 7 clusters would achieve a high matrix correlation between chord distance matrix and the binary allocation matrix


### Final cluster plots ####

# plotting function (from: https://github.com/JoeyBernhardt/NumericalEcology/blob/master/hcoplot.R)
"hcoplot" <- function(tree, diss, k, 
	title=paste("Reordered dendrogram from", deparse(tree$call), sep="\n"))
{
	require(gclus)
	gr <- cutree(tree, k=k)
	tor <- reorder.hclust(tree, diss)
	plot(tor, hang=-1, xlab=paste(length(gr),"sites"), sub=paste(k,"clusters"), 
		main=title)
	so <- gr[tor$order]
	gro <- numeric(k)
	for (i in 1:k)
	{
		gro[i] <- so[1]
		if (i<k) so <- so[so!=gro[i]]
	}
	rect.hclust(tor, k=k, border=gro+1, cluster=gr)
	legend("topright", paste("Cluster",1:k), pch=22, col=2:(k+1), bty="n")
}


par(mfrow=c(2,2))

# Fast method using the additional hcoplot() function:
hcoplot(UPGMA.aglom, dist, k = 4)
hcoplot(UPGMA.aglom, dist, k = 5)
hcoplot(UPGMA.aglom, dist, k = 6)
hcoplot(UPGMA.aglom, dist, k = 7)

hcoplot(single.link.aglom, dist, k = 4)
hcoplot(single.link.aglom, dist, k = 5)
hcoplot(single.link.aglom, dist, k = 6)
hcoplot(single.link.aglom, dist, k = 7)

# Based on the above plots, I think that 5 clusters is probably the best partitioning. If you go to 6 clusters, you split up Mondulkiri and Ratanikiri which doesn't make sense in the dendogram, or in reality. But then splitting by 6 and 7 clusters splits up the 6 provinces on the far right into smaller pair groups, which is interesting. 


# Bar plot dendrograms with coloured branches

# The "hclust" objects need to be "dendrogram" objects (see tangelgram section above)

# UPGMA - 4 cluster
UPGMA.dend %>% set("branches_k_color", k = 4) %>% plot
# Use standard colours for clusters
clusters <- cutree(UPGMA.dend, 4)[order.dendrogram(UPGMA.dend)]

UPGMA.dend %>%
set("branches_k_color", k = 4, value = unique(clusters) + 1) %>%
plot(main="UPGMA, 4 clusters")
# Add a coloured bar
colored_bars(clusters + 1,
y_shift = -2.5,
text_shift = 1.5,
rowLabels = "")

# UPGMA - 5 cluster
UPGMA.dend %>% set("branches_k_color", k = 5) %>% plot
# Use standard colours for clusters
clusters <- cutree(UPGMA.dend, 5)[order.dendrogram(UPGMA.dend)]

UPGMA.dend %>%
set("branches_k_color", k = 5, value = unique(clusters) + 1) %>%
plot(main="UPGMA, 5 clusters")
# Add a coloured bar
colored_bars(clusters + 1,
y_shift = -2.5,
text_shift = 0.1,
rowLabels = "")

# Single link - 4 cluster
single.dend %>% set("branches_k_color", k = 4) %>% plot
# Use standard colours for clusters
clusters <- cutree(single.dend, 4)[order.dendrogram(single.dend)]
single.dend %>%
set("branches_k_color", k = 4, value = unique(clusters) + 1) %>%
plot(main="Single link, 4 clusters")
# Add a coloured bar
colored_bars(clusters + 1,
y_shift = -1.5,
text_shift = 0.1,
rowLabels = "")

# Single link - 5 cluster
single.dend %>% set("branches_k_color", k = 5) %>% plot
# Use standard colours for clusters
clusters <- cutree(single.dend, 5)[order.dendrogram(single.dend)]
single.dend %>%
set("branches_k_color", k = 5, value = unique(clusters) + 1) %>%
plot(main="Single link, 5 clusters")
# Add a coloured bar
colored_bars(clusters + 1,
y_shift = -1.5,
text_shift = 0.1,
rowLabels = "")



## setting custom colours. 

# example
# Color in function of the cluster
par(mar=c(12,1,1,1))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)

UPGMA.dend %>%
  set("labels_col", value = c("chartreuse3", "cyan4", "darkorange", "seagreen"), k=4) %>%
  set("branches_k_color", value = c("chartreuse3", "cyan4", "darkorange", "seagreen"), k = 4) %>%
  plot
colored_bars(colors = cols, UPGMA.dend)

cols <- c("chartreuse3","chartreuse3","cyan4","darkorange","darkorange","darkorange","darkorange",
          "seagreen","seagreen","seagreen","seagreen","seagreen","seagreen","seagreen","seagreen","seagreen",
          "seagreen","seagreen","seagreen","seagreen","seagreen","seagreen","seagreen")


# set colours
cols <- c("blue","green","orange","red")

clusters <- cutree(UPGMA.dend, 4)[order.dendrogram(UPGMA.dend)]

# for UPGMA, 4 clusters
UPGMA.dend %>% 
  set("labels_col", value = cols, k=4) %>% 
  set("branches_k_color", value = cols, k=4) %>% 
  plot()


k4 <- cutree(UPGMA.dend, k=4)[order.dendrogram(UPGMA.dend)]
plot(UPGMA.dend)
cols <- c("blue","green","orange","red")[k4]
colored_bars(cols, UPGMA.dend)



rows_picking <- c(1:5, 25:30)
dend <- (iris[rows_picking, -5] * 10) %>%
  dist() %>%
  hclust() %>%
  as.dendrogram()
odd_numbers <- rows_picking %% 2
cols <- c("gold", "grey")[odd_numbers + 1]
# scale is off
plot(dend)
colored_bars(cols, dend)
# move and scale a bit
plot(dend)
colored_bars(cols, dend,
  y_shift = -1,
  rowLabels = "Odd\n numbers"
)
# Now let's cut the tree and add that info to the plot:
k2 <- cutree(dend, k = 2)
cols2 <- c("#0082CE", "#CC476B")[k2]
plot(dend)
colored_bars(cols2,dend)
colored_bars(cbind(cols2, cols), dend,
  rowLabels = c("2 clusters", "Odd numbers"))




#
### K-means with random start ####

# use if you have a pre-determined number of clusters

# I decided 5 was good before so I'll start with that

dist.kmeans <- kmeans(dist, centers = 5, nstart = 100)

table(dist.kmeans$cluster, UPGMA.aglom.g5)
# they are not that similar. Some agreement on clusters 3 and 5

### K-means - cascadeKM - iteratively run kmeans with different number of groups ####

## inf.gr and sup.gr are the min and max number of cluster for the function to try 
# using ssi statistic
dist.kM.cascade <- cascadeKM(dist, inf.gr = 2, sup.gr = 10, iter = 100, criterion = "ssi")
summary(dist.kM.cascade)
plot(dist.kM.cascade, sortg=TRUE)

# using calinksi
dist.kM.cascade.2 <- cascadeKM(dist, inf.gr = 2, sup.gr = 10, iter = 100, criterion = "calinski")
plot(dist.kM.cascade.2, sortg=TRUE)

# the documentation for cascadeKM() says that calinksi should not be used if the groups are not of equal size, which based on the above plots, they are not. Therefore the ssi criterion is probably better.


### re-order data based on the kmeans (using 9 groups)

# first run k-means with k=9
dist.kM.k9 <- kmeans(dist, centers = 9, nstart = 100)
k9.clus <- dist.kM.k9$cluster

# attach provinces and cluster back onto main data
mean.dat$province <- provs
mean.dat$cluster <- k9.clus 

mean.dat <- mean.dat %>% arrange(cluster)


### Comparing typology of provinces ####
  ## anovas & KW tests ####

# load the data with the vars meaned over all years and the clusters already attached
dat_prov <- read.csv("Data/commune/dat_prov_clus.csv")

# make cluster factor
dat_prov$cluster <- as.factor(dat_prov$cluster)



### DO NOT repeat the below. THis was to create the above file. Skip down to testing anovas

## I want to use the un-scaled province data for testing and plotting

# load data with no cluster
dat_prov <- read.csv("Data/commune/dat_prov.csv")

# get the mean values across all years (i.e. one variable value per province)
dat_prov <- dat_prov %>% group_by(Province) %>% summarise_all(mean) 

# remove PP
dat_prov <- dat_prov %>% filter(Province != "Phnom Penh")

# extract province and cluster from shapefile
t <- data.frame(Province = as.vector(prov.shp$KHETTRN),
                cluster = as.vector(prov.shp$cluster))

# add cluster to dat_prov
dat_prov$cluster <- t$cluster[match(dat_prov$Province, t$Province)]

# add ELC presence
#elcprov <- c("Takeo","Prey Veng","Pailin","Kep","Kandal","Kampong Chhnang","Battambang")
#dat_prov$elc <- ifelse(dat_prov$Province %in% elcprov, 0, 1)

# add PA presence
#paprov <- c("Kampong Cham","Kandal","Prey Veng","Svay Rieng")
#dat_prov$PA <- ifelse(dat_prov$Province %in% paprov, 0,1)

# save dat_prov with cluster, elc, and PA
write.csv(dat_prov, file="Data/commune/dat_prov_clus.csv")



# first test anova assumptions for the socioecon vars
  shapiro.test(resid(aov(dat_prov$tot_pop ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$land_confl ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$Pax_migt_in ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$Pax_migt_out ~ as.factor(dat_prov$cluster)))) # no
  shapiro.test(resid(aov(dat_prov$prop_ind ~ as.factor(dat_prov$cluster)))) # no
  shapiro.test(resid(aov(dat_prov$pop_den ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$M6_24_sch ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$propPrimSec ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$propSecSec ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$Les1_R_Land ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$pig_fam ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$dist_sch ~ as.factor(dat_prov$cluster)))) # no
  shapiro.test(resid(aov(dat_prov$garbage ~ as.factor(dat_prov$cluster)))) # no
  shapiro.test(resid(aov(dat_prov$KM_Comm ~ as.factor(dat_prov$cluster)))) # fine
  shapiro.test(resid(aov(dat_prov$crim_case ~ as.factor(dat_prov$cluster)))) # fine

# test homogeneity of variances (have to exclude cluster 1 as there is only one province in that cluster)
bartlett.test(dat_prov$tot_pop[dat_prov$cluster != 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # fine
bartlett.test(dat_prov$land_confl[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # fine
bartlett.test(dat_prov$Pax_migt_in[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) #fine
bartlett.test(dat_prov$pop_den[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # fine
bartlett.test(dat_prov$M6_24_sch[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1]))# fine
bartlett.test(dat_prov$propPrimSec[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # fine
bartlett.test(dat_prov$propSecSec[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # fine
bartlett.test(dat_prov$Les1_R_Land[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # fine
bartlett.test(dat_prov$pig_fam[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # NO
bartlett.test(dat_prov$KM_Comm[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # NO
bartlett.test(dat_prov$crim_case[dat_prov$cluster!= 1], as.factor(dat_prov$cluster[dat_prov$cluster!=1])) # fine

# anovas (tot_pop, land_confl,Pax_migt_in,pop_den,M6_24_sch,propPrimSec,propSecSec,Les1_R_Land,crim_case)
aov.tot_pop <- aov(tot_pop ~ as.factor(cluster), data = dat_prov)
tot_pop.f <- summary(aov.tot_pop)[[1]][1,4]
tot_pop.p <- summary(aov.tot_pop)[[1]][1,5]

aov.land_confl <- aov(land_confl ~ as.factor(cluster), data = dat_prov)
land_confl.f <- summary(aov.land_confl)[[1]][1,4]
land_confl.p <- summary(aov.land_confl)[[1]][1,5]

aov.Pax_migt_in <- aov(Pax_migt_in ~ as.factor(cluster), data = dat_prov)
Pax_migt_in.f <- summary(aov.Pax_migt_in)[[1]][1,4]
Pax_migt_in.p <- summary(aov.Pax_migt_in)[[1]][1,5]

aov.pop_den <- aov(pop_den ~ as.factor(cluster), data = dat_prov)
pop_den.f <- summary(aov.pop_den)[[1]][1,4]
pop_den.p <- summary(aov.pop_den)[[1]][1,5]

aov.M6_24_sch <- aov(M6_24_sch ~ as.factor(cluster), data = dat_prov)
M6_24_sch.f <- summary(aov.M6_24_sch)[[1]][1,4]
M6_24_sch.p <- summary(aov.M6_24_sch)[[1]][1,5]

aov.propPrimSec <- aov(propPrimSec ~ as.factor(cluster), data = dat_prov)
propPrimSec.f <- summary(aov.propPrimSec)[[1]][1,4]
propPrimSec.p <- summary(aov.propPrimSec)[[1]][1,5]

aov.propSecSec <- aov(propSecSec ~ as.factor(cluster), data = dat_prov)
propSecSec.f <- summary(aov.propSecSec)[[1]][1,4]
propSecSec.p <- summary(aov.propSecSec)[[1]][1,5]

aov.Les1_R_Land <- aov(Les1_R_Land ~ as.factor(cluster), data = dat_prov)
Les1_R_Land.f <- summary(aov.Les1_R_Land)[[1]][1,4]
Les1_R_Land.p <- summary(aov.Les1_R_Land)[[1]][1,5]

aov.crim_case <- aov(crim_case ~ as.factor(cluster), data = dat_prov)
crim_case.f <- summary(aov.crim_case)[[1]][1,4]
crim_case.p <- summary(aov.crim_case)[[1]][1,5]

test.df <- data.frame(variable = c("tot_pop","land_confl","Pax_migt_in","pop_den","M6_24_sch","propPrimSec",
                                  "propSecSec","Les1_R_Land","crim_case"),
                      test = "AOV",
                     stat = c(tot_pop.f,land_confl.f,Pax_migt_in.f,pop_den.f,M6_24_sch.f,propPrimSec.f,
                               propSecSec.f,Les1_R_Land.f,crim_case.f),
                     Pval = c(tot_pop.p,land_confl.p,Pax_migt_in.p,pop_den.p,M6_24_sch.p,propPrimSec.p,
                              propSecSec.p,Les1_R_Land.p,crim_case.p))


# Kruskal-Wallis tests (Pax_migt_out,prop_ind,dist_sch,garbage,pig_fam,KM_Comm)
kw.Pax_migt_out <- kruskal.test(Pax_migt_out ~ as.factor(cluster), data = dat_prov)
Pax_migt_out.csq <- as.vector(kw.Pax_migt_out$statistic)
Pax_migt_out.p <- kw.Pax_migt_out$p.value

kw.prop_ind <- kruskal.test(prop_ind ~ as.factor(cluster), data = dat_prov)
prop_ind.csq <- as.vector(kw.prop_ind$statistic)
prop_ind.p <- kw.prop_ind$p.value

kw.dist_sch <- kruskal.test(dist_sch ~ as.factor(cluster), data = dat_prov)
dist_sch.csq <- as.vector(kw.dist_sch$statistic)
dist_sch.p <- kw.dist_sch$p.value

kw.garbage <- kruskal.test(garbage ~ as.factor(cluster), data = dat_prov)
garbage.csq <- as.vector(kw.garbage$statistic)
garbage.p <- kw.garbage$p.value

kw.pig_fam <- kruskal.test(pig_fam ~ as.factor(cluster), data = dat_prov)
pig_fam.csq <- as.vector(kw.pig_fam$statistic)
pig_fam.p <- kw.pig_fam$p.value

kw.KM_Comm <- kruskal.test(KM_Comm ~ as.factor(cluster), data = dat_prov)
KM_Comm.csq <- as.vector(kw.KM_Comm$statistic)
KM_Comm.p <- kw.KM_Comm$p.value

kw.df <- data.frame(variable = c("Pax_migt_out","prop_ind","dist_sch","garbage","pig_fam","KM_Comm"),
                    test = "KW",
                    stat = c(19.26,18.54,20.52,2.77,10.29,20.55),
                    Pval = c(Pax_migt_out.p,prop_ind.p,dist_sch.p,garbage.p,pig_fam.p,KM_Comm.p))

test.df <- rbind(test.df,kw.df)

test.df %>% filter(Pval > 0.05)

# tukey post-hoc test

  ## Tukey post-hoc tests ####

# The anovas and KW tests have confirmed that there are some differences between the variable values of the provinces, but I want to know which are different. 

# you first have to run a lm(), then an aov(), then the Tueky test

### population demographics
# total population
tot_pop.lm <- lm(tot_pop ~ cluster, data=dat_prov)
tot_pop.aov <- aov(tot_pop.lm)
aov.tot_pop.Tuk <- HSD.test(tot_pop.aov, "cluster", group = TRUE)

# proportion indigenous
prop_ind.lm <- lm(prop_ind ~ cluster, data=dat_prov)
prop_ind.aov <- aov(prop_ind.lm)
aov.prop_ind.Tuk <- HSD.test(prop_ind.aov, "cluster", group = TRUE)
aov.prop_ind.Tuk

# population density
pop_den.lm <- lm(pop_den ~ cluster, data=dat_prov)
pop_den.aov <- aov(pop_den.lm)
aov.pop_den.Tuk <- HSD.test(pop_den.aov, "cluster", group = TRUE)
aov.pop_den.Tuk

### education
# proportion males in school
M6_24_sch.lm <- lm(M6_24_sch ~ cluster, data=dat_prov)
M6_24_sch.aov <- aov(M6_24_sch.lm)
aov.M6_24_sch.Tuk <- HSD.test(M6_24_sch.aov, "cluster", group = TRUE)
aov.M6_24_sch.Tuk

### employment
# propPrimSec
propPrimSec.lm <- lm(propPrimSec ~ cluster, data=dat_prov)
propPrimSec.aov <- aov(propPrimSec.lm)
aov.propPrimSec.Tuk <- HSD.test(propPrimSec.aov, "cluster", group = TRUE)
aov.propPrimSec.Tuk

# propSecSec
propSecSec.lm <- lm(propSecSec ~ cluster, data=dat_prov)
propSecSec.aov <- aov(propSecSec.lm)
aov.propSecSec.Tuk <- HSD.test(propSecSec.aov, "cluster", group = TRUE)
aov.propSecSec.Tuk


### economic securtiy
# Les1_R_Land
Les1_R_Land.lm <- lm(Les1_R_Land ~ cluster, data=dat_prov)
Les1_R_Land.aov <- aov(Les1_R_Land.lm)
aov.Les1_R_Land.Tuk <- HSD.test(Les1_R_Land.aov, "cluster", group = TRUE)
aov.Les1_R_Land.Tuk

# pig_fam
pig_fam.lm <- lm(pig_fam ~ cluster, data=dat_prov)
pig_fam.aov <- aov(pig_fam.lm)
aov.pig_fam.Tuk <- HSD.test(pig_fam.aov, "cluster", group = TRUE)
aov.pig_fam.Tuk


### access to services
# dist_sch
dist_sch.lm <- lm(dist_sch ~ cluster, data=dat_prov)
dist_sch.aov <- aov(dist_sch.lm)
aov.dist_sch.Tuk <- HSD.test(dist_sch.aov, "cluster", group = TRUE)
aov.dist_sch.Tuk

# garbage
garbage.lm <- lm(garbage ~ cluster, data=dat_prov)
garbage.aov <- aov(garbage.lm)
aov.garbage.Tuk <- HSD.test(garbage.aov, "cluster", group = TRUE)
aov.garbage.Tuk

# KM_Comm
KM_Comm.lm <- lm(KM_Comm ~ cluster, data=dat_prov)
KM_Comm.aov <- aov(KM_Comm.lm)
aov.KM_Comm.Tuk <- HSD.test(KM_Comm.aov, "cluster", group = TRUE)
aov.KM_Comm.Tuk


### social justice
# crim_case
crim_case.lm <- lm(crim_case ~ cluster, data=dat_prov)
crim_case.aov <- aov(crim_case.lm)
aov.crim_case.Tuk <- HSD.test(crim_case.aov, "cluster", group = TRUE)
aov.crim_case.Tuk

# land_confl
land_confl.lm <- lm(land_confl ~ cluster, data=dat_prov)
land_confl.aov <- aov(land_confl.lm)
aov.land_confl.Tuk <- HSD.test(land_confl.aov, "cluster", group = TRUE)
aov.land_confl.Tuk


### migration
# Pax_migt_in
Pax_migt_in.lm <- lm(Pax_migt_in ~ cluster, data=dat_prov)
Pax_migt_in.aov <- aov(Pax_migt_in.lm)
aov.Pax_migt_in.Tuk <- HSD.test(Pax_migt_in.aov, "cluster", group = TRUE)
aov.Pax_migt_in.Tuk

# Pax_migt_out
Pax_migt_out.lm <- lm(Pax_migt_out ~ cluster, data=dat_prov)
Pax_migt_out.aov <- aov(Pax_migt_out.lm)
aov.Pax_migt_out.Tuk <- HSD.test(Pax_migt_out.aov, "cluster", group = TRUE)
aov.Pax_migt_out.Tuk


### environmental
# mean_elev
mean_elev.lm <- lm(mean_elev ~ cluster, data=dat_prov)
mean_elev.aov <- aov(mean_elev.lm)
aov.mean_elev.Tuk <- HSD.test(mean_elev.aov, "cluster", group = TRUE)
aov.mean_elev.Tuk


### human additional
# dist_border
dist_border.lm <- lm(dist_border ~ cluster, data=dat_prov)
dist_border.aov <- aov(dist_border.lm)
aov.dist_border.Tuk <- HSD.test(dist_border.aov, "cluster", group = TRUE)
aov.dist_border.Tuk

# dist_provCap
dist_provCap.lm <- lm(dist_provCap ~ cluster, data=dat_prov)
dist_provCap.aov <- aov(dist_provCap.lm)
aov.dist_provCap.Tuk <- HSD.test(dist_provCap.aov, "cluster", group = TRUE)
aov.dist_provCap.Tuk

# elc
elc.lm <- lm(elc ~ cluster, data=dat_prov)
elc.aov <- aov(elc.lm)
aov.elc.Tuk <- HSD.test(elc.aov, "cluster", group = TRUE)
aov.elc.Tuk
## Doing this for ELC presence is not really meaningful. Either a province has ELCs or it does not, and so when averaging across clusters you end up with ELC presence values of 0.5, which are meaningless. The only way to do it would be to say that if any province in a cluster has an ELC then the value should be 1 for the whole cluster, but I am not sure that is appropriate seeing as some provinces are not spatially contiguous within a cluster.  It just doesn't feel right, so I will leave it out.

# PA
PA.lm <- lm(PA ~ cluster, data=dat_prov)
PA.aov <- aov(PA.lm)
aov.PA.Tuk <- HSD.test(PA.aov, "cluster", group = TRUE)
aov.PA.Tuk
# as above - doesn't really make sense


### forest cover and loss
# ForPix
ForPix.lm <- lm(ForPix ~ cluster, data=dat_prov)
ForPix.aov <- aov(ForPix.lm)
aov.ForPix.Tuk <- HSD.test(ForPix.aov, "cluster", group = TRUE)
aov.ForPix.Tuk

# diffPix
diffPix.lm <- lm(diffPix ~ cluster, data=dat_prov)
diffPix.aov <- aov(diffPix.lm)
aov.diffPix.Tuk <- HSD.test(diffPix.aov, "cluster", group = TRUE)
aov.diffPix.Tuk




  ## plots ####
    # maps ####

# load in updated Province shapefile
prov.shp <- st_read('Spatial_data/province_cluster.shp')
prov.shp$cluster <- as.factor(prov.shp$cluster)
prov.shp$agglo_clus <- as.factor(prov.shp$agglo_clus)

# plot using ggplot
ggplot(prov.shp)+
  geom_sf()

# plot map based on k-means cluster
kmean.map <- ggplot(prov.shp,aes(group=cluster, fill=cluster))+
              geom_sf()+
              scale_fill_brewer(palette = "Set1")+
              theme(panel.background = element_blank(),
                    legend.key.size = unit(1,'cm'),
                    legend.title = element_text(size=15),
                    legend.text = element_text(size = 15))+
              labs(fill = "K-means Clusters")

ggsave("Results/Cluster_analysis/k_means/kmean_map.png", kmean.map, height = 20, width = 20, units = "cm",
       dpi=300)


# plot map based on UPGMA cluster
cols <- c("tomato3","skyblue2","deepskyblue4","goldenrod","darkolivegreen3")

upgma.map <- ggplot(prov.shp,aes(group=agglo_clus, fill=agglo_clus))+
  geom_sf()+
  scale_fill_manual(values = cols)+
  theme(panel.background = element_blank(),
        legend.key.size = unit(1,'cm'),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15))+
  labs(fill = "UPGMA Clusters")

ggsave("Results/Cluster_analysis/final_UPGMA_plots/UPGMA_map.png", upgma.map, height = 20, width = 20, units = "cm",
       dpi=300)


      # add k-means and UPGMA clusters to shapefile (don't repeat) ####

## NO need to do the below each time 


### add k-means clusters (using raw shapefile)

# load in province shapefile
khet <- st_read('Spatial_data/boundary_khet.shp')
str(khet)

# remove PP & Tonle Sap
khet <- khet %>% filter(KHETTRN != "Phnom Penh")
khet <- khet %>% filter(KHETTRN != "Tonle Sap")
nrow(khet)

# standardise Province names
khet[1,4] <- "Ratanak Kiri"
khet[3,4] <- "Otdar Meanchey"
khet[8,4] <- "Mondul Kiri"
khet[9,4] <- "Kracheh"
khet[19,4] <- "Preah Sihanouk"
khet[21,4] <- "Pailin"

# add cluster
khet$cluster <- dat_prov$cluster[match(khet$KHETTRN,dat_prov$Province)]
khet$KHETTRN

# check
test.df <- data.frame(prov_shp = khet$KHETTRN,
                      shp_cluster = as.vector(khet$cluster),
                      prov_dat = dat_prov$Province,
                      dat_cluster = dat_prov$cluster,
                      prov_old = prov.shp$KHETTRN,
                      old_cluster = prov.shp$cluster)

# save shapefile
st_write(khet, "Spatial_data/province_cluster.shp", append = FALSE)



### add UPGMA cluster (using shapefile that already has the k-means clusters)

# make cluster vectors
clus1 <- c("Mondul Kiri", "Ratanak Kiri")
clus2 <- "Pailin"
clus3 <- c("Kandal", "Takeo", "Kampong Cham", "Prey Veng")
clus4 <- c("Banteay Meanchey", "Battambang")
clus5 <- c("Koh Kong","Kracheh","Otdar Meanchey","Preah Vihear","Stung Treng","Preah Sihanouk",
           "Kampong Chhnang","Pursat","Kampong Speu","Kampong Thom","Siem Reap","Kep","Kampot","Svay Rieng")

# add UPGMA clusters
prov.shp$agglo.clus <- ifelse(prov.shp$KHETTRN %in% clus1, "1",
                              ifelse(prov.shp$KHETTRN %in% clus2, "2",
                                     ifelse(prov.shp$KHETTRN %in% clus3, "3",
                                            ifelse(prov.shp$KHETTRN %in% clus4, "4", 
                                                   ifelse(prov.shp$KHETTRN %in% clus5, "5", NA)))))

prov.shp$agglo.clus <- as.factor(prov.shp$agglo.clus)

st_write(prov.shp, "Spatial_data/province_cluster.shp", append = FALSE)


    # variable plots ####

## plot the variable means for each cluster

# remove the columns I don't want to mean
#dat_prov_clus <- dat_prov %>% select(!c(year,diffPix,Province))

# group by cluster and calculate cluster means
#dat_prov_clus <- dat_prov_clus %>% group_by(cluster) %>% 
                  #summarise_all(mean)

#dat_prov_clus$cluster <- as.factor(dat_prov_clus$cluster)
#dat_prov_clus <- dat_prov_clus %>% rename(Cluster=cluster)

dat_prov$cluster <- as.factor(dat_prov$cluster)

# plot map based on k-means cluster
kmean.map <- ggplot(prov.shp,aes(group=cluster, fill=cluster))+
  geom_sf()+
  scale_fill_brewer(palette = "Set1")+
  theme(panel.background = element_blank(),
        legend.key.size = unit(0.5,'cm'),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15))+
  labs(fill = "Clusters \n(k-means)")



      # areaKM & ForPix ####

ggplot(dat_prov, aes(x=cluster, y=areaKM, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab(expression(Province~area~(km^{-2})))+
  xlab("Cluster (K-means)")
# provinces within clusters tend to have similar areas. The exception is cluster 3 (but it is one of the largest clusters)

# ForPix
ggplot(dat_prov, aes(x=cluster, y=ForPix, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Forest pixels")+
  xlab("Cluster (K-means)")


      # population demographics ####

# tot_pop
tot_pop_plot <- ggplot(dat_prov, aes(x=cluster, y=tot_pop, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Total population")+
  xlab("Cluster (K-means)")+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$tot_pop[dat_prov$cluster==1])+70000, label="c")+
  annotate("text", x=2, y=max(dat_prov$tot_pop[dat_prov$cluster==2])+70000, label="c")+
  annotate("text", x=3, y=max(dat_prov$tot_pop[dat_prov$cluster==3])+70000, label="b")+
  annotate("text", x=4, y=max(dat_prov$tot_pop[dat_prov$cluster==4])+70000, label="c")+
  annotate("text", x=5, y=max(dat_prov$tot_pop[dat_prov$cluster==5])+70000, label="ab")+
  annotate("text", x=6, y=max(dat_prov$tot_pop[dat_prov$cluster==6])+70000, label="b")+
  annotate("text", x=7, y=max(dat_prov$tot_pop[dat_prov$cluster==7])+70000, label="c")+
  annotate("text", x=8, y=max(dat_prov$tot_pop[dat_prov$cluster==8])+70000, label="a")+
  annotate("text", x=9, y=max(dat_prov$tot_pop[dat_prov$cluster==9])+70000, label="bc")

# prop_ind
prop_ind_plot <- ggplot(dat_prov, aes(x=cluster, y=prop_ind, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Proportion indigenous")+
  xlab("Cluster (K-means)")+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$prop_ind[dat_prov$cluster==1])+0.05, label="b")+
  annotate("text", x=2, y=max(dat_prov$prop_ind[dat_prov$cluster==2])+0.05, label="a")+
  annotate("text", x=3, y=max(dat_prov$prop_ind[dat_prov$cluster==3])+0.05, label="b")+
  annotate("text", x=4, y=max(dat_prov$prop_ind[dat_prov$cluster==4])+0.05, label="b")+
  annotate("text", x=5, y=max(dat_prov$prop_ind[dat_prov$cluster==5])+0.05, label="b")+
  annotate("text", x=6, y=max(dat_prov$prop_ind[dat_prov$cluster==6])+0.05, label="b")+
  annotate("text", x=7, y=max(dat_prov$prop_ind[dat_prov$cluster==7])+0.05, label="b")+
  annotate("text", x=8, y=max(dat_prov$prop_ind[dat_prov$cluster==8])+0.05, label="b")+
  annotate("text", x=9, y=max(dat_prov$prop_ind[dat_prov$cluster==9])+0.05, label="b")

# pop_den
pop_den_plot <- ggplot(dat_prov, aes(x=cluster, y=pop_den, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Population density")+
  xlab("Cluster (K-means)")+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$pop_den[dat_prov$cluster==1])+30, label="b")+
  annotate("text", x=2, y=max(dat_prov$pop_den[dat_prov$cluster==2])+30, label="b")+
  annotate("text", x=3, y=max(dat_prov$pop_den[dat_prov$cluster==3])+30, label="ab")+
  annotate("text", x=4, y=max(dat_prov$pop_den[dat_prov$cluster==4])+30, label="b")+
  annotate("text", x=5, y=max(dat_prov$pop_den[dat_prov$cluster==5])+30, label="a")+
  annotate("text", x=6, y=max(dat_prov$pop_den[dat_prov$cluster==6])+30, label="ab")+
  annotate("text", x=7, y=max(dat_prov$pop_den[dat_prov$cluster==7])+30, label="ab")+
  annotate("text", x=8, y=max(dat_prov$pop_den[dat_prov$cluster==8])+30, label="ab")+
  annotate("text", x=9, y=max(dat_prov$pop_den[dat_prov$cluster==9])+30, label="ab")

# grouped plot - map and population demographic plots
pop_demog_plot <- (kmean.map + tot_pop_plot)  / (prop_ind_plot + pop_den_plot)
# the more rural provinces have lower total population, and thsoe surrounding PP and tonle sap are higher. ONly MDK and RTK have signficantly different prop_ind - much higher than anywhere else. the other "remote" cluster (Stung Treng, Preah Vihear etc) have some variation of prop_ind, but not sig different from the others. The cluster around PP has sig differnet pop_den compared to all other clusters which is expected. clusters 1 and 2 (MDK/RTK + ST/PVH etc) plus the Pailin cluster are in their own group of low pop_den. The rest of the clusters sit in the middle

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/pop_dem_plot.png",pop_demog_plot,
       width = 20, height = 20, unit="cm", dpi=300)


      # education ####

# M6_24_sch
M6_24_sch_plot <- ggplot(dat_prov, aes(x=cluster, y=M6_24_sch, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Proportion of males (aged 6-24) in education")+
  xlab("Cluster (K-means)")+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$M6_24_sch[dat_prov$cluster==1])+0.01, label="ab")+
  annotate("text", x=2, y=max(dat_prov$M6_24_sch[dat_prov$cluster==2])+0.01, label="b")+
  annotate("text", x=3, y=max(dat_prov$M6_24_sch[dat_prov$cluster==3])+0.01, label="ab")+
  annotate("text", x=4, y=max(dat_prov$M6_24_sch[dat_prov$cluster==4])+0.01, label="ab")+
  annotate("text", x=5, y=max(dat_prov$M6_24_sch[dat_prov$cluster==5])+0.01, label="a")+
  annotate("text", x=6, y=max(dat_prov$M6_24_sch[dat_prov$cluster==6])+0.01, label="ab")+
  annotate("text", x=7, y=max(dat_prov$M6_24_sch[dat_prov$cluster==7])+0.01, label="a")+
  annotate("text", x=8, y=max(dat_prov$M6_24_sch[dat_prov$cluster==8])+0.01, label="a")+
  annotate("text", x=9, y=max(dat_prov$M6_24_sch[dat_prov$cluster==9])+0.01, label="a")

# grouped plot - map and plots
edu_plot <- kmean.map + M6_24_sch_plot
# cluster 2 (MDK/RTK) is in it's own low group, and the clusters around PP plus kep and kampot are in a high group. clusters 6 (around TS), 4 (Pailin), 3 (western 2), and 1 (PVH/ST etc) are in the middle

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/edu_plot.png",edu_plot,
       width = 20, height = 20, unit="cm", dpi=300)

      # employment ####

# propPrimSec
propPrimSec_plot <- ggplot(dat_prov, aes(x=cluster, y=propPrimSec, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Primary sector")+
  xlab("Cluster (K-means)")+
  ylim(0.25,1.2)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$propPrimSec[dat_prov$cluster==1])+0.1, label="a")+
  annotate("text", x=2, y=max(dat_prov$propPrimSec[dat_prov$cluster==2])+0.1, label="a")+
  annotate("text", x=3, y=max(dat_prov$propPrimSec[dat_prov$cluster==3])+0.1, label="a")+
  annotate("text", x=4, y=max(dat_prov$propPrimSec[dat_prov$cluster==4])+0.1, label="a")+
  annotate("text", x=5, y=max(dat_prov$propPrimSec[dat_prov$cluster==5])+0.1, label="a")+
  annotate("text", x=6, y=max(dat_prov$propPrimSec[dat_prov$cluster==6])+0.1, label="a")+
  annotate("text", x=7, y=max(dat_prov$propPrimSec[dat_prov$cluster==7])+0.1, label="a")+
  annotate("text", x=8, y=max(dat_prov$propPrimSec[dat_prov$cluster==8])+0.1, label="a")+
  annotate("text", x=9, y=max(dat_prov$propPrimSec[dat_prov$cluster==9])+0.1, label="a")

# propSecSec
propSecSec_plot <- ggplot(dat_prov, aes(x=cluster, y=propSecSec, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Secondary sector")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$propSecSec[dat_prov$cluster==1])+0.003, label="b")+
  annotate("text", x=2, y=max(dat_prov$propSecSec[dat_prov$cluster==2])+0.003, label="b")+
  annotate("text", x=3, y=max(dat_prov$propSecSec[dat_prov$cluster==3])+0.003, label="b")+
  annotate("text", x=4, y=max(dat_prov$propSecSec[dat_prov$cluster==4])+0.003, label="b")+
  annotate("text", x=5, y=max(dat_prov$propSecSec[dat_prov$cluster==5])+0.003, label="a")+
  annotate("text", x=6, y=max(dat_prov$propSecSec[dat_prov$cluster==6])+0.003, label="b")+
  annotate("text", x=7, y=max(dat_prov$propSecSec[dat_prov$cluster==7])+0.003, label="b")+
  annotate("text", x=8, y=max(dat_prov$propSecSec[dat_prov$cluster==8])+0.003, label="b")+
  annotate("text", x=9, y=max(dat_prov$propSecSec[dat_prov$cluster==9])+0.003, label="b")

# grouped plot - map and plots
emp_plot <- kmean.map | propPrimSec_plot / propSecSec_plot
# There is no sig difference between any of the clusters for propPrimSec. There are only two groups for prpoSecSec - cluster 5 (around PP) which is relatively high propSecSec, and then the rest are low. But all values of propSecSec are super low

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/emp_plot.png",emp_plot,
       width = 20, height = 20, unit="cm", dpi=300)



      # economic security ####

# Les1_R_Land
Les1_R_Land_plot <- ggplot(dat_prov, aes(x=cluster, y=Les1_R_Land, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Proportion with no farm land")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==1])+0.03, label="b")+
  annotate("text", x=2, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==2])+0.03, label="b")+
  annotate("text", x=3, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==3])+0.03, label="ab")+
  annotate("text", x=4, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==4])+0.03, label="b")+
  annotate("text", x=5, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==5])+0.03, label="a")+
  annotate("text", x=6, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==6])+0.03, label="ab")+
  annotate("text", x=7, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==7])+0.03, label="ab")+
  annotate("text", x=8, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==8])+0.03, label="ab")+
  annotate("text", x=9, y=max(dat_prov$Les1_R_Land[dat_prov$cluster==9])+0.03, label="ab")

# pig_fam
pig_fam_plot <- ggplot(dat_prov, aes(x=cluster, y=pig_fam, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Proportion with pigs")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$pig_fam[dat_prov$cluster==1])+0.04, label="a")+
  annotate("text", x=2, y=max(dat_prov$pig_fam[dat_prov$cluster==2])+0.04, label="a")+
  annotate("text", x=3, y=max(dat_prov$pig_fam[dat_prov$cluster==3])+0.04, label="a")+
  annotate("text", x=4, y=max(dat_prov$pig_fam[dat_prov$cluster==4])+0.04, label="a")+
  annotate("text", x=5, y=max(dat_prov$pig_fam[dat_prov$cluster==5])+0.04, label="a")+
  annotate("text", x=6, y=max(dat_prov$pig_fam[dat_prov$cluster==6])+0.04, label="a")+
  annotate("text", x=7, y=max(dat_prov$pig_fam[dat_prov$cluster==7])+0.04, label="a")+
  annotate("text", x=8, y=max(dat_prov$pig_fam[dat_prov$cluster==8])+0.04, label="a")+
  annotate("text", x=9, y=max(dat_prov$pig_fam[dat_prov$cluster==9])+0.04, label="a")

# grouped plot - map and plots
econ_plot <- kmean.map | Les1_R_Land_plot / pig_fam_plot
# two distinct groups for rice land - very low Les1_R_Land for cluster 1 (PVH/ST etc), cluster 2 (MDK/RTK), cluster 3 (western 2), and pailin. This means that in these provinces, very few people have no farm land. This is expected as they are very rural and there is still lots of land. Cluster 5 (around PP) is in its own group and has high Les1_R_Land values (i.e. is very urbanised). Cluster 6-9 are also very high, but sit in a middle group. Interestingly, there is no sig difference in pig_fam. 

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/econ_plot.png",econ_plot,
       width = 20, height = 20, unit="cm", dpi=300)

      # access to services ####

# dist_sch
dist_sch_plot <- ggplot(dat_prov, aes(x=cluster, y=dist_sch, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Distance to school (KM)")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$dist_sch[dat_prov$cluster==1])+1, label="b")+
  annotate("text", x=2, y=max(dat_prov$dist_sch[dat_prov$cluster==2])+1, label="a")+
  annotate("text", x=3, y=max(dat_prov$dist_sch[dat_prov$cluster==3])+1, label="c")+
  annotate("text", x=4, y=max(dat_prov$dist_sch[dat_prov$cluster==4])+1, label="c")+
  annotate("text", x=5, y=max(dat_prov$dist_sch[dat_prov$cluster==5])+1, label="c")+
  annotate("text", x=6, y=max(dat_prov$dist_sch[dat_prov$cluster==6])+1, label="c")+
  annotate("text", x=7, y=max(dat_prov$dist_sch[dat_prov$cluster==7])+1, label="c")+
  annotate("text", x=8, y=max(dat_prov$dist_sch[dat_prov$cluster==8])+1, label="c")+
  annotate("text", x=9, y=max(dat_prov$dist_sch[dat_prov$cluster==9])+1, label="c")

# garbage
garbage_plot <- ggplot(dat_prov, aes(x=cluster, y=garbage, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Poportion with access to waste disposal")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$garbage[dat_prov$cluster==1])+0.005, label="a")+
  annotate("text", x=2, y=max(dat_prov$garbage[dat_prov$cluster==2])+0.005, label="a")+
  annotate("text", x=3, y=max(dat_prov$garbage[dat_prov$cluster==3])+0.005, label="a")+
  annotate("text", x=4, y=max(dat_prov$garbage[dat_prov$cluster==4])+0.005, label="a")+
  annotate("text", x=5, y=max(dat_prov$garbage[dat_prov$cluster==5])+0.005, label="a")+
  annotate("text", x=6, y=max(dat_prov$garbage[dat_prov$cluster==6])+0.005, label="a")+
  annotate("text", x=7, y=max(dat_prov$garbage[dat_prov$cluster==7])+0.005, label="a")+
  annotate("text", x=8, y=max(dat_prov$garbage[dat_prov$cluster==8])+0.005, label="a")+
  annotate("text", x=9, y=max(dat_prov$garbage[dat_prov$cluster==9])+0.005, label="a")

# KM_Comm
KM_Comm_plot <- ggplot(dat_prov, aes(x=cluster, y=KM_Comm, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Mean distance to Commune office")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$KM_Comm[dat_prov$cluster==1])+0.5, label="ab")+
  annotate("text", x=2, y=max(dat_prov$KM_Comm[dat_prov$cluster==2])+0.5, label="a")+
  annotate("text", x=3, y=max(dat_prov$KM_Comm[dat_prov$cluster==3])+0.5, label="ab")+
  annotate("text", x=4, y=max(dat_prov$KM_Comm[dat_prov$cluster==4])+0.5, label="ab")+
  annotate("text", x=5, y=max(dat_prov$KM_Comm[dat_prov$cluster==5])+0.5, label="b")+
  annotate("text", x=6, y=max(dat_prov$KM_Comm[dat_prov$cluster==6])+0.5, label="ab")+
  annotate("text", x=7, y=max(dat_prov$KM_Comm[dat_prov$cluster==7])+0.5, label="b")+
  annotate("text", x=8, y=max(dat_prov$KM_Comm[dat_prov$cluster==8])+0.5, label="ab")+
  annotate("text", x=9, y=max(dat_prov$KM_Comm[dat_prov$cluster==9])+0.5, label="b")

# grouped plot - map and plots
acc_serv_plot <- (kmean.map + dist_sch_plot)  / (garbage_plot + KM_Comm_plot)
# clusters 1 and 2 have sig different (larger) distances to school, and all the other clusters are the same. No sig difference between clusters for access to waste disposal. cluster 2 has sig difference (higher) distance to KM office. South east clusters have their own low group, and then all the others are in between. 


ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/acc_serv_plot.png",acc_serv_plot,
       width = 20, height = 20, unit="cm", dpi=300)

      # social justice ####

# crim_case
crim_case_plot <- ggplot(dat_prov, aes(x=cluster, y=crim_case, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Criminal cases per capita")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$crim_case[dat_prov$cluster==1])+0.001, label="b")+
  annotate("text", x=2, y=max(dat_prov$crim_case[dat_prov$cluster==2])+0.001, label="b")+
  annotate("text", x=3, y=max(dat_prov$crim_case[dat_prov$cluster==3])+0.001, label="b")+
  annotate("text", x=4, y=max(dat_prov$crim_case[dat_prov$cluster==4])+0.001, label="a")+
  annotate("text", x=5, y=max(dat_prov$crim_case[dat_prov$cluster==5])+0.001, label="b")+
  annotate("text", x=6, y=max(dat_prov$crim_case[dat_prov$cluster==6])+0.001, label="b")+
  annotate("text", x=7, y=max(dat_prov$crim_case[dat_prov$cluster==7])+0.001, label="b")+
  annotate("text", x=8, y=max(dat_prov$crim_case[dat_prov$cluster==8])+0.001, label="b")+
  annotate("text", x=9, y=max(dat_prov$crim_case[dat_prov$cluster==9])+0.001, label="b")

# land_confl
land_confl_plot <- ggplot(dat_prov, aes(x=cluster, y=land_confl, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Land conflict cases")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$land_confl[dat_prov$cluster==1])+200, label="d")+
  annotate("text", x=2, y=max(dat_prov$land_confl[dat_prov$cluster==2])+200, label="d")+
  annotate("text", x=3, y=max(dat_prov$land_confl[dat_prov$cluster==3])+200, label="bcd")+
  annotate("text", x=4, y=max(dat_prov$land_confl[dat_prov$cluster==4])+200, label="d")+
  annotate("text", x=5, y=max(dat_prov$land_confl[dat_prov$cluster==5])+200, label="ab")+
  annotate("text", x=6, y=max(dat_prov$land_confl[dat_prov$cluster==6])+200, label="bc")+
  annotate("text", x=7, y=max(dat_prov$land_confl[dat_prov$cluster==7])+200, label="d")+
  annotate("text", x=8, y=max(dat_prov$land_confl[dat_prov$cluster==8])+200, label="a")+
  annotate("text", x=9, y=max(dat_prov$land_confl[dat_prov$cluster==9])+200, label="cd")

# grouped plot - map and plots
soc_jus_plot <- kmean.map | crim_case_plot / land_confl_plot
# no sig diff between crim cases between any cluster excpet 4 (pailin) which has significantly higher crim cases than any other cluster. land confl is more complex - south east clusters have sig higher number of conflict cases, followed by central tonle sap cluster, then western cluster, south coast, and then all the rural clusters are together with very few. 

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/soc_jus_plot.png",soc_jus_plot,
       width = 20, height = 20, unit="cm", dpi=300)

      # Migration ####

# Pax_migt_in
Pax_migt_in_plot <- ggplot(dat_prov, aes(x=cluster, y=Pax_migt_in, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Number of in-migrants")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==1])+2000, label="c")+
  annotate("text", x=2, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==2])+2000, label="c")+
  annotate("text", x=3, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==3])+2000, label="b")+
  annotate("text", x=4, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==4])+2000, label="c")+
  annotate("text", x=5, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==5])+2000, label="b")+
  annotate("text", x=6, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==6])+2000, label="bc")+
  annotate("text", x=7, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==7])+2000, label="c")+
  annotate("text", x=8, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==8])+2000, label="a")+
  annotate("text", x=9, y=max(dat_prov$Pax_migt_in[dat_prov$cluster==9])+2000, label="b")

# Pax_migt_out
Pax_migt_out_plot <- ggplot(dat_prov, aes(x=cluster, y=Pax_migt_out, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Number of out-migrants")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==1])+2000, label="c")+
  annotate("text", x=2, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==2])+2000, label="c")+
  annotate("text", x=3, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==3])+2000, label="a")+
  annotate("text", x=4, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==4])+2000, label="c")+
  annotate("text", x=5, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==5])+2000, label="bc")+
  annotate("text", x=6, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==6])+2000, label="bc")+
  annotate("text", x=7, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==7])+2000, label="c")+
  annotate("text", x=8, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==8])+2000, label="ab")+
  annotate("text", x=9, y=max(dat_prov$Pax_migt_out[dat_prov$cluster==9])+2000, label="c")


# grouped plot - map and plots
mig_plot <- kmean.map | Pax_migt_in_plot / Pax_migt_out_plot
# clusters around PP have lots of in-migs, whicih makes sense, but so does the western cluster which I don't have a reason for. Remote and coastal clusters have low in-mig.  The western cluster has massive out-mig, which I also don't have a reason for. All the rest have quite low out-mig, although clusters around PP and TS are a bit higher 

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/mig_plot.png",mig_plot,
       width = 20, height = 20, unit="cm", dpi=300)


      # Environmental ####

# mean_elev
mean_elev_plot <- ggplot(dat_prov, aes(x=cluster, y=mean_elev, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Mean elevation (m)")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$mean_elev[dat_prov$cluster==1])+20, label="b")+
  annotate("text", x=2, y=max(dat_prov$mean_elev[dat_prov$cluster==2])+20, label="a")+
  annotate("text", x=3, y=max(dat_prov$mean_elev[dat_prov$cluster==3])+20, label="b")+
  annotate("text", x=4, y=max(dat_prov$mean_elev[dat_prov$cluster==4])+20, label="a")+
  annotate("text", x=5, y=max(dat_prov$mean_elev[dat_prov$cluster==5])+20, label="b")+
  annotate("text", x=6, y=max(dat_prov$mean_elev[dat_prov$cluster==6])+20, label="b")+
  annotate("text", x=7, y=max(dat_prov$mean_elev[dat_prov$cluster==7])+20, label="b")+
  annotate("text", x=8, y=max(dat_prov$mean_elev[dat_prov$cluster==8])+20, label="b")+
  annotate("text", x=9, y=max(dat_prov$mean_elev[dat_prov$cluster==9])+20, label="b")

# grouped plot - map and plots
env_plot <- kmean.map + mean_elev_plot
# Cluster 2 (MDK/RTK) and Palin sig different (higher) than all the others. Koh kong obvioulsy being dragged down by all the other provs in the cluster

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/env_plot.png",env_plot,
       width = 20, height = 20, unit="cm", dpi=300)

      # Human additional ####

# dist_border
dist_border_plot <- ggplot(dat_prov, aes(x=cluster, y=dist_border, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Distance to international border (km)")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$dist_border[dat_prov$cluster==1])+20, label="b")+
  annotate("text", x=2, y=max(dat_prov$dist_border[dat_prov$cluster==2])+20, label="b")+
  annotate("text", x=3, y=max(dat_prov$dist_border[dat_prov$cluster==3])+20, label="b")+
  annotate("text", x=4, y=max(dat_prov$dist_border[dat_prov$cluster==4])+20, label="b")+
  annotate("text", x=5, y=max(dat_prov$dist_border[dat_prov$cluster==5])+20, label="b")+
  annotate("text", x=6, y=max(dat_prov$dist_border[dat_prov$cluster==6])+20, label="a")+
  annotate("text", x=7, y=max(dat_prov$dist_border[dat_prov$cluster==7])+20, label="b")+
  annotate("text", x=8, y=max(dat_prov$dist_border[dat_prov$cluster==8])+20, label="b")+
  annotate("text", x=9, y=max(dat_prov$dist_border[dat_prov$cluster==9])+20, label="b")

# dist_provCap
dist_provCap_plot <- ggplot(dat_prov, aes(x=cluster, y=dist_provCap, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Distance to provincial capital (km)")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$dist_provCap[dat_prov$cluster==1])+5, label="a")+
  annotate("text", x=2, y=max(dat_prov$dist_provCap[dat_prov$cluster==2])+5, label="ab")+
  annotate("text", x=3, y=max(dat_prov$dist_provCap[dat_prov$cluster==3])+5, label="ab")+
  annotate("text", x=4, y=max(dat_prov$dist_provCap[dat_prov$cluster==4])+5, label="b")+
  annotate("text", x=5, y=max(dat_prov$dist_provCap[dat_prov$cluster==5])+5, label="ab")+
  annotate("text", x=6, y=max(dat_prov$dist_provCap[dat_prov$cluster==6])+5, label="ab")+
  annotate("text", x=7, y=max(dat_prov$dist_provCap[dat_prov$cluster==7])+5, label="b")+
  annotate("text", x=8, y=max(dat_prov$dist_provCap[dat_prov$cluster==8])+5, label="ab")+
  annotate("text", x=9, y=max(dat_prov$dist_provCap[dat_prov$cluster==9])+5, label="ab")

# elc
elc_plot <- ggplot(dat_prov, aes(x=cluster, y=elc, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("ELC presence")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$elc[dat_prov$cluster==1])+0.1, label="a")+
  annotate("text", x=2, y=max(dat_prov$elc[dat_prov$cluster==2])+0.1, label="a")+
  annotate("text", x=3, y=max(dat_prov$elc[dat_prov$cluster==3])+0.1, label="a")+
  annotate("text", x=4, y=max(dat_prov$elc[dat_prov$cluster==4])+0.1, label="a")+
  annotate("text", x=5, y=max(dat_prov$elc[dat_prov$cluster==5])+0.1, label="a")+
  annotate("text", x=6, y=max(dat_prov$elc[dat_prov$cluster==6])+0.1, label="a")+
  annotate("text", x=7, y=max(dat_prov$elc[dat_prov$cluster==7])+0.1, label="a")+
  annotate("text", x=8, y=max(dat_prov$elc[dat_prov$cluster==8])+0.1, label="a")+
  annotate("text", x=9, y=max(dat_prov$elc[dat_prov$cluster==9])+0.1, label="a")

# PA
PA_plot <- ggplot(dat_prov, aes(x=cluster, y=PA, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("PA presence")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$PA[dat_prov$cluster==1])+0.1, label="ab")+
  annotate("text", x=2, y=max(dat_prov$PA[dat_prov$cluster==2])+0.1, label="a")+
  annotate("text", x=3, y=max(dat_prov$PA[dat_prov$cluster==3])+0.1, label="ab")+
  annotate("text", x=4, y=max(dat_prov$PA[dat_prov$cluster==4])+0.1, label="b")+
  annotate("text", x=5, y=max(dat_prov$PA[dat_prov$cluster==5])+0.1, label="ab")+
  annotate("text", x=6, y=max(dat_prov$PA[dat_prov$cluster==6])+0.1, label="ab")+
  annotate("text", x=7, y=max(dat_prov$PA[dat_prov$cluster==7])+0.1, label="ab")+
  annotate("text", x=8, y=max(dat_prov$PA[dat_prov$cluster==8])+0.1, label="ab")+
  annotate("text", x=9, y=max(dat_prov$PA[dat_prov$cluster==9])+0.1, label="ab")

# grouped plot - map and plots
hum_plot <- kmean.map | dist_border_plot / dist_provCap_plot
# The distance to border makes sense - cluster 6 s all the interior prvinces, and they are sig diff from all the rest. Distance to provCap is mostly about the size of the provinces in the clusters I think. cluster 1 is the largest provinces and is sig diff from custers 4 and 7 which are the smallest. all the others sit in the middle. 

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/hum_plot.png",hum_plot,
       width = 20, height = 20, unit="cm", dpi=300)


      # Forest cover and loss ####

# ForPix
ForPix_plot <- ggplot(dat_prov, aes(x=cluster, y=ForPix, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Forest cover (pixels)")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$ForPix[dat_prov$cluster==1])+10000, label="a")+
  annotate("text", x=2, y=max(dat_prov$ForPix[dat_prov$cluster==2])+10000, label="a")+
  annotate("text", x=3, y=max(dat_prov$ForPix[dat_prov$cluster==3])+10000, label="ab")+
  annotate("text", x=4, y=max(dat_prov$ForPix[dat_prov$cluster==4])+10000, label="ab")+
  annotate("text", x=5, y=max(dat_prov$ForPix[dat_prov$cluster==5])+10000, label="ab")+
  annotate("text", x=6, y=max(dat_prov$ForPix[dat_prov$cluster==6])+10000, label="ab")+
  annotate("text", x=7, y=max(dat_prov$ForPix[dat_prov$cluster==7])+10000, label="ab")+
  annotate("text", x=8, y=max(dat_prov$ForPix[dat_prov$cluster==8])+10000, label="ab")+
  annotate("text", x=9, y=max(dat_prov$ForPix[dat_prov$cluster==9])+10000, label="ab")

# diffPix
diffPix_plot <- ggplot(dat_prov, aes(x=cluster, y=diffPix, group=cluster, fill=cluster))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()+
  ylab("Forest cover loss (pixels)")+
  xlab("Cluster (K-means)")+
  #ylim(0,1)+
  theme(legend.position = "none")+
  annotate("text", x=1, y=max(dat_prov$diffPix[dat_prov$cluster==1])+150, label="a")+
  annotate("text", x=2, y=max(dat_prov$diffPix[dat_prov$cluster==2])+150, label="a")+
  annotate("text", x=3, y=max(dat_prov$diffPix[dat_prov$cluster==3])+150, label="a")+
  annotate("text", x=4, y=max(dat_prov$diffPix[dat_prov$cluster==4])+150, label="a")+
  annotate("text", x=5, y=max(dat_prov$diffPix[dat_prov$cluster==5])+150, label="a")+
  annotate("text", x=6, y=max(dat_prov$diffPix[dat_prov$cluster==6])+150, label="a")+
  annotate("text", x=7, y=max(dat_prov$diffPix[dat_prov$cluster==7])+150, label="a")+
  annotate("text", x=8, y=max(dat_prov$diffPix[dat_prov$cluster==8])+150, label="a")+
  annotate("text", x=9, y=max(dat_prov$diffPix[dat_prov$cluster==9])+150, label="a")


# grouped plot - map and plots
for_plot <- kmean.map | ForPix_plot / diffPix_plot

ggsave("Results/Cluster_analysis/k_means/Variable_Tukey_Plots/for_plot.png",for_plot,
       width = 20, height = 20, unit="cm", dpi=300)


  ## Describing clusters ####
      # cluster table ####

# I want to save a table that shows the clusters and the provinces
provClus <- dat_prov[ ,c("Province","cluster")]
prov.Clus <- provClus %>% mutate(row=row_number()) %>% 
              pivot_wider(names_from = cluster, values_from = Province) 
write.csv(prov.Clus, file="Results/Cluster_analysis/k_means/provinces_clusters.csv")

      # heatmap (k-means) - continuous ####


# remove "x" columns and year, province, tot_pop, diffPix
dat_prov <- dat_prov %>% select(!c(year, Province, tot_pop, diffPix))

# remove agglo.clus
dat_prov <- dat_prov[, -21] 

# now I need to scale the vars
dat_prov.s <- dat_prov %>% mutate_at(c("land_confl", "Pax_migt_in","Pax_migt_out","prop_ind", 
                             "pop_den", "M6_24_sch", "propPrimSec", "propSecSec","Les1_R_Land", 
                             "pig_fam", "dist_sch", "garbage", "KM_Comm", "crim_case","ForPix",
                             "areaKM","dist_provCap","dist_border","mean_elev"),
                             ~(scale(.) %>% as.vector))

# Mean the variables for clusters
dat_prov.m <- dat_prov.s %>% group_by(cluster) %>% summarise_all(mean)

# re-order vars
dat_prov.m <- dat_prov.m %>% select(cluster,
                                    mean_elev,areaKM,dist_provCap,dist_border,ForPix,
                                    pop_den,prop_ind,
                                    M6_24_sch,
                                    propPrimSec,propSecSec,
                                    Les1_R_Land,pig_fam,
                                    dist_sch,garbage,KM_Comm,
                                    crim_case,land_confl,
                                    Pax_migt_in,Pax_migt_out)

# put dat_prov into long format
dat_prov.l <- melt(dat_prov.m)

# add column that groups variables into sets
#physchar <- c("mean_elev","areaKM","dist_provCap","dist_border","ForPix")
#popdem <- c("pop_den","prop_ind")
#edu <- "M6_24_sch"
#emp <- c("propPrimSec","propSecSec")
#econ <- c("Les1_R_Land","pig_fam")
#acc <- c("dist_sch","garbage","KM_Comm")
#soc <- c("crim_case","land_confl")
#mig <- c("Pax_migt_in","Pax_migt_out")

#dat_prov.l$set <- ifelse(dat_prov.l$variable %in% physchar,1,
 #                   ifelse(dat_prov.l$variable %in% popdem,2,
  #                    ifelse(dat_prov.l$variable %in% edu,3,
   #                     ifelse(dat_prov.l$variable %in% emp,4,
    #                      ifelse(dat_prov.l$variable %in% econ,5,
     #                       ifelse(dat_prov.l$variable %in% acc,6,
      #                        ifelse(dat_prov.l$variable %in% soc,7,
       #                         ifelse(dat_prov.l$variable %in% mig,8,NA))))))))  

# re-order
#dat_prov.l <- dat_prov.l %>% arrange(set)

# heatmap
ggplot(dat_prov.l, aes(x=cluster, y=variable))+
  geom_tile(aes(fill=value))+
  scale_fill_gradient(low="white", high="red")


# physical characteristics - cluster 1, 2, 4 have higher elevation, all the rest are low.  Clustes 3, 5, 6, 8 have high distance to border values, all others are low. Clusters 1, 2, 6, 8 have high distance to provincial capital values, the others are low. Clusters 1, 2, 3, 6, 8 are made up of provinces with larger areas. Clusters 1, 2, and 6 have high forest cover values.

# population demographics - clusters 1, 2, 4, 6 have low population density, and clusters 3, 5, 7, 8, 9 have high population density. Clusters 1 and 2 have high proportion of indigneous people, and all the other clusters are very low.

# education - clusters 1, 2 and 6 are low levels of education.  All other clusters are higher but Cluster 5 and 9 are highest.

# employment - clusters 1, 2, 6, 8, 9 have the highest proprtion of primary sector workers, all the rest are low.. Clusters 4 and 7 have the lowest. CLusters 5 and 4 have the highest proportino of secondary sector workers, all the rest are very low

# economic security - CLusters 1 and 2 have few people with no farm land and lots of people with pigs. clusters 3 adn 4 have low numbers of people with no farm land and low number of people with pigs. cluster 5, 6, 8 have high number of people with no farm land, but low number of people with pigs. cluster 7 and 9 have high numbers of people with no farm land but higher number of people with pigs.

# access to services - Broadly cluster 1 and 2 have higher values for distance to school, and distance to commune office. clusters 3-9 all have low distances to school. clusters 1,3 7, and 9 have higher values of access to garbage collection. clusters 3-9 all have low distances to commune office. 

# social justice - clusters 1, 2, 4, and 7 have higher values of criminal cases, with 4 being very high. clusters 3, 5, 6, 8 all ahve high values of land conflict. 

# migration - clusters 3, 5, 6, 8 all have high levels of in and out migration. cluster 9 has a bit of in-migration. Interestingly, there is a clear pattern between migraiton and land conflict - all clusters that have high migration have high land conflict. 



### I think to make the classification of the typologies less subjective I want to display the heatmap using quantiles. So put each value for each variable/cluster into either the bottom (<25%), middle-bottom (26-50%), middle-top (51-75%), or top (>50%)



      # change k-means values to categorical ####

## calculate quantiles for each variable using the range for the entire variable
# remove cluster
dat_prov.q <- dat_prov.m[,-1]

# get quantiles
quants <- as.data.frame(apply(dat_prov.q, 2, function(x){quantile(x, probs = c(0.25,0.5,0.75))}))


dat_prov.quants <- dat_prov.m %>% 
                    mutate(mean_elev = ifelse(mean_elev < quants$mean_elev[1], "v.low",
                                          ifelse(mean_elev < quants$mean_elev[2], "low",
                                              ifelse(mean_elev < quants$mean_elev[3], "high",
                                                ifelse(mean_elev >= quants$mean_elev[3], "v.high",NA))))) %>% 
                    mutate(areaKM = ifelse(areaKM < quants$areaKM[1], "v.low",
                                          ifelse(areaKM < quants$areaKM[2], "low",
                                              ifelse(areaKM < quants$areaKM[3], "high",
                                                ifelse(areaKM >= quants$areaKM[3], "v.high",NA))))) %>% 
                    mutate(dist_provCap = ifelse(dist_provCap < quants$dist_provCap[1], "v.low",
                                          ifelse(dist_provCap < quants$dist_provCap[2], "low",
                                            ifelse(dist_provCap < quants$dist_provCap[3], "high",
                                              ifelse(dist_provCap >= quants$dist_provCap[3], "v.high",NA))))) %>%
                    mutate(dist_border = ifelse(dist_border < quants$dist_border[1], "v.low",
                                            ifelse(dist_border < quants$dist_border[2], "low",
                                              ifelse(dist_border < quants$dist_border[3], "high",
                                                ifelse(dist_border >= quants$dist_border[3], "v.high",NA))))) %>%
                    mutate(ForPix = ifelse(ForPix < quants$ForPix[1], "v.low",
                                            ifelse(ForPix < quants$ForPix[2], "low",
                                              ifelse(ForPix < quants$ForPix[3], "high",
                                                ifelse(ForPix >= quants$ForPix[3], "v.high",NA))))) %>%
                    mutate(pop_den = ifelse(pop_den < quants$pop_den[1], "v.low",
                                            ifelse(pop_den < quants$pop_den[2], "low",
                                              ifelse(pop_den < quants$pop_den[3], "high",
                                                ifelse(pop_den >= quants$pop_den[3], "v.high",NA))))) %>%
                    mutate(prop_ind = ifelse(prop_ind < quants$prop_ind[1], "v.low",
                                            ifelse(prop_ind < quants$prop_ind[2], "low",
                                              ifelse(prop_ind < quants$prop_ind[3], "high",
                                                ifelse(prop_ind >= quants$prop_ind[3], "v.high",NA))))) %>%
                    mutate(M6_24_sch = ifelse(M6_24_sch < quants$M6_24_sch[1], "v.low",
                                            ifelse(M6_24_sch < quants$M6_24_sch[2], "low",
                                              ifelse(M6_24_sch < quants$M6_24_sch[3], "high",
                                                ifelse(M6_24_sch >= quants$M6_24_sch[3], "v.high",NA))))) %>%
                    mutate(propPrimSec = ifelse(propPrimSec < quants$propPrimSec[1], "v.low",
                                            ifelse(propPrimSec < quants$propPrimSec[2], "low",
                                              ifelse(propPrimSec < quants$propPrimSec[3], "high",
                                                ifelse(propPrimSec >= quants$propPrimSec[3], "v.high",NA))))) %>%
                    mutate(propSecSec = ifelse(propSecSec < quants$propSecSec[1], "v.low",
                                            ifelse(propSecSec < quants$propSecSec[2], "low",
                                              ifelse(propSecSec < quants$propSecSec[3], "high",
                                                ifelse(propSecSec >= quants$propSecSec[3], "v.high",NA))))) %>%
                    mutate(Les1_R_Land = ifelse(Les1_R_Land < quants$Les1_R_Land[1], "v.low",
                                            ifelse(Les1_R_Land < quants$Les1_R_Land[2], "low",
                                              ifelse(Les1_R_Land < quants$Les1_R_Land[3], "high",
                                                ifelse(Les1_R_Land >= quants$Les1_R_Land[3], "v.high",NA))))) %>%
                    mutate(pig_fam = ifelse(pig_fam < quants$pig_fam[1], "v.low",
                                            ifelse(pig_fam < quants$pig_fam[2], "low",
                                              ifelse(pig_fam < quants$pig_fam[3], "high",
                                                ifelse(pig_fam >= quants$pig_fam[3], "v.high",NA))))) %>%
                    mutate(dist_sch = ifelse(dist_sch < quants$dist_sch[1], "v.low",
                                            ifelse(dist_sch < quants$dist_sch[2], "low",
                                              ifelse(dist_sch < quants$dist_sch[3], "high",
                                                ifelse(dist_sch >= quants$dist_sch[3], "v.high",NA))))) %>%
                    mutate(garbage = ifelse(garbage < quants$garbage[1], "v.low",
                                            ifelse(garbage < quants$garbage[2], "low",
                                              ifelse(garbage < quants$garbage[3], "high",
                                                ifelse(garbage >= quants$garbage[3], "v.high",NA))))) %>%
                    mutate(KM_Comm = ifelse(KM_Comm < quants$KM_Comm[1], "v.low",
                                            ifelse(KM_Comm < quants$KM_Comm[2], "low",
                                              ifelse(KM_Comm < quants$KM_Comm[3], "high",
                                                ifelse(KM_Comm >= quants$KM_Comm[3], "v.high",NA))))) %>%
                    mutate(crim_case = ifelse(crim_case < quants$crim_case[1], "v.low",
                                            ifelse(crim_case < quants$crim_case[2], "low",
                                              ifelse(crim_case < quants$crim_case[3], "high",
                                                ifelse(crim_case >= quants$crim_case[3], "v.high",NA))))) %>%
                    mutate(land_confl = ifelse(land_confl < quants$land_confl[1], "v.low",
                                            ifelse(land_confl < quants$land_confl[2], "low",
                                              ifelse(land_confl < quants$land_confl[3], "high",
                                                ifelse(land_confl >= quants$land_confl[3], "v.high",NA))))) %>%
                    mutate(Pax_migt_in = ifelse(Pax_migt_in < quants$Pax_migt_in[1], "v.low",
                                            ifelse(Pax_migt_in < quants$Pax_migt_in[2], "low",
                                              ifelse(Pax_migt_in < quants$Pax_migt_in[3], "high",
                                                ifelse(Pax_migt_in >= quants$Pax_migt_in[3], "v.high",NA))))) %>%
                    mutate(Pax_migt_out = ifelse(Pax_migt_out < quants$Pax_migt_out[1], "v.low",
                                            ifelse(Pax_migt_out < quants$Pax_migt_out[2], "low",
                                              ifelse(Pax_migt_out < quants$Pax_migt_out[3], "high",
                                                ifelse(Pax_migt_out >= quants$Pax_migt_out[3], "v.high",NA))))) 



      # heatmap (k-means) - categorical  ####

# put dat_prov into long format
dat_prov.ql <- melt(dat_prov.quants, id.vars = "cluster")

# set colours
cols <- c("v.low" = "darkblue", "low" = "blue", "high" = "orange", "v.high" = "red")

# change value to factor and re-order
dat_prov.ql$value <- as.factor(dat_prov.ql$value)
dat_prov.ql$value <- factor(dat_prov.ql$value, c("v.high","high","low","v.low"))

# heatmap with the categories
kmean_heatmap_cat <- ggplot(dat_prov.ql, aes(x=cluster, y=variable))+
                      geom_tile(aes(fill=value))+
                      scale_fill_manual(values = cols)+
                      xlab("Cluster (k-means)")+
                      ylab("Variable")+
                      theme(axis.text = element_text(size=15),
                            axis.title = element_text(size=18),
                            legend.text = element_text(size=18),
                            legend.title = element_text(size=17),
                            legend.key.size = unit(2,'cm'))

ggsave("Results/Cluster_analysis/k_means/kmean_heatmap_cat.png",kmean_heatmap_cat,
       width = 30, height = 30, unit="cm", dpi=300)

      # heatmap (UPGMA) - continuous ####

## I used mean.dat for the clustering and so I need to use the same data for the heatmap. Originally, and in the k-means section above I use the raw province data, but I don't think this is correct. Firstly because the raw data has ForPix and pixDiff, which I want to exclude from the heatmap as they were excluded from the clustering. Secondly, the data displayed in the heatmap needs to match the data used for the clustering. 


# make cluster vectors
clus1 <- c("Mondul Kiri", "Ratanak Kiri")
clus2 <- "Pailin"
clus3 <- c("Kandal", "Takeo", "Kampong Cham", "Prey Veng")
clus4 <- c("Banteay Meanchey", "Battambang")
clus5 <- c("Koh Kong","Kracheh","Otdar Meanchey","Preah Vihear","Stung Treng","Preah Sihanouk",
           "Kampong Chhnang","Pursat","Kampong Speu","Kampong Thom","Siem Reap","Kep","Kampot","Svay Rieng")

# create Province column using row labels
mean.dat$Province <- row.names(mean.dat)

# add UPGMA clusters to mean.dat
mean.dat$agglo.clus <- ifelse(mean.dat$Province %in% clus1, "1",
                        ifelse(mean.dat$Province %in% clus2, "2",
                         ifelse(mean.dat$Province %in% clus3, "3",
                          ifelse(mean.dat$Province %in% clus4, "4", 
                            ifelse(mean.dat$Province %in% clus5, "5", NA)))))


# remove Province
dat_prov.s <- mean.dat %>% select(-Province)

# Mean the variables for clusters
dat_prov.m <- dat_prov.s %>% group_by(agglo.clus) %>% summarise_all(mean)

# re-order vars
dat_prov.m <- dat_prov.m %>% select(agglo.clus,
                                    pop_den,prop_ind,
                                    M6_24_sch,
                                    propPrimSec,propSecSec,
                                    Les1_R_Land,pig_fam,
                                    dist_sch,garbage,KM_Comm,
                                    crim_case,land_confl,
                                    Pax_migt_in,Pax_migt_out)

# make agglo.clus a factor
dat_prov.m$agglo.clus <- as.factor(dat_prov.m$agglo.clus)

# put dat_prov into long format
dat_prov.l <- melt(dat_prov.m)

# heatmap
ggplot(dat_prov.l, aes(x=agglo.clus, y=variable))+
  geom_tile(aes(fill=value))+
  scale_fill_gradient(low="white", high="red")

      # change UPGMA values to categorical ####

## calculate quantiles for each variable using the range for the entire variable

# remove cluster
dat_prov.q <- dat_prov.m[,-1]

# get quantiles
quants <- as.data.frame(apply(dat_prov.q, 2, function(x){quantile(x, probs = c(0.25,0.5,0.75))}))


dat_prov.quants <- dat_prov.m %>% 
  mutate(pop_den = ifelse(pop_den < quants$pop_den[1], "v.low",
                          ifelse(pop_den < quants$pop_den[2], "low",
                                 ifelse(pop_den < quants$pop_den[3], "high",
                                        ifelse(pop_den >= quants$pop_den[3], "v.high",NA))))) %>%
  mutate(prop_ind = ifelse(prop_ind < quants$prop_ind[1], "v.low",
                           ifelse(prop_ind < quants$prop_ind[2], "low",
                                  ifelse(prop_ind < quants$prop_ind[3], "high",
                                         ifelse(prop_ind >= quants$prop_ind[3], "v.high",NA))))) %>%
  mutate(M6_24_sch = ifelse(M6_24_sch < quants$M6_24_sch[1], "v.low",
                            ifelse(M6_24_sch < quants$M6_24_sch[2], "low",
                                   ifelse(M6_24_sch < quants$M6_24_sch[3], "high",
                                          ifelse(M6_24_sch >= quants$M6_24_sch[3], "v.high",NA))))) %>%
  mutate(propPrimSec = ifelse(propPrimSec < quants$propPrimSec[1], "v.low",
                              ifelse(propPrimSec < quants$propPrimSec[2], "low",
                                     ifelse(propPrimSec < quants$propPrimSec[3], "high",
                                            ifelse(propPrimSec >= quants$propPrimSec[3], "v.high",NA))))) %>%
  mutate(propSecSec = ifelse(propSecSec < quants$propSecSec[1], "v.low",
                             ifelse(propSecSec < quants$propSecSec[2], "low",
                                    ifelse(propSecSec < quants$propSecSec[3], "high",
                                           ifelse(propSecSec >= quants$propSecSec[3], "v.high",NA))))) %>%
  mutate(Les1_R_Land = ifelse(Les1_R_Land < quants$Les1_R_Land[1], "v.low",
                              ifelse(Les1_R_Land < quants$Les1_R_Land[2], "low",
                                     ifelse(Les1_R_Land < quants$Les1_R_Land[3], "high",
                                            ifelse(Les1_R_Land >= quants$Les1_R_Land[3], "v.high",NA))))) %>%
  mutate(pig_fam = ifelse(pig_fam < quants$pig_fam[1], "v.low",
                          ifelse(pig_fam < quants$pig_fam[2], "low",
                                 ifelse(pig_fam < quants$pig_fam[3], "high",
                                        ifelse(pig_fam >= quants$pig_fam[3], "v.high",NA))))) %>%
  mutate(dist_sch = ifelse(dist_sch < quants$dist_sch[1], "v.low",
                           ifelse(dist_sch < quants$dist_sch[2], "low",
                                  ifelse(dist_sch < quants$dist_sch[3], "high",
                                         ifelse(dist_sch >= quants$dist_sch[3], "v.high",NA))))) %>%
  mutate(garbage = ifelse(garbage < quants$garbage[1], "v.low",
                          ifelse(garbage < quants$garbage[2], "low",
                                 ifelse(garbage < quants$garbage[3], "high",
                                        ifelse(garbage >= quants$garbage[3], "v.high",NA))))) %>%
  mutate(KM_Comm = ifelse(KM_Comm < quants$KM_Comm[1], "v.low",
                          ifelse(KM_Comm < quants$KM_Comm[2], "low",
                                 ifelse(KM_Comm < quants$KM_Comm[3], "high",
                                        ifelse(KM_Comm >= quants$KM_Comm[3], "v.high",NA))))) %>%
  mutate(crim_case = ifelse(crim_case < quants$crim_case[1], "v.low",
                            ifelse(crim_case < quants$crim_case[2], "low",
                                   ifelse(crim_case < quants$crim_case[3], "high",
                                          ifelse(crim_case >= quants$crim_case[3], "v.high",NA))))) %>%
  mutate(land_confl = ifelse(land_confl < quants$land_confl[1], "v.low",
                             ifelse(land_confl < quants$land_confl[2], "low",
                                    ifelse(land_confl < quants$land_confl[3], "high",
                                           ifelse(land_confl >= quants$land_confl[3], "v.high",NA))))) %>%
  mutate(Pax_migt_in = ifelse(Pax_migt_in < quants$Pax_migt_in[1], "v.low",
                              ifelse(Pax_migt_in < quants$Pax_migt_in[2], "low",
                                     ifelse(Pax_migt_in < quants$Pax_migt_in[3], "high",
                                            ifelse(Pax_migt_in >= quants$Pax_migt_in[3], "v.high",NA))))) %>%
  mutate(Pax_migt_out = ifelse(Pax_migt_out < quants$Pax_migt_out[1], "v.low",
                               ifelse(Pax_migt_out < quants$Pax_migt_out[2], "low",
                                      ifelse(Pax_migt_out < quants$Pax_migt_out[3], "high",
                                             ifelse(Pax_migt_out >= quants$Pax_migt_out[3], "v.high",NA))))) 


      # heatmap (UPGMA) - categorical ####


# put dat_prov into long format
dat_prov.ql <- melt(dat_prov.quants, id.vars = "agglo.clus")

# set colours
cols <- c("v.low" = "darkblue", "low" = "blue", "high" = "orange", "v.high" = "red")

# change value to factor and re-order
dat_prov.ql$value <- as.factor(dat_prov.ql$value)
dat_prov.ql$value <- factor(dat_prov.ql$value, c("v.high","high","low","v.low"))

# heatmap with the categories
upgma.heatmap.cat <- ggplot(dat_prov.ql, aes(x=agglo.clus, y=variable))+
                      geom_tile(aes(fill=value))+
                      scale_fill_manual(values = cols)+
                      xlab("Cluster (UPGMA)")+
                      ylab("Variable")+
                      theme(axis.text = element_text(size=15),
                            axis.title = element_text(size=18),
                            legend.text = element_text(size=18),
                            legend.title = element_text(size=17),
                            legend.key.size = unit(2,'cm'))

ggsave("Results/Cluster_analysis/Average_agglo/UPGMA_heatmap_cat_NEW.png", upgma.heatmap.cat, 
       width = 30, height = 30, units = "cm", dpi=300)

      # Plots of clusters versus environmental vars ####

# Now I want to plot clusters against the environmental vars

# I will need to treat the environ vars the same way as the vars used for clustering. i.e. mean across provinces and years

# summarise variables for all years - take the mean for each variable for each province across all years. 
env.dat <- dat_prov %>% group_by(Province) %>% summarise_all(mean) 

# pull uot envrion vars
env.dat <- env.dat %>% select(Province,areaKM,ForPix,diffPix,mean_elev,dist_border,dist_provCap)

# add clusters
env.dat$agglo.clus <- ifelse(env.dat$Province %in% clus1, "1",
                              ifelse(env.dat$Province %in% clus2, "2",
                                     ifelse(env.dat$Province %in% clus3, "3",
                                            ifelse(env.dat$Province %in% clus4, "4", 
                                                   ifelse(env.dat$Province %in% clus5, "5", NA)))))

# remove Phnom Penh
env.dat <- env.dat %>% filter(Province != "Phnom Penh")

# convert ForPix to km2
env.dat$For.area <- env.dat$ForPix * 0.09

# colors to match the map
cols <- c("tomato3","skyblue2","deepskyblue4","goldenrod","darkolivegreen3")

# plots
forpix.p <- ggplot(env.dat, aes(x=agglo.clus, y=For.area, color=agglo.clus))+
            scale_color_manual(values=cols)+
            geom_boxplot(size=1)+
            xlab("")+
            ylab("Mean forested area (km2)")+
            theme_classic()+
            theme(legend.position = "none")

area.p <- ggplot(env.dat, aes(x=agglo.clus, y=areaKM, color=agglo.clus))+
          scale_color_manual(values=cols)+
          geom_boxplot(size=1)+
          xlab("")+
          ylab("Mean size of province (km2)")+
          theme_classic()+
          theme(legend.position = "none")

difpix.p <- ggplot(env.dat, aes(x=agglo.clus, y=diffPix, color=agglo.clus))+
            scale_color_manual(values=cols)+
            geom_boxplot(size=1)+
            xlab("")+
            ylab("Change in forest cover between 2007 - 2012")+
            theme_classic()+
            theme(legend.position = "none")

elev.p <- ggplot(env.dat, aes(x=agglo.clus, y=mean_elev, color=agglo.clus))+
          scale_color_manual(values=cols)+
          geom_boxplot(size=1)+
          xlab("Cluster")+
          ylab("Mean elevation (masl)")+
          theme_classic()+
          theme(legend.position = "none")

distBord.p <- ggplot(env.dat, aes(x=agglo.clus, y=dist_border, color=agglo.clus))+
              scale_color_manual(values=cols)+
              geom_boxplot(size=1)+
              xlab("Cluster")+
              ylab("Mean distance to international border (KM)")+
              theme_classic()+
              theme(legend.position = "none")

distCap.p <- ggplot(env.dat, aes(x=agglo.clus, y=dist_provCap, color=agglo.clus))+
              scale_color_manual(values=cols)+
              geom_boxplot(size=1)+
              xlab("Cluster")+
              ylab("Mean distance to provincial capital (KM)")+
              theme_classic()+
              theme(legend.position = "none")

forpix.p + area.p + difpix.p + elev.p + distBord.p + distCap.p + plot_layout(ncol=3)
