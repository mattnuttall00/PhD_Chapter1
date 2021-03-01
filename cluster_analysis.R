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

## I want to use the un-scaled province data for testing and plotting

# get the mean values across all years (i.e. one variable value per province)
dat_prov <- dat_prov %>% group_by(Province) %>% summarise_all(mean) 

# remove PP
dat_prov <- dat_prov %>% filter(Province != "Phnom Penh")

# attach the cluster
dat_prov$cluster <- k9.clus

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

  ## plots ####

# load in updated Province shapefile
prov.shp <- st_read('Spatial_data/province_cluster.shp')
prov.shp$cluster <- as.factor(prov.shp$cluster)

# plot using ggplot
ggplot(prov.shp)+
  geom_sf()

## NO need to do the below each time (continue after "save shapefile")

# remove PP & Tonle Sap
prov.shp <- prov.shp %>% filter(KHETTRN != "Phnom Penh")
nrow(prov.shp)
prov.shp <- prov.shp %>% filter(KHETTRN != "Tonle Sap")

# standardise Province names
prov.shp[1,4] <- "Ratanak Kiri"
prov.shp[3,4] <- "Otdar Meanchey"
prov.shp[8,4] <- "Mondul Kiri"
prov.shp[9,4] <- "Kracheh"
prov.shp[19,4] <- "Preah Sihanouk"
prov.shp[21,4] <- "Pailin"

# add cluster
prov.shp$cluster <- dat_prov$cluster[match(prov.shp$KHETTRN,dat_prov$Province)]
prov.shp$KHETTRN

# save shapefile
st_write(prov.shp, "Spatial_data/province_cluster.shp")


# plot based on cluster
ggplot(prov.shp,aes(group=cluster, fill=cluster))+
  geom_sf()+
  scale_fill_viridis(discrete=TRUE, option = "C")
