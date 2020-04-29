library('tidyverse')
library('nlme')

## load latest version of data
dat <- read.csv("Data/commune/dat_merge.csv", header = TRUE)

# stage 1 from section 5.2 in Zuur mixed effect modelling book
modellist <- lmList(ForPix ~ tot_pop | Commune, dat)
plot(ranef(modellist))
m1re <- ranef(modellist, aug=TRUE)
plot(m1re, form=~fem_18_60)
betas <- coef(modellist)

# example from book using loop
RIKZ <- read.table("Data/Zuur_mixedModels_data/RIKZ.txt", header = TRUE)
 Beta <- vector(length = 9)
 for (i in 1:9){
Mi <- summary(lm(Richness ??? NAP,
subset = (Beach==i), data=RIKZ))
Beta[i] <- Mi$coefficients[2, 1]}
 
 # stage 2
 m2 <- lm(betas$tot_pop ~ dat$PA)
 
 
 # mixed model
 mm1 <- lme(ForPix ~ tot_pop, random = ~1|Commune, data=dat)
summary(mm1) 
# redisual variance = 512.97^2 =  263140.3
# variance for random intercept = 2709.885^2 = 7343477
# d^2 or variance for random intercept is large and so the difference in the lines for the differnet communes is large (the lines can move up and down from the population model line at lot)

F0 <- fitted(mm1, level = 0)
F1 <- fitted(mm1, level = 1)
I <- order(dat$tot_pop) 
pops <- sort(dat$tot_pop)
plot(pops, F0[I], lwd = 4, type = "l",
ylim = c(0, 40000), ylab = "Forest pixels", xlab = "Total population")
for (i in 1:690){
x1 <- dat$tot_pop[dat$Commune == i]
y1 <- F1[dat$Commune == i]
K <- order(x1)
lines(sort(x1), y1[K])
}
text(dat$tot_pop, dat$ForPix, dat$Commune, cex = 0.9)



F0 <- fitted(Mlme1, level = 0)
F1 <- fitted(Mlme1, level = 1)
I <- order(RIKZ$NAP) 
NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l",
ylim = c(0, 22), ylab = "Richness", xlab = "NAP")
5.3 The Linear Mixed Effects Model 109
for (i in 1:9){
x1 <- RIKZ$NAP[RIKZ$Beach == i]
y1 <- F1[RIKZ$Beach == i]
K <- order(x1)
lines(sort(x1), y1[K])
}
> text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9)
