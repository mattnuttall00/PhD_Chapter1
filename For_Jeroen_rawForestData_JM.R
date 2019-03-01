library(tidyverse)

# Load raw data
dat_raw_10_15 <- read.csv("CCI_ForCov_CommuneForest_2010-15.csv", header = TRUE)
str(dat_raw_10_15)
head(dat_raw_10_15)

# Split into two dfs
forestPix <- dat_raw_10_15[ ,c(1:3,5,7,9,11,13)]
str(forestPix)

pixDiff <- dat_raw_10_15[ ,c(1:2,4,6,8,10,12,14)]
str(pixDiff)
head(pixDiff)

# Get into tidy format
forestPix <- forestPix %>% 
  gather(key = Year, value = forestPix, X2010:X2015) 
str(forestPix)
head(forestPix)

pixDiff <- pixDiff %>% gather(key = Year, value = pixDiff, diffPix:diffPix.5)
head(pixDiff)
str(pixDiff)

# Add year to pixDiff to match forestPix
years <- c("X2010", "X2011", "X2012", "X2013", "X2014", "X2015")
years1 <- rep(years, each = 768)

pixDiff$Year <- years1
str(pixDiff)      
pixDiff[765:770,]
forestPix[765:770,]

# Merge forestPix and diffPix
dat_rawforest <- cbind(forestPix, pixDiff)
str(dat_rawforest)
head(dat_rawforest)

# Check the order is correct
dat_rawforest[1533:1540,]

# Remove duplicate columns
dat_rawforest <- dat_rawforest[,-c(5:7)]

# Remove "X" in year
dat_rawforest$Year <- substr(dat_rawforest$Year, start = 2, stop = 5)

# re-order columns
dat_rawforest <- dat_rawforest %>% select(Year,CommCode,Commune,forestPix,pixDiff)

# re-order rows so that they go 2010,2011,2012...etc 
dat_rawforest <- dat_rawforest[order(dat_rawforest$Commune, dat_rawforest$CommCode),]

head(dat_rawforest)

############
###
### So to expediate things, I've taken ALL of the above manipulations for granted and have just assumed that the
### resulting data frame has the correct working values... probably best to carefully check this is the case; see issues
### below.
###
############

library(lme4)

# Remove 5 communes that have 0 forest in 2010 (not sure how they slipped through the net)
dat_rawforest <- dat_rawforest %>% 
                  filter(!CommCode %in% c(40102,40105,150206,150208,150302))

# changing pixDiff to numeric 
# dat_rawforest$pixDiff <- as.numeric(dat_rawforest$pixDiff)

### Commented out!
### Somewhat of a massive issue here; the above statements translates the level names to numeric,
###  not the text values. This is completely different! 
### Worst is that by using your code above, al of the zero's change to 1's, but further very funny business
###  happens to a lot of the other values... Have a look at what happens when you put the original,
###  your version, and the correct conversion, next to each other in a table:

head(data.frame(orig=dat_rawforest$pixDiff, 
           yours=as.numeric(dat_rawforest$pixDiff), 
           correct=as.numeric(as.vector(dat_rawforest$pixDiff))), 50)
### I think the problem here is that because of your explicit 'na' values in the original data, and/or because
###  of tidyverse's witchcraft above, dat_rawforest$pixDiff is a factor, not a vector. When this is directly
###  translated to a numeric vector, R just uses the factor level names, not their text value, as the number!

### Here is the issue in histogram form:
par(mfrow=c(1,2))
hist(as.numeric(dat_rawforest$pixDiff), main='Your conversion', xlab='pixDiff')
hist(as.numeric(as.vector(dat_rawforest$pixDiff)), main='correct', xlab='pixDiff')
par(mfrow=c(1,1))

### So this is the most effective way of achieving what I think you want to achieve:
dat_rawforest$pixDiff <- as.numeric(as.vector(dat_rawforest$pixDiff))
### Note vectorise first, then make numeric.
### It's great and highly recommended to do these manipulations in R, but BE CAREFUL!
### I would always - always! - cross check that your manipulations actually do what you think they do-
### just print out a .csv of the resulting re-worked data every once in a while, and check directly.
### Best to do this at a number of different stages.


# Histogram of original forest cover
hist(dat_rawforest$forestPix)

# Pulling out the differences in pixels (pixDiff) for now just so I don't mess around with the main dataframe by 
# deleting NAs etc.
### Edited as per above:
pixdiff <- dat_rawforest$pixDiff 
#pixdiff <- na.omit(pixdiff) 
### Instead of the above (result of which leaves you with some crap inside the resulting vector)
###  this is a neater way of doing the same thing (basically, is.na() gives a logical vector for which values are NA,
###  by putting a ! infront you turn it around - ie. which values are NOT na - and then use that vector to subset:
pixdiff <- pixdiff[!is.na(pixdiff)]

hist(pixdiff)
hist(pixdiff, breaks = c(-50,0,554))
### Try looking at just the non-zero differences:
hist(pixdiff[pixdiff!=0])

summary(pixdiff)

# try truncating the data
pixdiff1 <- pixdiff[pixdiff >-5 & pixdiff <5] 
hist(pixdiff1)

### Perhaps more importantly than the above, I would also be very careful about an approach where you extract a single
### column to work on separately - it's easy to forget which manipulations you end up doing with this one. I would
### strongly suggest just working with a copy of the dataset instead. Also, you worry about 'deleting NA's' - I don't
### think that would be a problem, actually? Because, where pixDiff==NA, there was no prior year to calculate a
### difference from - so arguably, you can't use those rows anyway (except for needing the previous year forest cover
### value later on)... So lets make a copy of dat_rawforest and work with that one. This'll make it easier to work with
### the offset and models later.
dat_forest <- dat_rawforest

dat_forest$CommCode <- factor(dat_forest$CommCode)

dat_forest_new <- as.data.frame(NULL)
for(i in 1:nlevels(dat_forest$CommCode)) {
  temp <- dat_forest[dat_forest$CommCode==levels(dat_forest$CommCode)[i],]
  temp2 <- c(NA,temp$forestPix)
  temp2 <- temp2[1:length(temp2)-1]
  temp <- cbind(temp, temp2)
  dat_forest_new <- rbind(dat_forest_new, temp)
}
names(dat_forest_new)[6] <- 'prevForest'
dat_forest_new <- dat_forest_new[!is.na(dat_forest_new$pixDiff),]



### OK so on this basis I'm almost 100% sure that if this is what your response variable is going to be,
###  we'll need some way of explicitly dealing with the zero cases; just because there are so many of them...
###
### Probably need to discuss this, but one option (simplest) would be to just focus the analysis on those cases
###  (years, communes) where ANY forest was lost in the first place (so in the case of the pixDiff variable), all cases
###  that are >0. Basically this would give a model for predicting the magnitude of forest loss, where it occurs.
### Crucially, the model wouldn't be able to predict whether or not loss would occur, as a function of its predictors.
### I think the only possible other alternative would be to use some form of zero-inflated model; i.e. have a component
###  of the model that models zero loss (or gain) vs. loss, and a separate component that predicts the amount of loss,
###  where there is any.

### In terms of the offset, so the important point - I think - is that for any given year, say 2014, you want to predict
###  the amount of forest loss relative to total forest cover in 2013... so the previous, not the current year?

dat_forest_new$yr <- as.numeric(dat_forest_new$Year)-2000
dat_forest_new <- dat_forest_new[!is.na(dat_forest_new$pixDiff),]

jm_M1 <- lmer(pixDiff ~ 1 + (1|CommCode) + (1|Year), offset = prevForest, data=dat_forest_new)

jm_M3 <- lmer(pixDiff ~ 1 + (yr|CommCode), offset = prevForest, data=dat_forest_new)

jm_M4 <- lmer(pixDiff ~ 1 + (1|CommCode), offset = prevForest, data=dat_forest_new)

jm_M5 <- glmmadmb(pixDiff ~ 1 + (yr|CommCode) + offset(prevForest), data=dat_forest_new, family='nbinom1',
                  zeroInflation=T, admb.opts=admbControl(maxfn=10000, imaxfn=10000))





# Model 1 - offset = original amount of forest in 2010
M1 <- lmer(pixDiff ~ 1 + (1|Commune) + (1|Year), offset = forestPix, data=dat_rawforest)
summary(M1)

plot(residuals(M1))
hist(residuals(M1))

# create offset which is raw forest cover difference / original raw forest cover
dat_rawforest <- dat_rawforest %>% 
                  mutate(offset = pixDiff/forestPix)

# Model 2 - offset = raw forest cover difference / original raw forest cover
M2 <- lmer(pixDiff ~ 1 + (1|Commune) + (1|Year), offset = offset, data=dat_rawforest)
summary(M2)
plot(residuals(M2))
hist(residuals(M2))
