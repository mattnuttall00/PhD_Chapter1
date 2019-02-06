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

library(lme4)

# Remove 5 communes that have 0 forest in 2010 (not sure how they slipped through the net)
dat_rawforest <- dat_rawforest %>% 
                  filter(!CommCode %in% c(40102,40105,150206,150208,150302))

# changing pixDiff to numeric 
dat_rawforest$pixDiff <- as.numeric(dat_rawforest$pixDiff)

# Histogram of original forest cover
hist(dat_rawforest$forestPix)

# Pulling out the differences in pixels (pixDiff) for now just so I don't mess around with the main dataframe by deleting NAs etc.
pixdiff <- dat_rawforest$pixDiff
pixdiff <- as.numeric(pixdiff)
pixdiff <- na.omit(pixdiff) 


hist(pixdiff)
hist(pixdiff, breaks = c(-50,0,554))
summary(pixdiff)

# try truncating the data
pixdiff1 <- pixdiff[pixdiff >-5 & pixdiff <5] 
hist(pixdiff1)

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
