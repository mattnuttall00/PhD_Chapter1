setwd("/Users/Matt&Kez/Box Sync/Objective 1/Data/Text files")      # You'll need to change this
library('robustbase')
library('xlsx')
install.packages('rJava')
dat <- read.csv("Socioeconomic_variables.csv")
commDat <- read.csv("commGIS.csv")

# Subset indigenous group columns
dat.ig <- dat[,10:73]

# This new dataframe has 64 columns, representing 64/4=16 groups
# Groups start at columns 1,5,9,13 etc, so we first need to create a sequence of numbers representing this
grp.start <- seq(from=1,to=61,by=4)

# We now write a loop to sum age group columns for each indigenous group (represented by its start column)
dat.ig2 <- as.data.frame(matrix(NA,nrow(dat.ig),ncol=16)) 
for (g in 1:length(grp.start)){
  sub <- dat.ig[,grp.start[g]:(grp.start[g]+3)]
  ag.sum <- apply(sub,1,sum)
  dat.ig2[,g] <- ag.sum
}

# From this latest data frame, create a vector representing the proportion of indigenous people in total population
tot.pop <- dat$tot_pop
tot.ig <- rowSums(dat.ig2,na.rm=T)
prop.ig <- tot.ig/tot.pop

# Note that you would get the same by doing...
#prop.ig <- rowSums(dat[,10:73],na.rm=T)/dat$tot_pop

# Then add to initial dataset and remove unwanted columns
dat2 <- dat
dat2$Prop_Indigenous <- prop.ig
dat2 <- dat2[,-c(10:73)]

## add commGIS codes

dat2$CommCode <- NA   # Create an empty column in dat2 to store codes

for (i in 1:nrow(dat2)){   # For each row in dat2
  cm <- dat2$Commune[i]    # What is the commune name?
  pv <- dat2$Province[i]   # What is the province name?
  cc <- subset(commDat,province==pv & commune==cm)  # Subset the line in commDat that matches these two values
  dat2$CommCode[i] <- cc[,1]  # Insert the associated commune code into dat2
}

## Edit below code to select by communeGIS not commune
# To aggregate at the commune level, we can do another loop

commGIS <- unique(dat2$CommCode)    # Unique commune IDs
dat3 <- dat2[1,]        # Create template for final data frame columns
for (c in 1:length(commGIS)){      # For each commune ID
  sub <- dat2[dat2$CommCode==commGIS[c],]   # Subset the data frame
  for (cc in 1:ncol(sub)){                                            # For each column of sub
    if (cc %in% c(5:9,14,16,17,41,42)){   # If column number is in this list of numbers...
      dat3[c,cc] <- sum(sub[cc],na.rm=T)           # Do the column sum
    }
    if (cc %in% c(10:13,15,18:28,30:35,37:40,43)){     # If the column number is in this list of numbers...
      dat3[c,cc] <- mean(sub[cc],na.rm=T)                   # Do the column mean
    }
    if (cc==29){                                                 # If the column number is 29...
      dat3[c,cc] <- median(sub[cc],na.rm=T)     # Do the median
    } 
  }
}


commGIS <- unique(dat2$CommCode)    # Unique commune IDs
dat3 <- dat2[1,]        # Create template for final data frame columns
for (c in 1:length(commGIS)){      # For each commune ID
  sub <- dat2[dat2$CommCode==commGIS[c],]   # Subset the data frame
  for (cc in 1:ncol(sub)){                                            # For each column of sub
    if (cc %in% c(5:9,14,16,17,41,42)){   # If column number is in this list of numbers...
      dat3[c,cc] <- sum(sub[cc],na.rm=T)           # Do the column sum
    }
    if (cc %in% c(10:13,15,18:28,30:35,37:40,43)){     # If the column number is in this list of numbers...
      dat3[c,cc] <- mean(sub[cc],na.rm=T)                   # Do the column mean
    }
    if (cc==29){                                                 # If the column number is 29...
      dat3[c,cc] <- apply(x, 1, function(x){median(sub[cc][sub[cc]>0])})     # Do the median
    } 
  }
}


dat3 <- dat3[,-4]    # Remove column 4 as it should be empty (no villages)


