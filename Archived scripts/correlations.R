setwd("C://Users/Matt&Kez/Box Sync/Objective 1/Analysis/Pre-analysis/correlations/Text files")
library('Hmisc')
library('corrplot')
source("http://www.sthda.com/upload/rquery_cormat.r")

## load data
macDat <- read.delim("macroeconomic_variables.txt", header=T)
str(macDat)

## subset data
econ <- macDat[c(1:6)]
agri <- macDat[c(7:8, 11, 14, 17)]
prod <- macDat[c(20:24)]

## simple correlation on all data
corr1 <- rcorr(as.matrix(macDat, type="pearson"))
corr1

## create the flatten correlation matrix function 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

## run funciton - NOT WORKING
#corr_flat <- flattenCorrMatrix(corr1$r, corr1$p)

## trying a different function from the corrplot package

rquery.cormat(prod, type="full")
rquery.cormat(prod, type="flatten", graph=FALSE)
