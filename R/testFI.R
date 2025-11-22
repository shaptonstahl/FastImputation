
source("http://sheer.ucdavis.edu/svn/software/public/deCruft/deCruft.R")
library(Amelia)

# HapLap: setwd("C:/Documents and Settings/Steve/My Documents/Dropbox/FastImputation")
source("functions.R")  # Load FastImputation functions
test.data <- read.table("rf.tsv", header=TRUE,sep="\t",na.strings = "?")[,1:10]

# missing data
sum(is.na(test.data))
# No data is missing
ncol(test.data)


ImputeWithColumnMean <- function(x) {
  for(i in 1:ncol(x)) {
    x[is.na(x[,i]),i] <- mean(x[,i], na.rm=TRUE)
  }
  return(x)
}

ImputeWithColumnMedian <- function(x) {
  for(i in 1:ncol(x)) {
    x[is.na(x[,i]),i] <- median(x[,i], na.rm=TRUE)
  }
  return(x)
}

MakeSomeDataMissing <- function(x, prob.dropped=.1) {
  return( apply(x, c(1,2), function(this.cell) {
    return( ifelse(runif(1) < prob.dropped, NA, this.cell) )
  }) )
}

test.data.with.missing <- data.frame(test.data[,1:3],
  MakeSomeDataMissing(test.data[,4:10], prob.dropped=.1))
sum(is.na(test.data.with.missing))
mean(is.na(test.data.with.missing))
mean(is.na(test.data.with.missing[,4:10]))

imputed.with.means <- ImputeWithColumnMean(test.data.with.missing)
rmse.means <- sqrt(mean((imputed.with.means - test.data)^2))
rmse.means

imputed.with.medians <- ImputeWithColumnMedian(test.data.with.missing)
rmse.medians <- sqrt(mean((imputed.with.medians - test.data)^2))
rmse.medians

amelia.result <- amelia(test.data.with.missing)
imputed.with.amelia <- test.data.with.missing
for(i in 1:nrow(test.data.with.missing)) {
  for(j in 1:ncol(test.data.with.missing)) {
    imputed.with.amelia[i,j] <- mean(c(
      amelia.result$imputations$imp1[i,j],
      amelia.result$imputations$imp2[i,j],
      amelia.result$imputations$imp3[i,j],
      amelia.result$imputations$imp4[i,j],
      amelia.result$imputations$imp5[i,j]
    ))
  }
}
rmse.amelia <- sqrt(mean((imputed.with.amelia - test.data)^2))
rmse.amelia


patterns <- TrainFastImputation(test.data.with.missing[,4:10])
rbind( test.data.with.missing[2,4:10],
  FastImputation(test.data.with.missing[2,4:10], patterns) )
rbind( test.data.with.missing[4,4:10],
  FastImputation(test.data.with.missing[4,4:10], patterns) )

imputed.with.FI <- cbind(test.data.with.missing[,1:3],
  FastImputation(test.data.with.missing[,4:10], patterns))
rmse.FI <- sqrt(mean((imputed.with.FI - test.data)^2))
rmse.FI
