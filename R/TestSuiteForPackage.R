# Testing for FastImputation

# HapLap: setwd("C:/Documents and Settings/Steve/My Documents/Dropbox/FastImputation/dev")
library(roxygen2)           # provides roxygenize to regen Rd files befor compiling


# HapLap: setwd("C:/Documents and Settings/Steve/My Documents/Dropbox/FastImputation")
library(FastImputation)
library(mvtnorm)            # provides rmvnorm for multivariate normal draws
library(Amelia)             # provides King's multiple imputation software
library(abind)              # provides abind for making an array out of Amelia results
library(clusterGeneration)  # provides genPositiveDefMat to generate random covariance matrices



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
  return( as.data.frame( 
    apply(x, c(1,2), function(this.cell) {
      return( ifelse(runif(1) < prob.dropped, NA, this.cell) )
    }) 
  ) )
}

###  Test 1: Artificial multivariate normal data  ###
n <- 1000
n.cols <- 5
true.means <- 5 * rnorm(n.cols)
true.covariance <- genPositiveDefMat(dim=n.cols)$Sigma
test.data.complete <- as.data.frame(rmvnorm(n, mean=true.means, sigma=true.covariance))

test.data <- MakeSomeDataMissing(test.data.complete)

  ##  Imputation with column means  ##
  imputed.with.means <- ImputeWithColumnMean(test.data)
  rmse.means <- sqrt(mean((imputed.with.means - test.data.complete)^2))
  rmse.means

  ##  Imputation with column medians  ##
  imputed.with.medians <- ImputeWithColumnMedian(test.data)
  rmse.medians <- sqrt(mean((imputed.with.medians - test.data.complete)^2))
  rmse.medians

  ##  Imputation with Amelia  ##
  amelia.result <- amelia(test.data)
  imputed.with.amelia <- apply(abind(
    amelia.result$imputations$imp1,
    amelia.result$imputations$imp2,
    amelia.result$imputations$imp3,
    amelia.result$imputations$imp4,
    amelia.result$imputations$imp5,
    along=3), c(1,2), mean)
  rmse.amelia <- sqrt(mean((imputed.with.amelia - test.data.complete)^2))
  rmse.amelia

  ## Imputation with FastImputation  ##
  patterns <- TrainFastImputation(test.data)
  imputed.with.FI <- FastImputation(test.data, patterns)
  rmse.FI <- sqrt(mean((imputed.with.FI - test.data.complete)^2))
  rmse.FI
  
  ## Comparison  ##
  round(rbind(rmse.means, rmse.medians, rmse.amelia, rmse.FI), 3)


###  Test 2: Artificial data, with some bounds  ###
n <- 1000
n.cols <- 5
true.means <- 5 * rnorm(n.cols)
true.covariance <- genPositiveDefMat(dim=n.cols)$Sigma
test.data.complete <- as.data.frame(rmvnorm(n, mean=true.means, sigma=true.covariance))

# Add contraints to columns 1, 3, 5
test.data.complete[,1] <- exp(test.data.complete[,1]) + 10  # lower=10
test.data.complete[,3] <- 5 - exp(test.data.complete[,3])   # upper=5
test.data.complete[,5] <- pnorm(test.data.complete[,5]) * (15 - (-5)) + (-5)  # lower=-5, upper=15

test.data <- MakeSomeDataMissing(test.data.complete)

  ##  Imputation with column means  ##
  imputed.with.means <- ImputeWithColumnMean(test.data)
  rmse.means <- sqrt(mean((imputed.with.means - test.data.complete)^2))
  rmse.means

  ##  Imputation with column medians  ##
  imputed.with.medians <- ImputeWithColumnMedian(test.data)
  rmse.medians <- sqrt(mean((imputed.with.medians - test.data.complete)^2))
  rmse.medians

  ##  Imputation with Amelia  ##
  amelia.result <- amelia(test.data)
  imputed.with.amelia <- apply(abind(
    amelia.result$imputations$imp1,
    amelia.result$imputations$imp2,
    amelia.result$imputations$imp3,
    amelia.result$imputations$imp4,
    amelia.result$imputations$imp5,
    along=3), c(1,2), mean)
  rmse.amelia <- sqrt(mean((imputed.with.amelia - test.data.complete)^2))
  rmse.amelia

  ## Imputation with FastImputation  ##
  patterns <- TrainFastImputation(test.data, 
    constraints=list(
      list(1, list(lower=10)),
      list(3, list(upper=5)),
      list(5, list(lower=-5, upper=15)) ) )
  imputed.with.FI <- FastImputation(test.data, patterns)
  rmse.FI <- sqrt(mean((imputed.with.FI - test.data.complete)^2))
  rmse.FI
  
  ## Comparison  ##
  round(rbind(rmse.means, rmse.medians, rmse.amelia, rmse.FI), 3)

  ##  Imputation with Amelia (with bounds)  ##
  amelia.bounded.result <- amelia(test.data,
    bounds=rbind(c(1, 10, Inf),
      c(3, -Inf, 5),
      c(5, -5, 15)
    )
  )
  imputed.with.amelia.bounded <- apply(abind(
    amelia.bounded.result$imputations$imp1,
    amelia.bounded.result$imputations$imp2,
    amelia.bounded.result$imputations$imp3,
    amelia.bounded.result$imputations$imp4,
    amelia.bounded.result$imputations$imp5,
    along=3), c(1,2), mean)
  rmse.amelia.bnd <- sqrt(mean((imputed.with.amelia.bounded - test.data.complete)^2))
  rmse.amelia.bnd
  
  ## Comparison  ##
  round(rbind(rmse.means, rmse.medians, rmse.amelia, rmse.FI, rmse.amelia.bnd), 3)


###  Test 3: real data from classification task  ###
test.data.complete <- read.table("rf.tsv", header=TRUE,sep="\t",na.strings = "?")[,1:10]
test.data <- data.frame(test.data.complete[,1:3],
  MakeSomeDataMissing(test.data.complete[,4:10], prob.dropped=.1))
sum(is.na(test.data))
mean(is.na(test.data))
mean(is.na(test.data[,4:10]))

  ##  Imputation with column means  ##
  imputed.with.means <- ImputeWithColumnMean(test.data)
  rmse.means <- sqrt(mean((imputed.with.means - test.data.complete)^2))
  rmse.means

  ##  Imputation with column medians  ##
  imputed.with.medians <- ImputeWithColumnMedian(test.data)
  rmse.medians <- sqrt(mean((imputed.with.medians - test.data.complete)^2))
  rmse.medians

  ##  Imputation with Amelia  ##
  amelia.result <- amelia(test.data[,4:10])
  imputed.with.amelia <- cbind(test.data[,1:3], apply(abind(
    amelia.result$imputations$imp1,
    amelia.result$imputations$imp2,
    amelia.result$imputations$imp3,
    amelia.result$imputations$imp4,
    amelia.result$imputations$imp5,
    along=3), c(1,2), mean))
  rmse.amelia <- sqrt(mean((imputed.with.amelia - test.data.complete)^2))
  rmse.amelia

  ## Imputation with FastImputation  ##
  patterns <- TrainFastImputation(test.data[,4:10])
  imputed.with.FI <- cbind(test.data[,1:3], FastImputation(test.data[,4:10], patterns))
  rmse.FI <- sqrt(mean((imputed.with.FI - test.data.complete)^2))
  rmse.FI
  
  ## Comparison  ##
  round(rbind(rmse.means, rmse.medians, rmse.amelia, rmse.FI), 3)
