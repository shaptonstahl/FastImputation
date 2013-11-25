library(clusterGeneration)
library(mvtnorm)
library(plyr)

n.vars <- 5
n.obs <- 1e3
frac.missing <- .5

cov.true <- genPositiveDefMat(n.vars)$Sigma
x <- rmvnorm(n.obs, mean=10*rnorm(n.vars), sigma=cov.true)
cov.full <- var(x)

x.w.missing <- x
x.w.missing[sample(n.vars * n.obs, round(n.vars * n.obs * frac.missing))] <- NA
mean(is.na(x.w.missing))  # should be frac.missing

cov.default <- var(x.w.missing, na.rm=TRUE)
cov.new <- CovarianceWithMissing(x.w.missing)

cov.full
cov.default
cov.new

MeanRelativeError <- function(est, base) mean(abs((est-base)/base))
MedianRelativeError <- function(est, base) median(abs((est-base)/base))

MeanRelativeError(cov.default, cov.full)
MeanRelativeError(cov.new, cov.full)

##  Plot performance as missingness increases
frac.missing <- seq(.05, .95, .05)
n.vars <- 5
n.obs <- 1e3
n.trials <- 1e2

DoTrial <- function(frac.missing, n.obs, n.vars) {
  cov.true <- genPositiveDefMat(n.vars)$Sigma
  x <- rmvnorm(n.obs, mean=10*rnorm(n.vars), sigma=cov.true)
  cov.full <- var(x)
  x.w.missing <- x
  x.w.missing[sample(n.vars * n.obs, round(n.vars * n.obs * fm))] <- NA
  mre.default <- MedianRelativeError(var(x.w.missing, na.rm=TRUE), cov.full)
  mre.new <- MedianRelativeError(CovarianceWithMissing(x.w.missing), cov.full)
  out <- c(mre.default, mre.new)
  names(out) <- c("mre.default", "mre.new")
  return(out)
}

mres <- laply(frac.missing, function(fm) {
  cat("Calculating for fraction missing:", fm, "\n")
  colMeans(laply(1:n.trials, function(i) DoTrial(fm, n.obs, n.vars)))
})

plot(frac.missing, mres[,1],
     xlim=c(0,1), ylim=c(0, 5),
     lty=2, type="l",
     main="Relative error for covariance estimates")
lines(frac.missing, mres[,2])

###################################################
###  Generate test data for testing in package  ###
###################################################

n.vars <- 5
n.obs <- 1e3
frac.missing <- .5
miss <- sample(n.vars * n.obs, round(n.vars * n.obs * frac.missing))

cov.true <- genPositiveDefMat(n.vars)$Sigma
x.full <- rmvnorm(n.obs, mean=10*rnorm(n.vars), sigma=cov.true)
dump(c("miss", "x.full"))