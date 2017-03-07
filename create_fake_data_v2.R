#' Test data set

#'   constraints=list(list(1, list(ignore=TRUE)),       # use for ids, other vars not to impute
#'                    list(2, list(lower=0)),           # continuous var with lower bound
#'                    list(5, list(upper=0)),           # continuous var with only upper bound
#'                    list(6, list(lower=0, upper=1)),  # bounded to a finite interval
#'                    list(9, list(categorical=TRUE))   # finite number of values
#'                    ))                                # vars 3, 4, 7, 8 continuous with no bounds

#' categorical has three values
#' total of 11 variables, but one is ignored, so 10 imputed vars

library("clusterGeneration")
cov_matrix <- genPositiveDefMat(10)

n_obs_train <- 10000
n_obs_test <- 250
n_obs <- n_obs_train + n_obs_test
library("mvtnorm")
latent_data <- data.frame(id=paste("user", 1:n_obs, sep=""),
                          data.frame(rmvnorm(n_obs, sigma=cov_matrix$Sigma)))
names(latent_data) <- c(paste("V", 1:8, sep=""), paste("X", 9:11, sep=""))

source("dev/FastImputation/R/BoundNormalizedVariable.R")
latent_data[,"V2"] <- BoundNormalizedVariable(latent_data[,"V2"],
                                              constraints=list(lower=0))
latent_data[,"V5"] <- BoundNormalizedVariable(latent_data[,"V5"],
                                              constraints=list(upper=0))
latent_data[,"V6"] <- BoundNormalizedVariable(latent_data[,"V6"],
                                              constraints=list(lower=0, upper=1))
# categorical
for(i in 1:n_obs) {
  is_selected <- which.max(latent_data[i,9:11])
  is_not_selected <- setdiff(1:3, is_selected)
  latent_data[i, 8 + is_selected] <- 1
  latent_data[i, 8 + is_not_selected] <- -1
}
latent_data <- data.frame(latent_data, V9=sapply(1:n_obs, function(i) {
  if(1 == latent_data$X9[i]) return("A")
  if(1 == latent_data$X10[i]) return("B")
  if(1 == latent_data$X11[i]) return("C")
}))
latent_data <- latent_data[,-c(9:11)]


FI_train <- latent_data[1:n_obs_train,]
FI_true <- latent_data[(n_obs_train+1):(n_obs_train+n_obs_test),]

fraction_missing <- .05
FI_test <- FI_true
for(i in 1:nrow(FI_test)) {
  for(j in 2:9) {
    if(runif(1) < fraction_missing) FI_test[i,j] <- NA
  }
}

save(FI_train, file="dev/FastImputation/data/FI_train.RData")
save(FI_test, file="dev/FastImputation/data/FI_test.RData")
save(FI_true, file="dev/FastImputation/data/FI_true.RData")