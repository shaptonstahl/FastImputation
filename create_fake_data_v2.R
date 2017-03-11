#' Test data set
#'
#'   constraints=list(list("bounded_below_2", list(lower=0)),
#'                    list("bounded_above_5", list(upper=0)),
#'                    list("bounded_above_and_below_6", list(lower=0, upper=1))
#'                    ),
#'   idvars="user_id_1",
#'   categorical="categorical_9"
#'
#' Categorical has three values
#' Total of 11 latent variables, but one is ignored, so 10 imputed vars.  The last three are for 
#' one categorical variable, so there are a total of 1 id + 8 data = 9 columns in the dataset.

library("clusterGeneration")
cov_matrix_obj <- genPositiveDefMat(10, covMethod="unifcorrmat")
cov_matrix <- (cov_matrix_obj$Sigma + t(cov_matrix_obj$Sigma)) / 2

n_obs_train <- 10000
n_obs_test <- 250
n_obs <- n_obs_train + n_obs_test
library("mvtnorm")
latent_data <- data.frame(id=paste("user", 1:n_obs, sep=""),
                          data.frame(rmvnorm(n_obs, sigma=cov_matrix)))
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
# remove some training data
for(i in 1:nrow(FI_train)) {
  for(j in 2:9) {
    if(runif(1) < fraction_missing) FI_train[i,j] <- NA
  }
}
# remove some test data
FI_test <- FI_true
for(i in 1:nrow(FI_test)) {
  for(j in 2:9) {
    if(runif(1) < fraction_missing) FI_test[i,j] <- NA
  }
}

names(FI_train) <- c("user_id_1", "bounded_below_2", "unbounded_3", "unbounded_4", 
                     "bounded_above_5", "bounded_above_and_below_6", "unbounded_7", 
                     "unbounded_8", "categorical_9")
names(FI_test) <- c("user_id_1", "bounded_below_2", "unbounded_3", "unbounded_4", 
                    "bounded_above_5", "bounded_above_and_below_6", "unbounded_7", 
                    "unbounded_8", "categorical_9")
names(FI_true) <- c("user_id_1", "bounded_below_2", "unbounded_3", "unbounded_4", 
                    "bounded_above_5", "bounded_above_and_below_6", "unbounded_7", 
                    "unbounded_8", "categorical_9")

save(FI_train, file="dev/FastImputation/data/FI_train.RData")
save(FI_test, file="dev/FastImputation/data/FI_test.RData")
save(FI_true, file="dev/FastImputation/data/FI_true.RData")
