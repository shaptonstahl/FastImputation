library("clusterGeneration")
cov_matrix_obj <- genPositiveDefMat(10)
cov_matrix <- (cov_matrix_obj$Sigma + t(cov_matrix_obj$Sigma)) / 2

n_obs_train <- 100000
n_obs_test <- 2500
n_obs <- n_obs_train + n_obs_test
library("mvtnorm")
latent_data <- data.frame(id=paste("user", 1:n_obs, sep=""),
                          data.frame(rmvnorm(n_obs, sigma=cov_matrix)))

FI_train <- latent_data[1:n_obs_train,2:11]
FI_true <- latent_data[(n_obs_train+1):(n_obs_train+n_obs_test),2:11]

empirical_cov <- cov(FI_train)
mean(abs((empirical_cov - cov_matrix) / cov_matrix))

fraction_missing <- .05
# remove some training data
for(i in 1:nrow(FI_train)) {
  for(j in 1:ncol(FI_train)) {
    if(runif(1) < fraction_missing) FI_train[i,j] <- NA
  }
}
# remove some test data
FI_test <- FI_true
for(i in 1:nrow(FI_test)) {
  for(j in 1:ncol(FI_test)) {
    if(runif(1) < fraction_missing) FI_test[i,j] <- NA
  }
}

FI_means <- colMeans(FI_train, na.rm=TRUE)
FI_covariance <- CovarianceWithMissing(FI_train)

avg_rel_error_cov <- mean(abs((FI_covariance - cov_matrix) / cov_matrix))
avg_rel_error_cov

rel_error_from_missing_cov <- mean(abs((FI_covariance - empirical_cov) / empirical_cov))
rel_error_from_missing_cov

imputed_data <- FI_test

for(i_row in 1:nrow(imputed_data)) {
  constrained_row <- imputed_data[i_row,]
  if( sum(!is.na(constrained_row)) != 0 ) {  # do nothing if nothing is missing
    # Use formula for mean here: http://en.wikipedia.org/wiki/Multivariate_normal_distribution#Conditional_distributions
    cols_to_impute <- which(is.na(constrained_row))    # indices of "1" in Wikipedia formula for mean of conditional multivariate normal distribution
    if( length(cols_to_impute) == length(constrained_row) ) {
      # nothing to condition on
      replacement_values <- FI_means
    } else {
      # fill based on conditional normal distribution
      known_cols <- setdiff(1:ncol(FI_test), cols_to_impute)  # incides of "2" in Wikipedia formula for mean of conditional multivariate normal distribution
      
      replacement_values <- t(t(FI_means[cols_to_impute])) + 
        FI_covariance[cols_to_impute,known_cols, drop=FALSE] %*% 
        solve(a=FI_covariance[known_cols,known_cols], 
              b=t(constrained_row[known_cols]) - t(t(FI_means[known_cols])))
    }
    # Store replacement values (note that constraints are not yet applied)
    imputed_data[i_row,cols_to_impute] <- replacement_values ### PERHAPS ADD as.vector to RHS
  }
}

imputed_cells <- is.na(FI_test)
imputed_values <- imputed_data[imputed_cells]
true_values <- FI_true[imputed_cells]

rmse <- sqrt(mean((imputed_values - true_values)^2))
rmse
avg_rel_error <- mean( abs((imputed_values - true_values) / true_values) )
avg_rel_error
mean(true_values)
hist(true_values)
