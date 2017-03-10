#' Testing the main algorithm
#' 
#' Result: When the correlations among variables are small, there isn't much information
#' on which to base imputation, so the quality of imputation is low.  Real data is likely
#' to have meaningful correlations.  Changing the sample data generation from
#' 
#'   cov_matrix_obj <- genPositiveDefMat(10)
#' 
#' to
#' 
#'   cov_matrix_obj <- genPositiveDefMat(10, covMethod="unifcorrmat")
#'   
#' produced larger correlations, and the quality of imputation was greatly increased.

library("clusterGeneration")
cov_matrix_obj <- genPositiveDefMat(10, covMethod="unifcorrmat")
cov_matrix <- (cov_matrix_obj$Sigma + t(cov_matrix_obj$Sigma)) / 2

cov_matrix_wo_diag <- cov_matrix
for(i in 1:ncol(cov_matrix_wo_diag)) cov_matrix_wo_diag[i,i] <- NA
hist(cov_matrix_wo_diag)
cor_matrix_wo_diag <- cov2cor(cov_matrix)
for(i in 1:ncol(cor_matrix_wo_diag)) cor_matrix_wo_diag[i,i] <- NA
hist(cor_matrix_wo_diag, 20)

n_obs_train <- 10000
n_obs_test <- 250
n_obs <- n_obs_train + n_obs_test
library("mvtnorm")
latent_data <- data.frame(id=paste("user", 1:n_obs, sep=""),
                          data.frame(rmvnorm(n_obs, 
                                             mean=runif(ncol(cov_matrix), min=10, max=100),
                                             sigma=cov_matrix)))

FI_train <- latent_data[1:n_obs_train,2:11]
FI_true <- latent_data[(n_obs_train+1):(n_obs_train+n_obs_test),2:11]

cor_raw <- cor(FI_train)
hist(cor_raw)

# center and scale columns
FI_train <- scale(FI_train)
FI_true <- scale(FI_true)

cor_scaled <- cor(FI_train)
cor_rel_change <- (cor_scaled - cor_raw) / cor_raw
hist(cor_rel_change)
mean(cor_rel_change)
median(cor_rel_change)
# centering and scaling doesn't change correlation

empirical_cov <- cov(FI_train)
mean(abs((empirical_cov - cov_matrix) / cov_matrix))  # mean relative error in cov matrix when
                                                      # calculated on set with none missing

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

det(FI_covariance)
eigen(FI_covariance)$values
hist(FI_covariance, 20)

avg_rel_error_cov <- mean(abs((FI_covariance - cov_matrix) / cov_matrix))
avg_rel_error_cov

rel_error_from_missing_cov <- mean(abs((FI_covariance - empirical_cov) / empirical_cov))
rel_error_from_missing_cov




imputed_data <- FI_test

for(i_row in 1:nrow(imputed_data)) {
  constrained_row <- imputed_data[i_row,]
  if( sum(is.na(constrained_row)) != 0 ) {  # do nothing if nothing is missing
    # Use formula for mean here: http://en.wikipedia.org/wiki/Multivariate_normal_distribution#Conditional_distributions
    cols_to_impute <- which(is.na(constrained_row))    # indices of "1" in Wikipedia formula for mean of conditional multivariate normal distribution
    if( length(cols_to_impute) == length(constrained_row) ) {
      # nothing to condition on
      replacement_values <- FI_means
    } else {
      # fill based on conditional normal distribution
      known_cols <- setdiff(1:ncol(imputed_data), cols_to_impute)  # incides of "2" in Wikipedia formula for mean of conditional multivariate normal distribution
      
      replacement_values <- t(t(FI_means[cols_to_impute])) + 
        FI_covariance[cols_to_impute,known_cols, drop=FALSE] %*% 
        solve(a=FI_covariance[known_cols,known_cols], 
              b=t(t(constrained_row[known_cols] - FI_means[known_cols])))
    }
    # Store replacement values (note that constraints are not yet applied)
    imputed_data[i_row,cols_to_impute] <- replacement_values ### PERHAPS ADD as.vector to RHS
  }
}

# diagnostics of mvn imputation
imputed_cells <- is.na(FI_test)
imputed_values <- imputed_data[imputed_cells]
true_values <- FI_true[imputed_cells]

rmse <- sqrt(mean((imputed_values - true_values)^2))
rmse
rel_error_mvn <- abs((imputed_values - true_values) / true_values)
mean_rel_error_mvn <- mean(rel_error_mvn)
mean_rel_error_mvn
median_rel_error <- median( rel_error_mvn )
median_rel_error
median(true_values)
hist(true_values)
hist(imputed_values)

residuals <- imputed_values - true_values
hist(residuals)
plot(true_values, residuals)
abline(lm(residuals ~ true_values))
summary(lm(residuals ~ true_values))
# This residuals plot is the big deal.  This shows that mvn imputed values are
# consistently not extreme enough.

plot(true_values, imputed_values)
cor(true_values, imputed_values)
# This shows that (1) imputed values are small, and (2) they have little to do with true values

# rel size imputed values to true values
rel_size_mvn <- imputed_values / true_values
mean(rel_size_mvn)
median(rel_size_mvn)
hist(rel_size_mvn, 500, xlim=c(-2, 2))

# Impute with column means
imputed_w_col_means <- FI_test
for(j in 1:ncol(imputed_w_col_means)) {
  imputed_w_col_means[is.na(imputed_w_col_means[,j]),j] <- FI_means[j]
}
# diagnostics of column mean imputation
imputed_values_col_means <- imputed_w_col_means[imputed_cells]
sqrt(mean((imputed_values_col_means - true_values)^2))  # rmse
rel_error_col_means <- abs((imputed_values_col_means - true_values) / true_values)
mean( rel_error_col_means )
median( rel_error_col_means )

rel_size_col_means <- imputed_values_col_means / true_values
mean( rel_size_col_means )
median( rel_size_col_means )
hist(rel_size_col_means)

# RMSE is better for multivariate normal but 
# avg relative error is half for column means compared to multivariate normal
# median relative error is better for multivariate normal

hist(abs((imputed_values - true_values) / true_values), main="Multivariate Normal Relative Error")
hist(abs((imputed_values_col_means - true_values) / true_values), main="Multivariate Normal Relative Error")



