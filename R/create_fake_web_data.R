# Generate fake data similar to real data for testing and development

library(mvtnorm) # provides rmvnorm for multivariate normal draws

# Read in real data (first 10 columns only)
real_data <- read.table("rf.tsv", header = TRUE, sep = "\t", na.strings = "?")[,
  1:10
]

# Assign meaningful column names
norm_real_data <- real_data
names(norm_real_data) <- c(
  "cust_id",
  "order_id",
  "is_fraud",
  "customer_age_yrs",
  "spent_days_0to2",
  "spent_days_3to10",
  "spent_days_11to30",
  "geo_ip_fraud_rate",
  "account_age_days",
  "days_to_first_purchase"
)
head(norm_real_data) # Preview the data

# Normalize customer IDs to sequential integers
norm_real_data$cust_id <- sapply(norm_real_data$cust_id, function(real_id) {
  which(real_id == unique(norm_real_data$cust_id))
})

# Assign sequential order IDs
norm_real_data$order_id <- 1:nrow(norm_real_data)

# Log-transform and normalize relevant columns for modeling
norm_real_data$customer_age_yrs <- log(norm_real_data$customer_age_yrs)
norm_real_data$spent_days_0to2 <- log(pmax(
  round(norm_real_data$spent_days_0to2, 2),
  .01
))
norm_real_data$spent_days_3to10 <- log(pmax(
  round(norm_real_data$spent_days_3to10, 2),
  .01
))
norm_real_data$spent_days_11to30 <- log(pmax(
  round(norm_real_data$spent_days_11to30, 2),
  .01
))
norm_real_data$geo_ip_fraud_rate <- qnorm(pmin(
  pmax(norm_real_data$geo_ip_fraud_rate, .01),
  .99
))
norm_real_data$account_age_days <- log(pmax(norm_real_data$account_age_days, 1))
norm_real_data$days_to_first_purchase <- log(pmax(
  norm_real_data$days_to_first_purchase,
  1
))

# Select columns to use for imputation/fake data generation
norm_real_imp_data <- norm_real_data[, 3:10]

# Compute means and covariance for multivariate normal simulation
real_norm_means <- colMeans(norm_real_imp_data)
real_norm_cov <- cov(norm_real_imp_data, use = "complete.obs")

# Generate fake normalized data using multivariate normal distribution
norm_fake_data <- as.data.frame(rmvnorm(
  nrow(norm_real_data),
  mean = real_norm_means,
  sigma = real_norm_cov
))
names(norm_fake_data) <- c(
  "is_fraud",
  "customer_age_yrs",
  "spent_days_0to2",
  "spent_days_3to10",
  "spent_days_11to30",
  "geo_ip_fraud_rate",
  "account_age_days",
  "days_to_first_purchase"
)
norm_fake_data <- data.frame(norm_real_data[, 1:2], norm_fake_data)

# Helper function: limit values to closest in a set (for categorical variables)
limit_to_set <- function(x, set) {
  return(sapply(x, function(this_x) {
    index_winner <- which.min(abs(this_x - set))
    return(set[index_winner])
  }))
}

# Helper function: randomly set some values to NA (simulate missingness)
make_some_data_missing <- function(x, prob_dropped = .1) {
  return(as.data.frame(
    apply(x, c(1, 2), function(this_cell) {
      return(ifelse(runif(1) < prob_dropped, NA, this_cell))
    })
  ))
}

fake_data <- norm_fake_data

# Transform normalized fake data back to original scale
fake_data$is_fraud <- limit_to_set(norm_real_data$is_fraud, set = c(0:1))
fake_data$customer_age_yrs <- exp(norm_fake_data$customer_age_yrs)
fake_data$spent_days_0to2 <- round(exp(norm_fake_data$spent_days_0to2), 2)
fake_data$spent_days_3to10 <- round(exp(norm_fake_data$spent_days_3to10), 2)
fake_data$spent_days_11to30 <- round(exp(norm_fake_data$spent_days_11to30), 2)
fake_data$geo_ip_fraud_rate <- pnorm(norm_fake_data$geo_ip_fraud_rate)
fake_data$account_age_days <- pmax(
  round(exp(norm_fake_data$account_age_days)),
  1
)
fake_data$days_to_first_purchase <- pmax(
  round(exp(norm_fake_data$days_to_first_purchase)),
  1
)

# Save the generated fake data for later use
saveRDS(fake_data, file = "fake.data.rds")
# To load: fake_data <- readRDS(file="fake.data.rds")

# Create training and test datasets for the package
FItrain <- fake_data[1:10000, ]
FItest <- FItrue <- fake_data[10001:10010, ]
# Introduce missing values in test set
FItest <- cbind(FItrue[, 1:3], make_some_data_missing(FItrue[, 4:10], .2))

# Save datasets to package data directory
save(FItrain, file = "dev/FastImputation/data/FItrain.RData")
save(FItest, file = "dev/FastImputation/data/FItest.RData")
save(FItrue, file = "dev/FastImputation/data/FItrue.RData")
