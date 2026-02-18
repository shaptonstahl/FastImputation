context("TrainFastImputation")

good_df <- data.frame(
  X1 = letters,
  X2 = 1:26,
  X3 = rnorm(26),
  X4 = rexp(26),
  X5 = runif(26),
  X6 = sample(c(TRUE, FALSE), 26, replace = TRUE),
  X7 = 3 - rexp(26)
)
good_constraints_upper <- list("X7" = list(upper = 3))
good_constraints_lower <- list("X4" = list(lower = 0))
good_constraints_both <- list("X5" = list(lower = 0, upper = 1))
good_ignore_1 <- 1:2
good_ignore_2 <- c("X2", "X1")
good_categorical_1 <- 6
good_categorical_2 <- "X6"

warning_df <- data.frame(
  X3 = rnorm(5),
  X6 = sample(c(TRUE, FALSE), 5, replace = TRUE)
)
bad_array <- matrix(1:9, nrow = 3)
bad_constraints_upper <- list("X4" = list(upper = 0))
bad_constraints_lower <- list("X7" = list(lower = 4))
bad_constraints_order <- list("X5" = list(lower = 1, upper = 0))
bad_ignore_1 <- 10
bad_ignore_2 <- "X10"
bad_categorical_1 <- 10
bad_categorical_2 <- "X10"

test_that("TrainFastImputation catches bad input", {
  expect_error(TrainFastImputation(x = bad_array), "'x' must be a dataframe.")
})

test_that("TrainFastImputation returns correct types", {
  res <- TrainFastImputation(
    x = good_df,
    constraints = good_constraints_both,
    ignore_cols = good_ignore_2
  )
  expect_true(is.list(res))
  expect_true(is.numeric(res$FI_means))
  expect_true(is.vector(res$FI_means))
  expect_true(det(res$FI_covariance) > 0)
  expect_true(is.list(res$FI_constraints))
  expect_true(is.numeric(res$FI_categorical))
})

test_that("TrainFastImputation returns correct values", {
  res_1 <- TrainFastImputation(
    x = good_df,
    constraints = good_constraints_upper,
    ignore_cols = good_ignore_1,
    categorical = good_categorical_1
  )
  res_2 <- TrainFastImputation(
    x = good_df,
    constraints = good_constraints_lower,
    ignore_cols = good_ignore_2,
    categorical = good_categorical_2
  )
  expect_equal(res_1$FI_constraints[[1]], list())
  expect_equal(res_1$FI_constraints[[5]], list(upper = 3))
  expect_equal(res_1$FI_ignore_cols, c(1, 2))
  expect_equal(res_2$FI_ignore_cols, c(1, 2))
  expect_equal(res_1$FI_categorical, 4)
  expect_equal(res_2$FI_categorical, 4)
})
