context("train_fast_imputation")

set.seed(42)
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
good_constraints_both  <- list("X5" = list(lower = 0, upper = 1))

bad_array <- matrix(1:9, nrow = 3)

test_that("train_fast_imputation catches bad input", {
  expect_error(train_fast_imputation(x = bad_array), "'x' must be a data frame.")
})

test_that("train_fast_imputation returns correct types", {
  res <- train_fast_imputation(
    x           = good_df,
    constraints = good_constraints_both,
    ignore_cols = c(X1, X2),
    categorical = X6
  )
  expect_s3_class(res, "fast_imputation_patterns")
  expect_true(is.list(res))
  expect_true(is.numeric(res$means))
  expect_true(is.vector(res$means))
  expect_true(det(as.matrix(res$covariance)) > 0)
  expect_true(is.list(res$constraints))
  expect_true(is.numeric(res$cols_categorical))
})

test_that("train_fast_imputation returns correct values", {
  res_1 <- train_fast_imputation(
    x           = good_df,
    constraints = good_constraints_upper,
    ignore_cols = c(X1, X2),
    categorical = X6
  )
  res_2 <- train_fast_imputation(
    x           = good_df,
    constraints = good_constraints_lower,
    ignore_cols = c(X2, X1),
    categorical = X6
  )
  expect_null(res_1$constraints[[1]])
  expect_equal(res_1$constraints[[5]], list(upper = 3))
  expect_equal(res_1$cols_to_ignore, c(1L, 2L))
  expect_equal(res_2$cols_to_ignore, c(2L, 1L))
  expect_equal(res_1$cols_categorical, 4L)
  expect_equal(res_2$cols_categorical, 4L)
})

test_that("train_fast_imputation accepts tidyselect helpers", {
  res <- train_fast_imputation(
    x           = good_df,
    ignore_cols = tidyselect::all_of(c("X1", "X2")),
    categorical = X6
  )
  expect_s3_class(res, "fast_imputation_patterns")
  expect_true("X1" %in% res$var_names[res$cols_to_ignore])
})

test_that("train_fast_imputation accepts tibble input", {
  tbl <- tibble::as_tibble(good_df)
  res <- train_fast_imputation(
    x           = tbl,
    constraints = good_constraints_both,
    ignore_cols = c(X1, X2),
    categorical = X6
  )
  expect_s3_class(res, "fast_imputation_patterns")
})
