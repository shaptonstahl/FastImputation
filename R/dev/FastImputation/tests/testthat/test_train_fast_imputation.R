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
good_constraints_both <- list("X5" = list(lower = 0, upper = 1))

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

test_that("train_fast_imputation stores ignored columns", {
  df <- data.frame(a = c(1, 2, NA), b = c(4, NA, 6), c = c(7, 8, 9))
  patterns <- train_fast_imputation(df, ignore_cols = c("c"))
  expect_equal(length(patterns$cols_to_ignore), 1)
})

test_that("train_fast_imputation stores categorical columns with all_of selector", {
  df2 <- data.frame(a = c(1, 2, NA), b = c("x", "y", "x"), stringsAsFactors = FALSE)
  patterns <- train_fast_imputation(df2, categorical = tidyselect::all_of("b"))
  expect_equal(length(patterns$cols_categorical), 1)
  expect_equal(patterns$cols_categorical, 2)
})

test_that("train_fast_imputation stores bounded columns with exact positions", {
  df <- data.frame(a = c(1, 2, NA), b = c(4, NA, 6), c = c(7, 8, 9))
  patterns <- train_fast_imputation(
    df,
    constraints = list(a = list(lower = 0), b = list(upper = 10))
  )
  expect_equal(patterns$cols_bound, c(1, 2))
})

test_that("train_fast_imputation handles numeric-valued categorical columns", {
  df <- data.frame(a = c(1, 2, 3), b = c(4L, 5L, 6L))
  patterns <- train_fast_imputation(df, categorical = b)
  expect_s3_class(patterns, "fast_imputation_patterns")
  expect_equal(patterns$cols_categorical, 2L)
})

test_that("train_fast_imputation errors on non-numeric columns not in categorical", {
  df <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  expect_error(
    train_fast_imputation(df),
    "non-numeric"
  )
})
