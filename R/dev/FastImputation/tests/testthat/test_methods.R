context("S3 methods for fast_imputation_patterns")

set.seed(1)
df <- data.frame(
  id  = 1:20,
  age = rexp(20, rate = 0.05),
  cat = sample(c("a", "b", "c"), 20, replace = TRUE),
  x   = rnorm(20)
)

patterns <- train_fast_imputation(
  df,
  constraints = list(age = list(lower = 0)),
  ignore_cols = id,
  categorical = cat
)

test_that("print.fast_imputation_patterns outputs without error", {
  expect_output(print(patterns), "fast_imputation_patterns")
  expect_output(print(patterns), "Variables")
  expect_output(print(patterns), "Categorical")
  expect_output(print(patterns), "Bounded")
})

test_that("tidy.fast_imputation_patterns returns a tibble with correct columns", {
  result <- generics::tidy(patterns)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("variable", "mean", "is_categorical", "bound_lower", "bound_upper", "is_ignored") %in% names(result)))
})

test_that("tidy.fast_imputation_patterns has one row per original variable", {
  result <- generics::tidy(patterns)
  expect_equal(nrow(result), ncol(df))
})

test_that("tidy.fast_imputation_patterns marks ignored columns correctly", {
  result <- generics::tidy(patterns)
  expect_true(result$is_ignored[result$variable == "id"])
  expect_false(result$is_ignored[result$variable == "age"])
})

test_that("tidy.fast_imputation_patterns marks categorical columns correctly", {
  result <- generics::tidy(patterns)
  expect_true(result$is_categorical[result$variable == "cat"])
  expect_false(result$is_categorical[result$variable == "age"])
})

test_that("tidy.fast_imputation_patterns records bounds correctly", {
  result <- generics::tidy(patterns)
  expect_equal(result$bound_lower[result$variable == "age"], 0)
  expect_true(is.na(result$bound_upper[result$variable == "age"]))
})
