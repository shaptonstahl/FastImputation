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
  expect_true(all(c(
    "variable", "mean", "is_categorical", "bound_lower",
    "bound_upper", "is_ignored"
  ) %in% names(result)))
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

test_that("tidy on untrained recipe step returns tibble with terms and id", {
  rec <- recipes::recipe(~., data = data.frame(a = c(1, 2, 3), b = c(4, 5, 6))) |>
    step_fast_imputation(everything())
  result <- generics::tidy(rec, number = 1)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("terms", "id") %in% names(result)))
  expect_equal(result$terms[1], "everything()")
})

test_that("tidy on trained recipe step returns variable-level tibble", {
  rec <- recipes::recipe(~., data = data.frame(a = c(1, 2, 3), b = c(4, 5, 6))) |>
    step_fast_imputation(everything())
  prepped <- recipes::prep(rec)
  result2 <- generics::tidy(prepped, number = 1)
  expect_s3_class(result2, "tbl_df")
  expect_true(all(c(
    "variable", "mean", "is_categorical", "bound_lower",
    "bound_upper", "is_ignored"
  ) %in% names(result2)))
  expect_equal(nrow(result2), 2)
})

test_that("tidy.fast_imputation_patterns records upper bounds correctly", {
  p <- train_fast_imputation(
    data.frame(a = 1:10, b = rnorm(10)),
    constraints = list(b = list(upper = 5))
  )
  result <- generics::tidy(p)
  expect_equal(result$bound_upper[result$variable == "b"], 5)
})
