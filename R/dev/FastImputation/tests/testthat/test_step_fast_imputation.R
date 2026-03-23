context("step_fast_imputation")

set.seed(42)
train_df <- data.frame(
  id  = 1:30,
  age = rexp(30, rate = 0.05),
  cat = sample(c("a", "b", "c"), 30, replace = TRUE),
  x   = rnorm(30),
  y   = rnorm(30)
)
test_df <- data.frame(
  id  = 31:33,
  age = c(NA, 10, 20),
  cat = c("a", NA, "b"),
  x   = c(1, NA, 3),
  y   = rnorm(3)
)

test_that("step_fast_imputation returns a recipe", {
  rec <- recipes::recipe(~ ., data = train_df) |>
    step_fast_imputation(
      tidyselect::everything(),
      constraints = list(age = list(lower = 0)),
      ignore_cols = id,
      categorical = cat
    )
  expect_s3_class(rec, "recipe")
})

test_that("prep trains the step", {
  rec <- recipes::recipe(~ ., data = train_df) |>
    step_fast_imputation(
      tidyselect::everything(),
      constraints = list(age = list(lower = 0)),
      ignore_cols = id,
      categorical = cat
    )
  prepped <- recipes::prep(rec, training = train_df)
  step    <- prepped$steps[[1]]
  expect_true(step$trained)
  expect_s3_class(step$patterns, "fast_imputation_patterns")
})

test_that("bake imputes missing values", {
  rec <- recipes::recipe(~ ., data = train_df) |>
    step_fast_imputation(
      tidyselect::everything(),
      constraints = list(age = list(lower = 0)),
      ignore_cols = id,
      categorical = cat
    )
  prepped <- recipes::prep(rec, training = train_df)
  baked   <- recipes::bake(prepped, new_data = test_df)
  expect_false(any(is.na(baked$age)))
  expect_false(any(is.na(baked$x)))
})

test_that("tidy on untrained step returns a tibble", {
  rec <- recipes::recipe(~ ., data = train_df) |>
    step_fast_imputation(tidyselect::everything())
  step <- rec$steps[[1]]
  result <- generics::tidy(step)
  expect_s3_class(result, "tbl_df")
})

test_that("tidy on trained step returns patterns tibble", {
  rec <- recipes::recipe(~ ., data = train_df) |>
    step_fast_imputation(
      tidyselect::everything(),
      ignore_cols = id,
      categorical = cat
    )
  prepped <- recipes::prep(rec, training = train_df)
  result  <- generics::tidy(prepped$steps[[1]])
  expect_s3_class(result, "tbl_df")
  expect_true("variable" %in% names(result))
})
