context("CovarianceWithMissing")

set.seed(42)
n <- 100
x_complete <- matrix(rnorm(n * 4), nrow = n,
                     dimnames = list(NULL, c("w", "x", "y", "z")))
x_missing <- x_complete
x_missing[sample(length(x_complete), size = floor(0.1 * length(x_complete)))] <- NA

mean_rel_err <- function(est, base) mean(abs((est - base) / base))

test_that("CovarianceWithMissing rejects invalid inputs", {
  expect_error(CovarianceWithMissing(data.frame(a = 1:3, b = letters[1:3])))
  expect_error(CovarianceWithMissing(letters))
  expect_error(CovarianceWithMissing(1:5))
  expect_error(CovarianceWithMissing(matrix(letters, nrow = 2)))
  expect_error(CovarianceWithMissing(array(1:27, dim = c(3, 3, 3))))
})

test_that("CovarianceWithMissing returns a numeric matrix of correct size", {
  out <- CovarianceWithMissing(x_missing)
  expect_true(is.matrix(out))
  expect_true(is.numeric(out))
  expect_equal(dim(out), c(4L, 4L))
})

test_that("CovarianceWithMissing preserves column names", {
  out <- CovarianceWithMissing(x_missing)
  expect_equal(rownames(out), c("w", "x", "y", "z"))
  expect_equal(colnames(out), c("w", "x", "y", "z"))
})

test_that("CovarianceWithMissing returns a symmetric matrix", {
  out <- CovarianceWithMissing(x_missing)
  expect_equal(out, t(out))
})

test_that("CovarianceWithMissing matches cov() exactly when data are complete", {
  out <- CovarianceWithMissing(x_complete)
  expect_equal(out, cov(x_complete))
})

test_that("CovarianceWithMissing accepts a numeric data.frame", {
  df <- as.data.frame(x_missing)
  expect_equal(CovarianceWithMissing(df), CovarianceWithMissing(x_missing))
})

test_that("CovarianceWithMissing output is positive-definite", {
  out <- CovarianceWithMissing(x_missing)
  expect_gt(det(out), 0)
})

test_that("CovarianceWithMissing is closer to full-data cov than naive pairwise estimate", {
  cov_full    <- cov(x_complete)
  cov_lounici <- CovarianceWithMissing(x_missing)
  cov_naive   <- var(x_missing, na.rm = TRUE)
  expect_lt(mean_rel_err(cov_lounici, cov_full),
            mean_rel_err(cov_naive, cov_full))
})
