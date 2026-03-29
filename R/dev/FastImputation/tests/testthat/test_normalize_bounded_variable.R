context("normalize_bounded_variable")

test_that("normalize_bounded_variable returns numeric(0) for NULL input", {
  expect_equal(normalize_bounded_variable(NULL, list()), numeric(0))
})

test_that("normalize_bounded_variable applies normal quantile transform within bounds", {
  x <- c(0, 0.5, 1)
  normalized <- normalize_bounded_variable(x, list(lower = 0, upper = 1))
  expect_equal(normalized, c(-5, 0, 5), tolerance = 1e-4)
})

test_that("normalize_bounded_variable returns 0 for midpoint of symmetric bounds", {
  normalized <- normalize_bounded_variable(c(5), list(lower = 0, upper = 10))
  expect_equal(normalized, 0, tolerance = 1e-4)
})

test_that("normalize_bounded_variable trims and transforms values outside bounds", {
  normalized <- suppressWarnings(
    normalize_bounded_variable(c(-5, 15), list(lower = 0, upper = 10))
  )
  expect_equal(normalized[1], -5.426934, tolerance = 1e-4)
  expect_equal(normalized[2], 5.426934, tolerance = 1e-4)
})

test_that("normalize_bounded_variable handles NA values", {
  normalized <- normalize_bounded_variable(c(0, NA, 10), list(lower = 0, upper = 10))
  expect_equal(normalized[1], -5.426934, tolerance = 1e-4)
  expect_true(is.na(normalized[2]))
  expect_equal(normalized[3], 5.426934, tolerance = 1e-4)
})

test_that("normalize_bounded_variable handles infinite values outside bounds", {
  normalized <- suppressWarnings(
    normalize_bounded_variable(c(-Inf, Inf), list(lower = 0, upper = 10))
  )
  expect_equal(normalized[1], -5.426934, tolerance = 1e-4)
  expect_equal(normalized[2], 5.426934, tolerance = 1e-4)
})

test_that("normalize_bounded_variable errors when constraints is not a list", {
  expect_error(
    normalize_bounded_variable(c(1, 2), "not a list"),
    "`constraints` must be a named list"
  )
})

test_that("normalize_bounded_variable errors when upper is less than lower", {
  expect_error(
    normalize_bounded_variable(c(1, 2), list(lower = 5, upper = 0)),
    "upper.*must be greater than.*lower"
  )
})

test_that("normalize_bounded_variable warns when values are trimmed to boundary", {
  expect_warning(
    normalize_bounded_variable(c(-1, 3), list(lower = 0, upper = 5)),
    "1 value"
  )
})

test_that("normalize_bounded_variable errors when trim=FALSE and value is below lower bound", {
  expect_error(
    normalize_bounded_variable(c(-1, 3), list(lower = 0, upper = 5), trim = FALSE),
    "greater than or equal to the lower bound"
  )
})

test_that("normalize_bounded_variable errors when trim=FALSE and value is above upper bound", {
  expect_error(
    normalize_bounded_variable(c(3, 6), list(lower = 0, upper = 5), trim = FALSE),
    "less than or equal to the upper bound"
  )
})

test_that("normalize_bounded_variable errors when tol is larger than half the interval", {
  expect_error(
    normalize_bounded_variable(c(0.5), list(lower = 0, upper = 1), tol = 0.6),
    "tol.*must be less than half"
  )
})

test_that("normalize_bounded_variable returns x unchanged when unbounded", {
  x <- c(1, 2, 3)
  out <- normalize_bounded_variable(x, list())
  expect_equal(out, x)
})
