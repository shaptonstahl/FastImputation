context("bound_normalized_variable")

test_that("bound_normalized_variable catches bad input", {
  good.x.vector <- c(-3:3)
  good.x.matrix <- matrix(-2:3, nrow = 2)
  good.x.df <- data.frame(x = -3:0, y = 0:3)
  good.x.array <- array(c(-3:0, 0:3), dim = c(4, 2))

  bad.x.list <- list(x = 1:3, y = 4:6)

  bad.constraints.order <- list(lower = 1, upper = 0)
  good.constraints.lower <- list(lower = -5)
  good.constraints.upper <- list(upper = 5)
  good.constraints.both <- list(lower = -5, upper = 5)

  expect_error(
    bound_normalized_variable(
      x = good.x.vector,
      constraints = bad.constraints.order
    ),
    "'upper' must be greater than 'lower.'"
  )
  expect_error(
    bound_normalized_variable(
      x = good.x.matrix,
      constraints = bad.constraints.order
    ),
    "'upper' must be greater than 'lower.'"
  )
  expect_error(
    bound_normalized_variable(x = good.x.df, constraints = bad.constraints.order),
    "'upper' must be greater than 'lower.'"
  )
  expect_error(
    bound_normalized_variable(
      x = good.x.array,
      constraints = bad.constraints.order
    ),
    "'upper' must be greater than 'lower.'"
  )
})

test_that("bound_normalized_variable returns correct types", {
  good.x.vector <- c(-3:3)
  good.x.matrix <- matrix(-2:3, nrow = 2)
  good.x.df <- data.frame(x = -3:0, y = 0:3)
  good.x.array <- array(c(-3:0, 0:3), dim = c(4, 2))
  good.constraints.lower <- list(lower = -5)
  good.constraints.upper <- list(upper = 5)
  good.constraints.both <- list(lower = -5, upper = 5)

  expect_true(is.vector(bound_normalized_variable(
    x = good.x.vector,
    constraints = good.constraints.lower
  )))
  expect_true(is.vector(bound_normalized_variable(
    x = good.x.vector,
    constraints = good.constraints.upper
  )))
  expect_true(is.vector(bound_normalized_variable(
    x = good.x.vector,
    constraints = good.constraints.both
  )))

  expect_true(is.matrix(bound_normalized_variable(
    x = good.x.matrix,
    constraints = good.constraints.lower
  )))
  expect_true(is.matrix(bound_normalized_variable(
    x = good.x.matrix,
    constraints = good.constraints.upper
  )))
  expect_true(is.matrix(bound_normalized_variable(
    x = good.x.matrix,
    constraints = good.constraints.both
  )))

  expect_true(is.data.frame(bound_normalized_variable(
    x = good.x.df,
    constraints = good.constraints.lower
  )))
  expect_true(is.data.frame(bound_normalized_variable(
    x = good.x.df,
    constraints = good.constraints.upper
  )))
  expect_true(is.data.frame(bound_normalized_variable(
    x = good.x.df,
    constraints = good.constraints.both
  )))

  expect_true(is.array(bound_normalized_variable(
    x = good.x.array,
    constraints = good.constraints.lower
  )))
  expect_true(is.array(bound_normalized_variable(
    x = good.x.array,
    constraints = good.constraints.upper
  )))
  expect_true(is.array(bound_normalized_variable(
    x = good.x.array,
    constraints = good.constraints.both
  )))
})

test_that("bound_normalized_variable returns correct values", {
  good.x <- c(-4:4)
  good.constraints.lower <- list(lower = -5)
  good.constraints.upper <- list(upper = 5)
  good.constraints.both <- list(lower = -5, upper = 5)

  expect_equal(
    bound_normalized_variable(x = good.x, constraints = good.constraints.lower),
    exp(good.x) + good.constraints.lower$lower
  )
  expect_equal(
    bound_normalized_variable(x = good.x, constraints = good.constraints.upper),
    good.constraints.upper$upper - exp(good.x)
  )
  expect_equal(
    bound_normalized_variable(x = good.x, constraints = good.constraints.both),
    pnorm(good.x) *
      (good.constraints.both$upper - good.constraints.both$lower) +
      good.constraints.both$lower
  )
})

test_that("bound_normalized_variable errors when x is character", {
  expect_error(bound_normalized_variable("a", list(lower = 0)), "x must be numeric")
})

test_that("bound_normalized_variable recovers only-lower-bounded values", {
  out <- bound_normalized_variable(c(-6, 0, 6), list(lower = 0))
  expect_equal(out, c(2.478752e-03, 1.000000e+00, 4.034288e+02), tolerance = 1e-4)
})

test_that("bound_normalized_variable recovers upper-only-bounded values", {
  out <- bound_normalized_variable(c(-6, 0, 6), list(upper = 5))
  expect_equal(out, c(4.997521, 4.000000, -398.428793), tolerance = 1e-4)
})

test_that("bound_normalized_variable recovers only-upper-bound-zero values", {
  out <- bound_normalized_variable(c(-6, 0, 6), list(upper = 0))
  expect_equal(out, c(-2.478752e-03, -1.000000e+00, -4.034288e+02), tolerance = 1e-4)
})

test_that("bound_normalized_variable returns x unchanged when constraints have no lower or upper", {
  out <- bound_normalized_variable(c(-6, 0, 6), list(set = c(0, 5, 10)))
  expect_equal(out, c(-6, 0, 6))
})

test_that("bound_normalized_variable errors when constraints is not a list", {
  expect_error(
    bound_normalized_variable(1, "not_a_list"),
    "constraints.*must be a named list"
  )
})

test_that("bound_normalized_variable errors when x is numeric but not vector/matrix/array", {
  expect_error(
    bound_normalized_variable(ts(c(-6, 0, 6)), list(lower = 0)),
    "x must be a vector, matrix, or array"
  )
})
