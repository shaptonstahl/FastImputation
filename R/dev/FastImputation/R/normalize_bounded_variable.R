#' @keywords internal
normalize_bounded_variable <-
  function(x, constraints, tol = stats::pnorm(-5), trim = TRUE) {
    if (is.data.frame(x)) {
      was.data.frame <- TRUE
      df.names <- names(x)
      x <- as.matrix(x)
    } else {
      was.data.frame <- FALSE
    }

    constraints <- .nbv_validate(constraints, tol)
    x <- .nbv_clip(x, constraints, trim)

    if (is.finite(constraints$lower)) x <- pmax(constraints$lower + tol, x)
    if (is.finite(constraints$upper)) x <- pmin(constraints$upper - tol, x)

    out <- .nbv_transform(x, constraints$lower, constraints$upper)

    if (was.data.frame) {
      out <- as.data.frame(out)
      names(out) <- df.names
    }
    out
  }

# Validate constraints and fill in -Inf/Inf defaults. Returns normalised constraints.
.nbv_validate <- function(constraints, tol) {
  if (!is.list(constraints)) {
    stop("`constraints` must be a named list, e.g. list(lower = 0, upper = 1)")
  }
  if (is.null(constraints$lower)) constraints$lower <- -Inf
  if (is.null(constraints$upper)) constraints$upper <- Inf
  if (constraints$upper < constraints$lower) {
    stop("'upper' must be greater than 'lower.'")
  }
  if (
    is.finite(constraints$lower) &&
      is.finite(constraints$upper) &&
      tol > (constraints$upper - constraints$lower) / 2
  ) {
    stop("`tol` must be less than half the distance between upper and lower bounds.")
  }
  constraints
}

# Clip x to [lower, upper], warning or erroring depending on trim.
.nbv_clip <- function(x, constraints, trim) {
  if (trim) {
    clipped <- x < constraints$lower | x > constraints$upper
    if (any(clipped, na.rm = TRUE)) {
      warning(sum(clipped, na.rm = TRUE), " value(s) were trimmed to the boundary.")
    }
    x <- pmax(x, constraints$lower)
    x <- pmin(x, constraints$upper)
  } else {
    if (min(x) < constraints$lower) {
      stop("All values in x must be greater than or equal to the lower bound.")
    }
    if (max(x) > constraints$upper) {
      stop("All values in x must be less than or equal to the upper bound.")
    }
  }
  x
}

# Apply the appropriate monotone transform based on which bounds are finite.
.nbv_transform <- function(x, lower, upper) {
  lower_finite <- is.finite(lower)
  upper_finite <- is.finite(upper)
  if (!lower_finite && !upper_finite) {
    return(x)
  }
  if (!lower_finite) {
    return(log(upper - x))
  }
  if (!upper_finite) {
    return(log(x - lower))
  }
  stats::qnorm((x - lower) / (upper - lower))
}
