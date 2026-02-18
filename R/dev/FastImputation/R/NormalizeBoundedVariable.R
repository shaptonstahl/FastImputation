#' Take a variable bounded above/below/both and return an unbounded (normalized) variable.
#'
#' This transforms bounded variables so that they are not bounded.
#' First variables are coerced away from the boundaries. by a distance of \code{tol}.
#' The natural log is used for variables bounded either above or below but not both.
#' The inverse of the standard normal cumulative distribution function
#'   (the quantile function) is used for variables bounded above and below.
#'
#' @param x A vector, matrix, array, or dataframe with value to be
#'   coerced into a range or set.
#' @param constraints A named list of constraints with optional \code{lower} and/or
#'   \code{upper} elements. See the examples below for formatting details.
#' @param tol Variables will be forced to be at least this far away
#'   from the boundaries.
#' @param trim If TRUE values in x < lower and values in x > upper
#'   will be set to lower and upper, respectively, before normalizing.
#'   A warning is issued if any values are trimmed.
#' @return An object of the same class as \code{x} with the values
#'   transformed so that they spread out over any part of the real
#'   line.
#'
#' A variable \code{x} that is bounded below by \code{lower} is
#'   transformed to \code{log(x - lower)}.
#'
#' A variable \code{x} that is bounded above by \code{upper} is
#'   transformed to \code{log(upper - x)}.
#'
#' A variable \code{x} that is bounded below by \code{lower} and
#'   above by \code{upper} is transformed to
#'   \code{qnorm((x-lower)/(upper - lower))}.
#' @export
#' @examples
#'   NormalizeBoundedVariable(c(5, 7, 10), constraints = list(lower = 0))
#'   NormalizeBoundedVariable(c(1, 3, 5), constraints = list(upper = 10))
#'   NormalizeBoundedVariable(c(5, 7, 9), constraints = list(lower = 0, upper = 10))
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
NormalizeBoundedVariable <-
  function(x, constraints, tol = stats::pnorm(-5), trim = TRUE) {
    if (!is.list(constraints))
      stop("`constraints` must be a named list, e.g. list(lower = 0, upper = 1)")
    if (is.data.frame(x)) {
      was.data.frame <- TRUE
      df.names <- names(x)
      x <- as.matrix(x)
    } else {
      was.data.frame <- FALSE
    }
    if (is.null(constraints$lower)) constraints$lower <- -Inf
    if (is.null(constraints$upper)) constraints$upper <- Inf
    if (constraints$upper < constraints$lower)
      stop("'upper' must be greater than 'lower.'")
    if (trim) {
      clipped <- x < constraints$lower | x > constraints$upper
      if (any(clipped, na.rm = TRUE))
        warning(sum(clipped, na.rm = TRUE), " value(s) were trimmed to the boundary.")
      x <- pmax(x, constraints$lower)
      x <- pmin(x, constraints$upper)
    } else {
      if (min(x) < constraints$lower)
        stop(
          "All values in x must be greater than or equal to the lower bound."
        )
      if (max(x) > constraints$upper)
        stop("All values in x must be less than or equal to the upper bound.")
    }
    if (
      is.finite(constraints$lower) &&
        is.finite(constraints$upper) &&
        tol > (constraints$upper - constraints$lower) / 2
    ) {
      stop(
        "`tol` must be less than half the distance between upper and lower bounds."
      )
    }

    # force values away from boundaries
    if (is.finite(constraints$lower)) x <- pmax(constraints$lower + tol, x)
    if (is.finite(constraints$upper)) x <- pmin(constraints$upper - tol, x)

    if (is.infinite(constraints$lower) && is.infinite(constraints$upper)) {
      # not bounded; degenerate case
      out <- x
    } else if (is.infinite(constraints$lower) && is.finite(constraints$upper)) {
      # only bounded above
      out <- log(constraints$upper - x)
    } else if (is.finite(constraints$lower) && is.infinite(constraints$upper)) {
      # only bounded below
      out <- log(x - constraints$lower)
    } else if (is.finite(constraints$lower) && is.finite(constraints$upper)) {
      # bounded above and below
      out <- stats::qnorm(
        (x - constraints$lower) / (constraints$upper - constraints$lower)
      )
    }
    if (was.data.frame) {
      out <- as.data.frame(out)
      names(out) <- df.names
    }
    return(out)
  }
