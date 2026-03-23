normalize_bounded_variable <-
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
