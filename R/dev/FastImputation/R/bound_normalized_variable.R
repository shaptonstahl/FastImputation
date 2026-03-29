bound_normalized_variable <-
  function(x, constraints) {
    if (!is.list(constraints)) {
      stop("`constraints` must be a named list, e.g. list(lower = 0, upper = 1)")
    }
    if (is.data.frame(x)) {
      was.data.frame <- TRUE
      df.names <- names(x)
      x <- as.matrix(x)
    } else {
      was.data.frame <- FALSE
    }
    if (!is.numeric(x)) stop("x must be numeric")
    if (!(is.vector(x) || is.matrix(x) || is.array(x))) {
      stop("x must be a vector, matrix, or array")
    }
    if (is.null(constraints$lower)) constraints$lower <- -Inf
    if (is.null(constraints$upper)) constraints$upper <- Inf
    if (constraints$upper < constraints$lower) {
      stop("'upper' must be greater than 'lower.'")
    }

    if (is.infinite(constraints$lower) && is.infinite(constraints$upper)) {
      # not bounded; degenerate case
      out <- x
    } else if (is.infinite(constraints$lower)) {
      # only bounded above
      out <- constraints$upper - exp(x)
    } else if (is.infinite(constraints$upper)) {
      # only bounded below
      out <- exp(x) + constraints$lower
    } else {
      # bounded above and below
      out <- stats::pnorm(x) *
        (constraints$upper - constraints$lower) +
        constraints$lower
    }
    if (was.data.frame) {
      out <- as.data.frame(out)
      names(out) <- df.names
    }
    out
  }
