#' @keywords internal
covariance_with_missing <- function(x) {
  stopifnot(
    methods::is(x, "matrix") |
      (methods::is(x, "data.frame") && is.numeric(as.matrix(x)))
  )

  # probability a cell is observed, proportion of observed entries
  delta <- mean(!is.na(x))
  if (1 == delta) {
    # no missing data
    out <- stats::cov(x)
  } else {
    # set up for notation of Louncini (2012)
    x <- as.matrix(x)
    x <- t(x) # puts observations in columns, variables in rows
    n <- ncol(x) # number of observations
    p <- nrow(x) # number of variables

    # per page 1 of the article
    y <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
    y[is.na(y)] <- 0

    # Equation at bottom of page 3
    sigma_delta_n <- matrix(0, nrow = p, ncol = p)
    for (i in 1:n) sigma_delta_n <- sigma_delta_n + y[, i] %o% y[, i]
    sigma_delta_n <- sigma_delta_n / n

    # Equation 1.4
    out <- ((delta - 1) * diag(diag(sigma_delta_n)) + sigma_delta_n) / (delta^2)
  }
  # prepare and return the output
  out
}
