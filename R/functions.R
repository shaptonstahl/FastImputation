# FastImputation
#
# Normal imputation requires running a slow batch process on
# an entire dataset in order to fill in missing values.
# This package will allow fast imputation of single rows
# by using a training dataset to "learn" how to fill
# in missing values given the observed values.
# Imputation on a single row is expected to be very fast
# relative to other imputation methods.

# The saved file stores the column means and covariance matrix of
# the training data.

TrainFastImputation <- function(
  x, # dataframe containing training data. Can have incomplete rows.
  silent = FALSE # When TRUE no status messages are displayed.
) {
  if ("data.frame" != class(x)) stop("Training data must be in a data.frame")

  if (!silent) require("time", character.only = TRUE)

  # Need to unfactor the columns

  FastImputationMeans <- colMeans(x, na.rm = TRUE)
  FastImputationCovariance <- cov(x, use = "complete.obs")

  patterns <- list(
    FImeans = FastImputationMeans,
    FIcovariance = FastImputationCovariance
  )
  class(patterns) <- "FastImputationPatterns"
  return(patterns)
}
# TFI.little <- TrainFastImputation(little.x)
# TFI.little[1:30,]

ConstrainValues <- function(
  x,
  constraints # A list of constraints.  Examples:
  # constraints=list(lower=5)           # lower bound when constrining to an interval
  # constraints=list(upper=10)          # upper bound when constraining to an interval
  # constraints=list(lower=5, upper=10)
  # constraints=list(set=c(1:7))        # numeric vector containing a fixed set of values
) {
  if (is.null(constraints$lower)) constraints$lower <- -Inf
  if (is.null(constraints$upper)) constraints$upper <- Inf
  if (constraints$upper < constraints$lower)
    stop("'upper' must be greater than 'lower.'")

  if (!is.null(constraints$set)) {
    out <- sapply(x, function(this.x) {
      index.winner <- which.min(abs(this.x - constraints$set))
      return(constraints$set[index.winner])
    })
  } else {
    out <- sapply(x, function(this.x) {
      return(max(c(constraints$lower, min(c(constraints$upper, this.x)))))
    })
  }
}
# test.values <- seq(from=-2, to=2, by=.1)
# plot(cbind(test.values, ConstrainValues(test.values, constraints=list(set=-3:3))))
# plot(cbind(test.values, ConstrainValues(test.values, constraints=list(lower=-1))))
# plot(cbind(test.values, ConstrainValues(test.values, constraints=list(upper=1))))
# plot(cbind(test.values, ConstrainValues(test.values, constraints=list(lower=-1, upper=1))))

FastImputation <- function(
  x, # Dataframe with rows that need imputation, or a vector corresponding to a single row to impute.
  patterns, # patterns object passed to itself when imputing more than one row
  constraints # a list with pairs of column numbers and constraints
  # formatted for ConstrainValues.  Examples:
  #  constraints=list(list(1, list(set=-3)),
  #                   list(3, list(lower=-1)),
  #                   list(7, list(upper=1)),
  #                   list(10, list(lower=-1, upper=1)))
  #  constraints=list(list(1, list(set=-3)), list(3, list(lower=-1)), list(7, list(upper=1)), list(10, list(lower=-1, upper=1)))
) {
  if (missing(patterns)) {
    # Look for existing patterns object
    if (exists(patterns) && class(patterns) == "FastImputationPatterns") {
      # Use existing patterns object
    } else {
      stop("A 'patterns' object must be specified.")
    }
  } else {
    # patterns explicitly passed to function
    if (class(patterns) != "FastImputationPatterns")
      stop("'patterns' must be of class 'FastImputationPatterns'")
  }
  if (is.vector(x) && is.numeric(x)) x <- as.data.frame(t(x))
  if (!is.data.frame(x) && is.matrix(x)) x <- as.data.frame(x)
  if (!is.data.frame(x)) stop("'x' must be a data.frame or matrix.")

  n.cols <- length(patterns$FImeans)
  n.rows <- nrow(x)

  require("time", character.only = TRUE)
  n.current.bars <- progressBar()

  cols.to.constrain <- sapply(constraints, function(this.cons) this.cons[[1]])

  for (i.row in 1:n.rows) {
    # Use formula for mean here: http://en.wikipedia.org/wiki/Multivariate_normal_distribution#Conditional_distributions
    cols.to.impute <- which(is.na(x[i.row, ])) # indices of "1" in Wikipedia formula for mean of conditional multivariate normal distribution
    known.cols <- which(!is.na(x[i.row, ])) # incides of "2" in Wikipedia formula for mean of conditional multivariate normal distribution

    replacement.values <- t(
      patterns$FImeans[cols.to.impute] +
        patterns$FIcovariance[cols.to.impute, known.cols, drop = FALSE] %*%
          solve(
            a = patterns$FIcovariance[known.cols, known.cols],
            b = t(x[i.row, known.cols, drop = FALSE]) -
              t(t(patterns$FImeans[known.cols]))
          )
    )
    x[i.row, cols.to.impute] <- replacement.values

    imputed.cols.to.constrain <- intersect(cols.to.constrain, cols.to.impute)
    for (this.col in cols.to.constrain) {
      this.constraint <- constraints[[which(cols.to.constrain == this.col)]][[
        2
      ]]
      x[i.row, this.col] <- ConstrainValues(
        x[i.row, this.col],
        constraints = this.constraint
      )
    }

    n.current.bars <- progressBar(i.row / n.rows, prev = n.current.bars)
    flush.console()
  }
  cat("\n")
  return(x)
}
