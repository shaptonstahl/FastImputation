#' Learn from the training data so that later you can fill in missing data
#'
#' Like Amelia, FastImputation assumes that the columns of the data are
#' multivariate normal or can be transformed into approximately
#' multivariate normal.
#'
#' @param x Dataframe containing training data. Can have incomplete rows.
#' @param constraints A list of constraints.  See the examples below for formatting details.
#' @param ignore_cols A vector of column indices or column names to be ignored in the imputation process.
#' @param categorical A vector of column indices or column names of varaibles with a finite set of possible values.
#' @return An object of class 'FastImputationPatterns' that contains
#'   information needed later to impute on a single row.
#' @export
#' @seealso \code{\link{FastImputation}}
#' @references
#' \url{https://gking.harvard.edu/amelia}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#'
#' data(FI_train)   # provides FI_train dataset
#'
#' patterns_with_constraints <- TrainFastImputation(
#'   FI_train,
#'   constraints=list("bounded_below_2" = list(lower=0),
#'                    "bounded_above_5" = list(upper=0),
#'                    "bounded_above_and_below_6" = list(lower=0, upper=1)
#'                    ),
#'   ignore_cols="user_id_1",
#'   categorical="categorical_9")
#'
TrainFastImputation <-
  function(
    x,
    constraints,
    ignore_cols,
    categorical
  ) {
    if (!is.data.frame(x)) stop("'x' must be a dataframe.")

    CoerceToColumnNames <- function(x) {
      # If passed a vector of indices, convert to names
      # If passed a vector of names, check that they are valid
      # If passed a vector of names that are not valid, remove them
      # If passed a vector of indices that are not valid, remove them
      if (is.numeric(x)) {
        x <- as.vector(na.omit(names(x)))
      } else if (is.character(x)) {
        # Remove invalid column names
        x <- x[x %in% names(x)]
      } else {
        stop(paste0(
          "Invalid type for 'x'. Expected numeric or character."
        ))
      }
      return(x)
    }

    # Coerce categorical to character names if indices were passed
    if (missing(categorical)) {
      categorical <- character(0)
    } else {
      categorical <- CoerceToColumnNames(categorical)
    }
    # Convert columns specified by cols_categorical
    # (vector of characters) to factors
    for (col_name in categorical) {
      x[[col_name]] <- as.factor(x[[col_name]])
    }

    # Convert factor columns to character or numeric
    x[] <- lapply(x, function(col) {
      if (!is.factor(col)) return(col)
      ch <- as.character(col)
      suppressWarnings(num <- as.numeric(ch))
      if (!any(is.na(num)) && length(num) == length(ch)) num else ch
    })

    # tally and remove ignored columns
    if (missing(ignore_cols)) {
      ignore_cols <- numeric(0)
    } else {
      ignore_cols <- CoerceToColumnNames(ignore_cols)
    }

    # Normalize variables bounded to an interval
    for (this_col in names(constraints)) {
      x[, this_col] <- NormalizeBoundedVariable(
        x[, this_col],
        constraints
      )
    }
    # Encode categorical variables as dummies
    # and remove them from the data
    x <- fastDummies::dummy_cols(
      x,
      select_columns = categorical
    )[, !(names(x) %in% categorical)]

    FastImputationMeans <- colMeans(x, na.rm = TRUE)
    FastImputationCovariance <- CovarianceWithMissing(x)

    # Ensure covariance matrix is well-conditioned
    # One-hot encoding with dummies for all category values
    # can make the covariance matrix singular or nearly so.
    # This forces the covariance matrix to the "closest" one
    # with only eigenvalues < min_size_eigenvalue being affected.
    min_size_eigenvalue <- .01
    eg <- eigen(FastImputationCovariance)$values
    if (min(eg) < min_size_eigenvalue) {
      eps_ev <- min_size_eigenvalue / abs(eg[1])
      FastImputationCovariance <- Matrix::nearPD(
        FastImputationCovariance,
        do2eigen = TRUE,
        posd.tol = eps_ev
      )$mat
    }

    patterns <- list(
      # Used for imputation
      FI_means = FastImputationMeans,
      # Used for imputation
      FI_covariance = FastImputationCovariance,
      # Names of columns with bounds
      FI_constraints = constraints,
      # Names of columns to ignore
      FI_ignore_cols = ignore_cols,
      # names of categorical variables
      # (not the dummy variables created by fastDummies)
      FI_categorical = categorical
    )
    class(patterns) <- "FastImputationPatterns"
    return(patterns)
  }
