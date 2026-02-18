#' Learn from the training data so that later you can fill in missing data
#'
#' Like Amelia, FastImputation assumes that the columns of the data are
#' multivariate normal or can be transformed into approximately
#' multivariate normal.
#'
#' @param x Dataframe containing training data. Can have incomplete rows.
#' @param constraints A named list of constraints.  See the examples below for formatting details.
#' @param ignore_cols A vector of column indices or column names to be ignored in the imputation process.
#' @param categorical A vector of column indices or column names of varaibles with a finite set of possible values.
#' @return An object of class 'FastImputationPatterns' that contains
#'   information needed later to impute on a single row.
#' @importFrom stats na.omit
#' @export
#' @seealso \code{\link{FastImputation}}
#' @references
#' \url{https://gking.harvard.edu/amelia}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' # Small fast example
#' train_df <- data.frame(x = c(1, 2, NA, 4, 5), y = c(2, NA, 6, 8, 10))
#' TrainFastImputation(train_df)
#'
#' \donttest{
#' # Full example with package dataset
#' data(FI_train)
#' patterns_with_constraints <- TrainFastImputation(
#'   FI_train,
#'   constraints=list("bounded_below_2" = list(lower=0),
#'                    "bounded_above_5" = list(upper=0),
#'                    "bounded_above_and_below_6" = list(lower=0, upper=1)
#'                    ),
#'   ignore_cols="user_id_1",
#'   categorical="categorical_9")
#' }
#'
TrainFastImputation <-
  function(
    x,
    constraints,
    ignore_cols,
    categorical
  ) {
    if (!is.data.frame(x)) stop("'x' must be a dataframe.")

    if (missing(constraints)) constraints <- list()

    # Store original variable names (used by FastImputation to validate input)
    FI_var_names <- names(x)

    # Resolve ignore_cols to sorted integer indices in the original x
    if (missing(ignore_cols) || length(ignore_cols) == 0) {
      FI_ignore_cols <- integer(0)
    } else if (is.numeric(ignore_cols)) {
      FI_ignore_cols <- sort(as.integer(ignore_cols[ignore_cols >= 1 & ignore_cols <= ncol(x)]))
    } else if (is.character(ignore_cols)) {
      FI_ignore_cols <- sort(which(names(x) %in% ignore_cols))
    } else {
      stop("'ignore_cols' must be numeric indices or column names.")
    }

    # Resolve categorical to column names in the original x
    if (missing(categorical) || length(categorical) == 0) {
      categorical_names <- character(0)
    } else if (is.numeric(categorical)) {
      categorical_names <- names(x)[as.integer(categorical)]
    } else if (is.character(categorical)) {
      categorical_names <- categorical[categorical %in% names(x)]
    } else {
      stop("'categorical' must be numeric indices or column names.")
    }

    # Convert specified columns to factors so the lapply below treats them as categorical
    for (col_name in categorical_names) {
      x[[col_name]] <- as.factor(x[[col_name]])
    }

    # Convert factor columns to character or numeric
    x[] <- lapply(x, function(col) {
      if (!is.factor(col)) return(col)
      ch <- as.character(col)
      suppressWarnings(num <- as.numeric(ch))
      if (!any(is.na(num)) && length(num) == length(ch)) num else ch
    })

    # Remove ignored columns to form y (the working data frame)
    if (length(FI_ignore_cols) > 0) {
      y <- x[, -FI_ignore_cols, drop = FALSE]
    } else {
      y <- x
    }

    # FI_categorical: indices of categorical columns in y
    FI_categorical <- which(names(y) %in% categorical_names)

    # FI_constraints: list over y columns; each entry is list() or list(lower=...) etc.
    FI_constraints <- vector("list", ncol(y))
    for (i in seq_along(names(y))) {
      col_name <- names(y)[i]
      if (col_name %in% names(constraints)) {
        FI_constraints[[i]] <- constraints[[col_name]]
      } else {
        FI_constraints[[i]] <- list()
      }
    }

    # FI_bound_cols: indices in y with non-empty constraints
    FI_bound_cols <- which(names(y) %in% names(constraints))

    # Normalize bounded variables in y
    for (i_col in FI_bound_cols) {
      col_name <- names(y)[i_col]
      y[, i_col] <- NormalizeBoundedVariable(y[, i_col], constraints[[col_name]])
    }

    # One-hot encode categorical columns to form z; store category levels
    FI_categories <- list()
    if (length(FI_categorical) > 0) {
      for (i in seq_along(FI_categorical)) {
        col_vals <- as.character(y[, FI_categorical[i]])
        FI_categories[[i]] <- sort(unique(col_vals[!is.na(col_vals)]))
      }
      z <- fastDummies::dummy_cols(y, select_columns = categorical_names)
      z <- z[, !(names(z) %in% categorical_names), drop = FALSE]
    } else {
      z <- y
    }

    FastImputationMeans <- colMeans(z, na.rm = TRUE)
    FastImputationCovariance <- CovarianceWithMissing(z)

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
      FI_var_names    = FI_var_names,
      FI_means        = FastImputationMeans,
      FI_covariance   = FastImputationCovariance,
      FI_constraints  = FI_constraints,
      FI_ignore_cols  = FI_ignore_cols,
      FI_bound_cols   = FI_bound_cols,
      FI_categorical  = FI_categorical,
      FI_categories   = FI_categories
    )
    class(patterns) <- "FastImputationPatterns"
    return(patterns)
  }
