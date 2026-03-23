#' Learn from training data so that later you can fill in missing data
#'
#' Like Amelia, FastImputation assumes that the columns of the data are
#' multivariate normal or can be transformed into approximately
#' multivariate normal.
#'
#' @param x A data frame (or tibble) containing training data. May have
#'   incomplete rows.
#' @param constraints A named list of bound constraints. Each element is named
#'   after a column in \code{x} and is itself a list with optional
#'   \code{lower} and/or \code{upper} numeric elements.
#' @param ignore_cols <[`tidy-select`][tidyselect::language]> Columns to
#'   exclude from imputation (e.g. ID columns). These columns are preserved
#'   in the output unchanged.
#' @param categorical <[`tidy-select`][tidyselect::language]> Columns that
#'   contain a finite set of discrete values (factors/characters). These are
#'   one-hot encoded internally.
#' @return An object of class \code{fast_imputation_patterns} that contains
#'   all information needed to impute missing values with
#'   \code{\link{fast_imputation}}.
#' @export
#' @seealso \code{\link{fast_imputation}}
#' @references \url{https://gking.harvard.edu/amelia}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(FI_train)
#'
#' patterns <- train_fast_imputation(
#'   FI_train,
#'   constraints = list(
#'     bounded_below_2           = list(lower = 0),
#'     bounded_above_5           = list(upper = 0),
#'     bounded_above_and_below_6 = list(lower = 0, upper = 1)
#'   ),
#'   ignore_cols = user_id_1,
#'   categorical = categorical_9
#' )
#'
train_fast_imputation <-
  function(
    x,
    constraints = list(),
    ignore_cols = NULL,
    categorical = NULL
  ) {
    if (!inherits(x, "data.frame"))
      rlang::abort("'x' must be a data frame.")

    # Resolve tidyselect expressions to integer positions
    ignore_quo      <- rlang::enquo(ignore_cols)
    categorical_quo <- rlang::enquo(categorical)

    if (rlang::quo_is_null(ignore_quo)) {
      ignore_pos <- integer(0)
    } else {
      ignore_pos <- tidyselect::eval_select(ignore_quo, data = x)
    }

    if (rlang::quo_is_null(categorical_quo)) {
      categorical_pos <- integer(0)
    } else {
      categorical_pos <- tidyselect::eval_select(categorical_quo, data = x)
    }

    categorical_names <- names(categorical_pos)

    # Store original variable names
    var_names <- names(x)

    # Remove ignored columns before processing
    if (length(ignore_pos) > 0) {
      x <- x[, -ignore_pos, drop = FALSE]
    }

    # Convert specified columns to factor to capture levels
    for (col_name in categorical_names) {
      x[[col_name]] <- as.factor(x[[col_name]])
    }

    # Capture category levels before converting factors to character/numeric
    categories <- lapply(categorical_names, function(col_name) levels(x[[col_name]]))

    # Convert factor columns to character or numeric
    x[] <- lapply(x, function(col) {
      if (!is.factor(col)) return(col)
      ch <- as.character(col)
      suppressWarnings(num <- as.numeric(ch))
      if (!any(is.na(num)) && length(num) == length(ch)) num else ch
    })

    # Store categorical column positions (relative to x with ignored cols removed)
    cols_categorical <- which(names(x) %in% categorical_names)

    # Normalize variables bounded to an interval
    for (this_col in names(constraints)) {
      x[, this_col] <- normalize_bounded_variable(
        x[, this_col],
        constraints[[this_col]]
      )
    }

    # Store bounded column positions and build constraints list indexed by position
    cols_bound <- which(names(x) %in% names(constraints))
    constraints_by_pos <- vector("list", ncol(x))
    for (col_name in names(constraints)) {
      i <- which(names(x) == col_name)
      if (length(i) > 0) constraints_by_pos[[i]] <- constraints[[col_name]]
    }

    # Encode categorical variables as dummies and remove originals
    if (length(categorical_names) > 0) {
      x_with_dummies <- fastDummies::dummy_cols(x, select_columns = categorical_names)
      x <- x_with_dummies[, !names(x_with_dummies) %in% categorical_names, drop = FALSE]
    }

    # Guard: all remaining columns must be numeric for MVN estimation
    non_numeric <- names(x)[!vapply(x, is.numeric, logical(1))]
    if (length(non_numeric) > 0) {
      rlang::abort(paste0(
        "The following columns are non-numeric and cannot be used in the MVN model: ",
        paste(non_numeric, collapse = ", "), ". ",
        "Pass them to `ignore_cols` (to carry through unchanged) or ",
        "`categorical` (to one-hot encode them)."
      ))
    }

    means <- colMeans(x, na.rm = TRUE)
    covariance <- covariance_with_missing(x)

    # Ensure covariance matrix is well-conditioned.
    # One-hot encoding with dummies for all category values can make the
    # covariance matrix singular or nearly so. This forces the covariance
    # matrix to the "closest" one with only eigenvalues < min_size_eigenvalue
    # being affected.
    min_size_eigenvalue <- .01
    eg <- eigen(covariance)$values
    if (min(eg) < min_size_eigenvalue) {
      eps_ev <- min_size_eigenvalue / abs(eg[1])
      covariance <- Matrix::nearPD(
        covariance,
        do2eigen = TRUE,
        posd.tol = eps_ev
      )$mat
    }

    patterns <- list(
      var_names        = var_names,
      means            = means,
      covariance       = covariance,
      constraints      = constraints_by_pos,
      cols_to_ignore   = unname(ignore_pos),
      cols_bound       = cols_bound,
      cols_categorical = cols_categorical,
      categories       = categories
    )
    class(patterns) <- "fast_imputation_patterns"
    return(patterns)
  }
