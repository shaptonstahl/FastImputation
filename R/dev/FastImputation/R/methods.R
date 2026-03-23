#' Print a fast_imputation_patterns object
#'
#' @param x An object of class \code{fast_imputation_patterns}.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.fast_imputation_patterns <- function(x, ...) {
  n_vars <- length(x$var_names)
  n_ignored <- length(x$cols_to_ignore)
  n_active <- n_vars - n_ignored
  n_categorical <- length(x$cols_categorical)
  n_bounded <- length(x$cols_bound)

  cat("<fast_imputation_patterns>\n")
  cat("  Variables :", n_vars, "(", n_active, "active,", n_ignored, "ignored )\n")
  cat("  Categorical:", n_categorical, "\n")
  cat("  Bounded    :", n_bounded, "\n")
  invisible(x)
}

#' Tidy a fast_imputation_patterns object into a tibble
#'
#' Returns one row per variable in the original training data (before
#' one-hot encoding). Categorical variables are shown as a single row
#' with \code{mean = NA}; their dummy-encoded means live inside the
#' covariance model and are not surfaced here.
#'
#' @param x An object of class \code{fast_imputation_patterns}.
#' @param ... Ignored.
#' @return A \code{\link[tibble]{tibble}} with columns \code{variable},
#'   \code{mean}, \code{is_categorical}, \code{bound_lower},
#'   \code{bound_upper}, \code{is_ignored}.
#' @importFrom generics tidy
#' @export
tidy.fast_imputation_patterns <- function(x, ...) {
  # Active variable names: original names minus ignored columns
  # (pre one-hot-encoding, so length matches cols_categorical / cols_bound)
  active_indices <- setdiff(seq_along(x$var_names), x$cols_to_ignore)
  active_names   <- x$var_names[active_indices]
  n_active       <- length(active_names)

  # Non-categorical active variable names (these appear in x$means by name)
  non_cat_mask  <- !seq_len(n_active) %in% x$cols_categorical
  non_cat_names <- active_names[non_cat_mask]

  # Look up means by name (post-encoding means vector is named)
  active_means <- vapply(active_names, function(nm) {
    if (nm %in% non_cat_names) unname(x$means[nm]) else NA_real_
  }, numeric(1))

  # Bounds are indexed by position in the active (pre-encoding) data frame
  active_lower <- vapply(seq_len(n_active), function(i) {
    con <- x$constraints[[i]]
    if (is.null(con) || is.null(con$lower)) NA_real_ else con$lower
  }, numeric(1))

  active_upper <- vapply(seq_len(n_active), function(i) {
    con <- x$constraints[[i]]
    if (is.null(con) || is.null(con$upper)) NA_real_ else con$upper
  }, numeric(1))

  active_tbl <- tibble::tibble(
    variable       = active_names,
    mean           = active_means,
    is_categorical = seq_len(n_active) %in% x$cols_categorical,
    bound_lower    = active_lower,
    bound_upper    = active_upper,
    is_ignored     = FALSE
  )

  if (length(x$cols_to_ignore) > 0) {
    ignored_tbl <- tibble::tibble(
      variable       = x$var_names[x$cols_to_ignore],
      mean           = NA_real_,
      is_categorical = FALSE,
      bound_lower    = NA_real_,
      bound_upper    = NA_real_,
      is_ignored     = TRUE
    )
    result <- dplyr::bind_rows(active_tbl, ignored_tbl)
    # Re-order rows to match original column order
    order_idx <- order(c(active_indices, x$cols_to_ignore))
    result <- result[order_idx, ]
  } else {
    result <- active_tbl
  }

  result
}
