#' Imputation step for use in a \code{recipes} pipeline
#'
#' \code{step_fast_imputation} creates a specification of a recipe step that
#' will impute missing values using the conditional multivariate normal
#' approach of \code{\link{train_fast_imputation}} and
#' \code{\link{fast_imputation}}.
#'
#' @param recipe A \code{recipe} object.
#' @param ... <[`tidy-select`][tidyselect::language]> Columns to impute.
#' @param constraints A named list of bound constraints passed to
#'   \code{\link{train_fast_imputation}}.
#' @param ignore_cols <[`tidy-select`][tidyselect::language]> Columns to
#'   ignore during imputation (e.g. ID columns). Resolved to a character
#'   vector at recipe-creation time.
#' @param categorical <[`tidy-select`][tidyselect::language]> Categorical
#'   columns. Resolved to a character vector at recipe-creation time.
#' @param role Not used; included for \code{recipes} API compatibility.
#' @param trained Set automatically by \code{\link[recipes]{prep}}.
#' @param patterns Internal. Populated after \code{\link[recipes]{prep}}.
#' @param skip Passed to \code{\link[recipes]{add_step}}.
#' @param id Unique step identifier.
#' @return An updated \code{recipe}.
#' @export
#' @keywords internal
#' @importFrom recipes add_step rand_id
#' @importFrom generics tidy
#' @examples
#' \dontrun{
#' library(recipes)
#' data(FI_train)
#' data(FI_test)
#'
#' rec <- recipe(~., data = FI_train) |>
#'   step_fast_imputation(
#'     everything(),
#'     constraints = list(bounded_below_2 = list(lower = 0)),
#'     ignore_cols = user_id_1,
#'     categorical = categorical_9
#'   )
#'
#' trained_rec <- prep(rec, training = FI_train)
#' bake(trained_rec, new_data = FI_test)
#' }
step_fast_imputation <- function(
  recipe,
  ...,
  constraints = list(),
  ignore_cols = NULL,
  categorical = NULL,
  role = NA,
  trained = FALSE,
  patterns = NULL,
  skip = FALSE,
  id = recipes::rand_id("fast_imputation")
) {
  # Resolve tidyselect expressions to character vectors NOW using the
  # recipe's template data, so the step object never stores quosures.
  # recipes iterates over all step fields looking for tune() markers and
  # calls rlang::call_name() on each — quosures wrapping symbols fail that.
  template <- recipe$template

  ignore_quo <- rlang::enquo(ignore_cols)
  if (rlang::quo_is_null(ignore_quo)) {
    ignore_names <- character(0)
  } else {
    ignore_names <- names(tidyselect::eval_select(ignore_quo, data = template))
  }

  categorical_quo <- rlang::enquo(categorical)
  if (rlang::quo_is_null(categorical_quo)) {
    categorical_names <- character(0)
  } else {
    categorical_names <- names(tidyselect::eval_select(categorical_quo, data = template))
  }

  recipes::add_step(
    recipe,
    step_fast_imputation_new(
      terms             = rlang::enquos(...),
      constraints       = constraints,
      ignore_cols       = ignore_names,
      categorical       = categorical_names,
      role              = role,
      trained           = trained,
      patterns          = patterns,
      skip              = skip,
      id                = id
    )
  )
}

step_fast_imputation_new <- function(
  terms, constraints, ignore_cols, categorical,
  role, trained, patterns, skip, id
) {
  recipes::step(
    subclass    = "fast_imputation",
    terms       = terms,
    constraints = constraints,
    ignore_cols = ignore_cols, # character vector
    categorical = categorical, # character vector
    role        = role,
    trained     = trained,
    patterns    = patterns,
    skip        = skip,
    id          = id
  )
}

#' @export
#' @importFrom recipes prep
prep.step_fast_imputation <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  training_subset <- training[, col_names, drop = FALSE]

  trained_patterns <- train_fast_imputation(
    training_subset,
    constraints = x$constraints,
    ignore_cols = tidyselect::all_of(x$ignore_cols),
    categorical = tidyselect::all_of(x$categorical)
  )

  step_fast_imputation_new(
    terms       = x$terms,
    constraints = x$constraints,
    ignore_cols = x$ignore_cols,
    categorical = x$categorical,
    role        = x$role,
    trained     = TRUE,
    patterns    = trained_patterns,
    skip        = x$skip,
    id          = x$id
  )
}

#' @export
#' @importFrom recipes bake
bake.step_fast_imputation <- function(object, new_data, ...) {
  all_var_names <- object$patterns$var_names
  new_data_subset <- new_data[, all_var_names, drop = FALSE]

  imputed <- fast_imputation(new_data_subset, object$patterns, verbose = FALSE)

  new_data[, all_var_names] <- imputed
  tibble::as_tibble(new_data)
}

#' @export
#' @importFrom generics tidy
tidy.step_fast_imputation <- function(x, ...) {
  if (x$trained) {
    tidy(x$patterns)
  } else {
    tibble::tibble(
      terms = recipes::sel2char(x$terms),
      id    = x$id
    )
  }
}
