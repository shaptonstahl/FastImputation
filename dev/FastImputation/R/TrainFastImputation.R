#' Learn from the training data so that later you can fill in missing data
#'
#' Like Amelia, FastImputation assumes that the columns of the data are
#' multivariate normal or can be transformed into approximately
#' multivariate normal.
#' 
#' @param x Dataframe containing training data. Can have incomplete rows.
#' @param constraints A list of constraints.  See the examples below for formatting details.
#' @return An object of class 'FastImputationPatterns' that contains
#'   information needed later to impute on a single row.
#' @export
#' @seealso \code{\link{FastImputation}}
#' @references
#' \url{http://gking.harvard.edu/amelia/}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#'
#' data(FI_train)   # provides FI_train dataset
#'
#' patterns_with_constraints <- TrainFastImputation(
#'   FI_train,
#'   constraints=list(list(1, list(ignore=TRUE)),       # use for ids, other vars not to impute
#'                    list(2, list(lower=0)),           # continuous var with lower bound
#'                    list(5, list(upper=0)),           # continuous var with only upper bound
#'                    list(6, list(lower=0, upper=1)),  # bounded to a finite interval
#'                    list(9, list(categorical=TRUE))   # finite number of values
#'                    ))                                # vars 3, 4, 7, 8 continuous with no bounds
TrainFastImputation <-
function(
  x,
  constraints=list()
) {
  # TODO:
  # - add idvars parameter such that: a vector of column numbers or 
  #   column names that indicates identification variables.  These 
  #   will be dropped from the analysis but copied into the imputed 
  #   datasets.

  if( "data.frame" != class(x) ) stop("Training data must be in a data.frame")
  
  x <- UnfactorColumns(x)  # unfactor the columns
  
  # Fill the constraints so there is a constraint entry for each column
  if( 0==length(constraints) ) {
    filled_constraints <- replicate(ncol(x), list())
  } else {
    filled_constraints <- sapply(1:ncol(x), function(i.col) {
      is_each_constraint_for_this_col <- sapply(constraints, function(this_cons) {
        return( this_cons[[1]] == i.col )
      })

      if( 0 == sum(is_each_constraint_for_this_col) ) {
        return( list() )
      } else if( 1 == sum(is_each_constraint_for_this_col) ) {
        return( constraints[[which(is_each_constraint_for_this_col)]][[2]] )
      } else {
        return( constraints[[max(which(is_each_constraint_for_this_col))]][[2]] )
      }
    })
  }
  
  # tally and remove ignored columns
  cols_to_ignore <- which(sapply(filled_constraints, function(this_cons) 
    !is.null(this_cons$ignore) ))
  y <- x[,-cols_to_ignore]
  y_constraints <- filled_constraints[-cols_to_ignore]
  
  # Tally the columns with each type of constraint
  cols_bound_to_intervals <- which(sapply(y_constraints, function(this_cons) 
    !(is.null(this_cons$upper) && is.null(this_cons$lower))))
  cols_categorical <- which(sapply(y_constraints, function(this_cons) 
    !is.null(this_cons$categorical) ))

  # Normalize variables bounded to an interval
  for(this_col in cols_bound_to_intervals) {
    y[,this_col] <- NormalizeBoundedVariable(y[,this_col], constraints=y_constraints[[this_col]])
  }
  
  if(length(cols_categorical) > 0) {
    # count dummies for one-hot encoding
    categories <- lapply(cols_categorical, function(i) {
        u <- unique(y[,i])
        return( u[!is.na(u)] )
      })
    total_one_hot_dummies <- sum(sapply(categories, length))
    total_not_categorical <- ncol(y) - length(cols_categorical)
    if(total_not_categorical < 2)
      stop("Too few continuous variables.")
    if(total_one_hot_dummies > total_not_categorical)
      warning("More categories in categorical variables than continuous variables. Performance will suffer.")
    
    y_categorical <- y[,cols_categorical, drop=FALSE]
    
    z <- y[,-cols_categorical]
    z <- data.frame(z, matrix(NA_real_, nrow=nrow(y), ncol=total_one_hot_dummies))
    
    current_col_to_fill <- 1
    while(current_col_to_fill < total_one_hot_dummies) {
      for(i in 1:length(categories)) {
        for(j in 1:length(categories[[i]])) {
          z[,total_not_categorical+current_col_to_fill] <- ifelse(y_categorical[,i]==categories[[i]][j], 1, -1)
          current_col_to_fill <- current_col_to_fill + 1
        }
      }
    }
  } else {
    z <- x
  }
  
  FastImputationMeans <- colMeans(z, na.rm=TRUE)
  FastImputationCovariance <- CovarianceWithMissing(z)
  
  patterns <- list(
    FI_var_names=names(x),
    FI_means=FastImputationMeans, 
    FI_covariance=FastImputationCovariance, 
    FI_constraints=filled_constraints,
    FI_cols_to_ignore=cols_to_ignore,
    FI_cols_bound_to_intervals=cols_bound_to_intervals,
    FI_cols_categorical=cols_categorical,
    FI_categories=categories)
  class(patterns) <- "FastImputationPatterns"
  return( patterns )
}
