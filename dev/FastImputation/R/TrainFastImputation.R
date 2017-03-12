#' Learn from the training data so that later you can fill in missing data
#'
#' Like Amelia, FastImputation assumes that the columns of the data are
#' multivariate normal or can be transformed into approximately
#' multivariate normal.
#' 
#' @param x Dataframe containing training data. Can have incomplete rows.
#' @param constraints A list of constraints.  See the examples below for formatting details.
#' @param idvars A vector of column numbers or column names to be ignored in the imputation process.
#' @param categorical A vector of column numbers or column names of varaibles with a (small) set of possible values.
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
#'   constraints=list(list("bounded_below_2", list(lower=0)),
#'                    list("bounded_above_5", list(upper=0)),
#'                    list("bounded_above_and_below_6", list(lower=0, upper=1))
#'                    ),
#'   idvars="user_id_1",
#'   categorical="categorical_9")
#'   
TrainFastImputation <-
function(
  x,
  constraints=list(),
  idvars,
  categorical
) {
  # TODO:
  # - add transformations of the data other than for boundaries (?)

  if( "data.frame" != class(x) ) stop("Training data must be in a data.frame")
  
  x <- UnfactorColumns(x)  # unfactor the columns
  
  if(missing(categorical)) {
    cols_categorical <- numeric(0)
  } else {
    # tally categorical columns
    if(is.numeric(categorical)) {
      cols_categorical <- sort(categorical)
      if(any(cols_categorical > ncol(x))) stop("Categorical column specified by index that doesn't exist.")
    } else {
      cols_categorical <- as.vector(sort(sapply(categorical, function(col_name) {
        col_i <- which(names(x) == col_name)
        if(0 == length(col_i)) stop("Categorical column specified by name that doesn't exist.")
        return(col_i)
      })))
    }
  }
  
  # Fill the constraints so there is a constraint entry for each column
  if( 0==length(constraints) ) {
    filled_constraints_in_x <- replicate(ncol(x), list())
  } else {
    filled_constraints_in_x <- sapply(1:ncol(x), function(i_col) {
      is_each_constraint_for_this_col <- sapply(constraints, function(this_cons) {
        return( this_cons[[1]] == i_col | any((names(x) == this_cons[[1]]) == i_col) )
      })
      
      if( 0 == sum(is_each_constraint_for_this_col) ) {
        return( list() )
      } else if( 1 == sum(is_each_constraint_for_this_col) ) {
        return( constraints[[which(is_each_constraint_for_this_col)]][[2]] )
      } else {
        warning("More than one constraint specified for variable ", i_col)
        return( constraints[[max(which(is_each_constraint_for_this_col))]][[2]] )
      }
    })
  }
  
  # tally and remove ignored columns
  if(missing(idvars)) {
    cols_to_ignore <- numeric(0)
    y <- x
    filled_constraints_in_y <- filled_constraints_in_x
  } else {
    if(is.numeric(idvars)) {
      cols_to_ignore <- idvars
    } else {
      cols_to_ignore <- as.vector(sort(sapply(idvars, function(col_name) which(names(x) == col_name))))
    }
    y <- x[,-cols_to_ignore]
    filled_constraints_in_y <- filled_constraints_in_x[-cols_to_ignore]
    # shift the indices of categorical variables down as needed
    if(length(cols_categorical) > 0) {
      for(cti in rev(sort(cols_to_ignore))) {
        cols_categorical[cols_categorical > cti] <- cols_categorical[cols_categorical > cti] - 1
      }
    }
  }
  
  # tally the columns with each type of constraint
  cols_in_y_bound_to_intervals <- which(sapply(filled_constraints_in_y, function(this_cons) 
    !(is.null(this_cons$upper) && is.null(this_cons$lower))))
  
  # Check that constraints are respected in the training data
  for(i in cols_in_y_bound_to_intervals) {
    if(!is.null(filled_constraints_in_y[[i]]$lower)) {
      if(any(y[,i] < filled_constraints_in_y[[i]]$lower, na.rm=TRUE)) {
        stop("Column ", i, " does not respect the lower bound specified.")
      }
    }
    if(!is.null(filled_constraints_in_y[[i]]$upper)) {
      if(any(y[,i] > filled_constraints_in_y[[i]]$upper, na.rm=TRUE)) {
        stop("Column ", i, " does not respect the upper bound specified.")
      }
    }
  }
  
  # Normalize variables bounded to an interval
  for(this_col in cols_in_y_bound_to_intervals) {
    y[,this_col] <- NormalizeBoundedVariable(y[,this_col], constraints=filled_constraints_in_y[[this_col]])
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
    z <- data.frame(z, matrix(NA_real_, nrow=nrow(z), ncol=total_one_hot_dummies))
    
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
    categories <- list()
    z <- y
  }
  
  FastImputationMeans <- colMeans(z, na.rm=TRUE)
  FastImputationCovariance <- CovarianceWithMissing(z)
  
  # Ensure covariance matrix is well-conditioned
  # One-hot encoding with dummies for all category values makes the covariance matrix
  # singular or nearly so.  This forces the covariance matrix to the "closest" one
  # with only eigenvalues < min_size_eigenvalue being affected.
  min_size_eigenvalue <- .01
  eg <- eigen(FastImputationCovariance)$values
  if(min(eg) < min_size_eigenvalue) {
    eps_ev <- min_size_eigenvalue / abs(eg[1])
    FastImputationCovariance <- Matrix::nearPD(FastImputationCovariance, do2eigen=TRUE, posd.tol=eps_ev)$mat
  }
  
  patterns <- list(
    FI_var_names=names(x),  # match against cols of data to impute to help ensure that it's in the same format
    FI_means=FastImputationMeans,           # used for imputation
    FI_covariance=FastImputationCovariance, # used for imputation
    FI_constraints=filled_constraints_in_y, # one for each variable in input training data y, with empty constraints
    FI_cols_to_ignore=cols_to_ignore,       # indices in x of columns to ignore
    FI_cols_bound_to_intervals=cols_in_y_bound_to_intervals,  # indices in y of columns of bounded intervals
    FI_cols_categorical=cols_categorical,   # indices in y of columns with categorical data (order here used)
    FI_categories=categories)  # for each index with categorical data, list the values observed (order here used)
  class(patterns) <- "FastImputationPatterns"
  return( patterns )
}
