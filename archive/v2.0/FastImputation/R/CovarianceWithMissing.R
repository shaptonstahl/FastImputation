#' Estimate covariance when data is missing
#'
#' Ignoring missing values can lead to biased estimates of the covariance.
#' Lounici (2012) gives an unbiased estimator when the data has missing values.
#' 
#' @param x matrix or data.frame, data with each row an observation and each column a variable.
#' @return matrix, unbiased estimate of the covariance.
#' @export
#' @references
#' High-dimensional covariance matrix estimation with missing observations.
#' Karim Lounici. 2012.
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
CovarianceWithMissing <- function(x) {
  stopifnot(methods::is(x, "matrix") | (methods::is(x, "data.frame") && is.numeric(as.matrix(x))))
  
  delta <- mean(!is.na(x))  # probabiliity a cell is observed, proportion of observed entries
  if( 1 == delta ) {        # no missing data
    out <- stats::cov(x)
  } else {
    # set up for notation of Louncini (2012)
    x <- as.matrix(x)
    x <- t(x)  # puts observations in columns, variables in rows
    n <- ncol(x)  # number of observations
    p <- nrow(x)  # number of variables
    
    # per page 1 of the article
    y <- sweep(x, 1, rowMeans(x, na.rm=TRUE))
    y[is.na(y)] <- 0
    
    # Equation at bottom of page 3
    SigmaDeltaN <- matrix(0, nrow=p, ncol=p)
    for(i in 1:n) SigmaDeltaN <- SigmaDeltaN + y[,i] %o% y[,i]
    SigmaDeltaN <- SigmaDeltaN / n
    
    # Equation 1.4
    out <- ((delta - 1) * diag(diag(SigmaDeltaN)) + SigmaDeltaN) / (delta^2)
  }
  # prepare and return the output
  return(out)
}