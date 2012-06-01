\name{NormalizeBoundedVariable}
\alias{NormalizeBoundedVariable}
\title{Take a variable bounded above/below/both and return an unbounded (normalized) variable.}
\usage{
  NormalizeBoundedVariable(x, constraints, tol = pnorm(-5))
}
\arguments{
  \item{x}{A vector, matrix, array, or dataframe with value
  to be coerced into a range or set.}

  \item{constraints}{A list of constraints.  See the
  examples below for formatting details.}

  \item{tol}{Variables will be forced to be at least this
  far away from the boundaries.}
}
\value{
  stuff
}
\description{
  This transforms bounded variables so that they are not
  bounded. First variables are coerced away from the
  boundaries. by a distance of \code{tol}. The natural log
  is used for variables bounded either above or below but
  not both. The inverse of the standard normal cumulative
  distribution function (the quantile function) is used for
  variables bounded above and below.
}
\examples{
constraints=list(lower=5)           # lower bound when constrining to an interval
  constraints=list(upper=10)          # upper bound when constraining to an interval
  constraints=list(lower=5, upper=10) # both lower and upper bounds
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}

