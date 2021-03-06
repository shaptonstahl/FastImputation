\name{BoundNormalizedVariable}
\alias{BoundNormalizedVariable}
\title{Take a normalized variable and transform it back to a bounded variable.}
\usage{
  BoundNormalizedVariable(x, constraints)
}
\arguments{
  \item{x}{A vector, matrix, array, or dataframe with value
  to be coerced into a range or set.}

  \item{constraints}{A list of constraints.  See the
  examples below for formatting details.}
}
\value{
  An object of the same class as x with the values
  transformed into the desired half-line or segment.
}
\description{
  This takes variables on the real line and constrains them
  to be on a half-line (constrained above or below) or a
  segment (constrained both above and below). This is
  approximately the inverse of
  \code{NormalizeBoundedVariable}; this does not completely
  reverse the effect of \code{NormalizeBoundedVariable}
  because \code{NormalizeBoundedVariable} first forces
  values away from the bounds, and this information is
  lost.
}
\examples{
constraints=list(lower=5)           # lower bound when constrining to an interval
  constraints=list(upper=10)          # upper bound when constraining to an interval
  constraints=list(lower=5, upper=10) # both lower and upper bounds
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}

