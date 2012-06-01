\name{LimitToSet}
\alias{LimitToSet}
\title{Coerce numeric values into a given set.}
\usage{
  LimitToSet(x, set)
}
\arguments{
  \item{x}{A vector, matrix, array, or dataframe with value
  to be coerced into a range or set.}

  \item{set}{A list of values that x will be forced to take
  on.}
}
\value{
  An object of the same class as \code{x} with values
  replaced as needed to satisfy the constraints.
}
\description{
  Given some values \code{x} and a set of values
  \code{set}, each value in \code{x} is changed to the
  value in \code{set} that is closest.
}
\examples{
x <- runif(100, min=0, max=10)
y <- LimitToSet(x, set=c(1:10))
plot(x, y)
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}

