\name{TrainFastImputation}
\alias{TrainFastImputation}
\title{Learn from the training data so that later you can fill in missing data}
\usage{
  TrainFastImputation(x, constraints = list())
}
\arguments{
  \item{x}{Dataframe containing training data. Can have
  incomplete rows.}

  \item{constraints}{A list of constraints.  See the
  examples below for formatting details.}
}
\value{
  An object of class 'FastImputationPatterns' that contains
  information needed later to impute on a single row.
}
\description{
  Like Amelia, FastImputation assumes that the columns of
  the data are multivariate normal or can be transformed
  into approximately multivariate normal.
}
\examples{
data(FItrain)   # provides FItrain dataset
patterns <- TrainFastImputation(FItrain)

patterns.with.constraints <- TrainFastImputation(
  FItrain,
  constraints=list(list(1, list(set=0:1)),
                   list(2, list(lower=0)),
                   list(3, list(lower=0)),
                   list(4, list(lower=0)),
                   list(5, list(lower=0)),
                   list(6, list(lower=0, upper=1)),
                   list(7, list(lower=0)),
                   list(8, list(lower=0))))
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  \url{http://gking.harvard.edu/amelia/}
}
\seealso{
  \code{\link{FastImputation}}
}

