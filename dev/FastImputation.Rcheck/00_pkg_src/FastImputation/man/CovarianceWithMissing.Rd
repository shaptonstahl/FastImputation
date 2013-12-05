\name{CovarianceWithMissing}
\alias{CovarianceWithMissing}
\title{Estimate covariance when data is missing}
\usage{
  CovarianceWithMissing(x)
}
\arguments{
  \item{x}{matrix or data.frame, data with each row an
  observation and each column a variable.}
}
\value{
  matrix, unbiased estimate of the covariance.
}
\description{
  Ignoring missing values can lead to biased estimates of
  the covariance. Lounici (2012) gives an unbiases
  estimator when the data has missing values.
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  High-dimensional covariance matrix estimation with
  missing observations. Karim Lounici. 2012.
}

