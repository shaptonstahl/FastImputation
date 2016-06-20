\name{FItrue}
\docType{data}
\alias{FItrue}

\title{Fraud "True" Data}

\description{Complete observations of Web financial transactions.  
Used to gauge the accuracy of imputation of FItest.}

\format{ A data frame with 10 variables and 10000 observations. \enumerate{
  \item \code{cust.id}: Internal customer identification number
  \item \code{order.id}: Unique identification number for this transaction (row)
  \item \code{is.fraud}: 1 if the transaction is fraudulent, 0 otherwise
  \item \code{customer.age.yrs}: Customer age in years; may be a decimal
  \item \code{spent.days.0to2}: Amount spent in dollars by customer between 0 and 2 days before the current transaction
  \item \code{spent.days.3to10}: Amount spent in dollars by customer between 3 and 10 days before the current transaction
  \item \code{spent.days.11to30}: Amount spent in dollars by customer between 11 and 30 days before the current transaction
  \item \code{geo.ip.fraud.rate}: Fraction between 0 and 1 of transactions from that geographic location (identified by IP address) that have been fraudulent
  \item \code{account.age.days}: Integer number of days the customer has had the account
  \item \code{days.to.first.purchase} Integer number of days between account creation and the first purchase by the customer
}}

\source{This is simulated data generated to be similar to real data.}

\author{Stephen R. Haptonstahl \email{srh@haptonstahl.org}}

\usage{FItrue}
\keyword{datasets}
