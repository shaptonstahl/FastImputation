#' @name FI_true
#' @docType data
#' @title Imputation "True" Data
#' @description Smaller simulated dataset drawn from the same distribution as FI_train and FI_test.
#'   This dataset is entirely the same as FI_test except FI_test has 5\% of its values missing.
#'   Used to evaluate the quality of the values imputed in FI_test.
#' @source All columns start as multivariate normal draws.  Columns 2, 5, and 6 are transformed.
#'   Column 9 is the result of three multivariate normal columns being interpreted as one-hot encoding
#'   of a three-valued categorical variable.
#' @author Stephen R. Haptonstahl \email{srh@haptonstahl.org}
#' @format A data frame with 9 variables and 250 observations. \describe{
#'   \item{\code{user_id_1}}{Sequential user ids}
#'   \item{\code{bounded_below_2}}{Multivariate normal, transformed using \code{exp(x)}}
#'   \item{\code{unbounded_3}}{Multivariate normal}
#'   \item{\code{unbounded_4}}{Multivariate normal}
#'   \item{\code{bounded_above_5}}{Multivariate normal, transformed using \code{-exp(x)}}
#'   \item{\code{bounded_above_and_below_6}}{Multivariate normal, transformed using \code{pnorm(x)}}
#'   \item{\code{unbounded_7}}{Multivariate normal}
#'   \item{\code{unbounded_8}}{Multivariate normal}
#'   \item{\code{categorical_9}}{"A" if the first of three multivariate normal draws is greatest; "B" if the
#'   second is greatest; "C" if the third is greatest}
#'   }
#' @usage data(FI_true)
#' @keywords datasets
NA
