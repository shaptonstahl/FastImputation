#' Convert columns of a dataframe from factors to character or numeric.
#'
#' @param x A dataframe
#' @return A dataframe containing the same data but any \code{factor} columns have been replaced with numeric or character columns.
#' @export
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
UnfactorColumns <- function(x) {
  if (!is.data.frame(x)) stop("UnfactorColumns requires a data.frame for input")

  for (col_name in names(x)) {
    if (is.factor(x[[col_name]])) {
      # Convert factor to character
      x[[col_name]] <- as.character(x[[col_name]])
      # Attempt to convert to numeric if possible
      suppressWarnings(
        as_num <- as.numeric(x[[col_name]])
      )
      if (!any(is.na(as_num)) && length(as_num) == length(x[[col_name]])) {
        x[[col_name]] <- as_num
      }
    }
  }
  return(x)
}
