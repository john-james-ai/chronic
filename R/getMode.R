#==============================================================================#
#                                  getMode                                     #
#==============================================================================#
#' getMode
#'
#' \code{getMode(v)} returns the mode for a vector v
#'
#' This function takes as its parameters, a vector of numeric values and
#' computes the mode, and returns it to the calling environment.
#'
#' @param v - vector of numbers
#' @return mode - Numeric, the most frequently occurring value
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
getMode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}
