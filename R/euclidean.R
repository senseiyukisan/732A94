#' @title Euclidean
#' @param a An Integer.
#' @param b A Integer
#' @description Implementation of the substraction based euclidean algorithm to find the Greatest Common Divisior (GCD) of two Integer numbers.
#' @return The GCD of input params.
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' @export
euclidean <- function(a, b) {
  if (!a == round(a))
    stop("a should be integer")
  if (!b == round(b))
    stop("b should be integer")
  
  a = abs(a)
  b = abs(b)
  
  while (a != b) {
    if (a > b) {
      a = a - b
    }
    else {
      b = b - a
    }
  }
  return(a)
}
