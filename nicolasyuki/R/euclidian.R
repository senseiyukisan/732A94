#' @param a An Integer.
#' @param b A Integer
#' @description Implementation of the substraction based euclidean algorithm to find the Greatest Common Divisior (GCD) of two Integer numbers.
#' @return The GCD of input params.
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean <- function(a, b) {
  if (!a == round(a))
    stop("a should be integer")
  if (!b == round(b))
    stop("b should be integer")
  
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
euclidean(123612, 13892347912)
euclidean(100, 1000)
