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
