#' Recursive Sequence Element Find
#'
#' Takes a positive numeric vector of 3 and finds the nth element of the
#' sequence following the formula: x{n} = x{n-1} + (x{n-3}-x{n-2})/4.
#'
#' @param x vector consisting of three positive numbers.
#' @param n number for the nth element of the sequence.
#'
#' @return X value of the nth element using the sequence.
#' @export
#'
#' @examples myseq_n(x = c(2,4,3), n = 5)
myseq_n <- function(x, n) {
  vec <- x
  if (is.character(x)) stop("x must be a positive number and contain 3 elements")
  if (length(x) != 3) stop("x must have 3 elements")
  if (is.character(n)) stop("n must be a number")
  if (n < 1) stop("n must be greater than 0")
  #
  #
  else if (n >= 4) {
    for (n in 4:n)
      x[n] <- x[n-1] + (x[n-3]-x[n-2])/n
    return(x[n])
  }
  else if (n < 4) {return(x[n])}
}
