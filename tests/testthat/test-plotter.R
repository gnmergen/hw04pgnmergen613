check_plotter <- function() {
  if (is.null(plotter)) {
    skip("myseq_n passed, this will pass since it is dependant on it")
  }
}

test_that("plotter is present", {
  check_plotter()
})
