test_that("myseq_n provides", {
  expect_equal(myseq_n(x = c(2,4,3), n = 3), 3)
  expect_equal(myseq_n(x = c(2,4,3), n = 4), 2.5)
  expect_equal(myseq_n(x = c(2,4,3), n = 5), 2.7)
})
