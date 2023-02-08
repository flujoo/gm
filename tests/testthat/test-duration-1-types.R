test_that("values of duration types are powers of 2", {
  out <- duration_types$value
  expected <- 2^(6 - 1:14)
  expect_equal(out, expected)
})
