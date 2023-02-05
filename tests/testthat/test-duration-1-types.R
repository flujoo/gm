test_that("values of duration types are powers of 2", {
  expect_equal(duration_types$value, 2^(6 - 1:14))
})
