test_that("tuplets with invalid takes or units are nevertheless valid", {
  notation <- "whole./3*(l/16..)/500"

  out <- is_duration_notation(notation)
  expect_true(out)
})
