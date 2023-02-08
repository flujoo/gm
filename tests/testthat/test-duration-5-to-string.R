test_that("tied complex tuplets", {
  duration <- Duration("w./3 - h/4*(8./q)/5")
  long_string <- "whole./3 - half/4*(eighth./quarter)/5"
  short_string <- "w./3-h/4*(8./q)/5"

  expect_equal(to_string(duration), long_string)
  expect_equal(to_string(duration, TRUE), short_string)
})
