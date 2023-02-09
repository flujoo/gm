test_that("convert tied complex tuplets to string", {
  duration <- Duration("w./3 - h/4*(8./q)/5")

  out <- to_string(duration)
  expected <- "whole./3 - half/4*(eighth./quarter)/5"
  expect_equal(out, expected)

  # short string
  out <- to_string(duration, TRUE)
  expected <- "w./3-h/4*(8./q)/5"
  expect_equal(out, expected)
})
