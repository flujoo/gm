test_that("convert complex tuplets to string", {
  duration <- Duration("h / 4 *(8./q) / 5")

  out <- to_string(duration)
  expected <- "half/4*(eighth./quarter)/5"
  expect_equal(out, expected)

  out <- to_string(duration, TRUE)
  expected <- "h/4*(8./q)/5"
  expect_equal(out, expected)
})
