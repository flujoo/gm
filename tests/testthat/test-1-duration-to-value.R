test_that("convert complex tuplets to value", {
  duration <- to_Duration("h / 4 *(8./q) /5")

  out <- to_value(duration)
  expected <- 2/4*(0.5*1.5/1)/5
  expect_equal(out, expected)
})
