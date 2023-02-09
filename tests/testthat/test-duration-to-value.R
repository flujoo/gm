test_that("convert tied complex tuplets to value", {
  duration <- Duration("w./3 - h/4*(8./q)/5")

  out <- to_value(duration)
  expected <- 4*1.5/3 + 2/4*(0.5*1.5/1)/5
  expect_equal(out, expected)
})
