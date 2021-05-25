library(gm)


test_that("Meter -> value/string", {
  m <- Meter(c(1, 2, 3), 4, actual_number = 3, actual_unit = 8)

  expect_equal(quantify(m), 1.5)
  expect_equal(signify(m), "1+2+3/4 (3/8)")
})


test_that("`as_symbol`", {
  expect_s3_class(Meter(3, 4, as_symbol = FALSE), "Meter")
  expect_s3_class(Meter(4, 4, as_symbol = TRUE), "Meter")
  expect_s3_class(Meter(2, 2, as_symbol = TRUE), "Meter")
  expect_error(Meter(3, 4, as_symbol = TRUE))
})
