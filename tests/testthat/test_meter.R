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


test_that("Music + Meter", {
  m1 <- Meter(1, 4, actual_number = 3)
  m2 <- Meter(3, 8, bar = 11)
  m <- Music() + m1 + m2
  out <- m$global

  expected <- tibble::tibble(
    object = list(m1, m2),
    notation = c("1/4 (3/4)", "3/8"),
    value = c(3, 1.5),
    bar = c(1L, 11L),
    offset = c(NA_real_, NA_real_),
    line = c(NA_integer_, NA_integer_),
    scope = c(NA_character_, NA_character_)
  )

  expect_identical(out, expected)
})
