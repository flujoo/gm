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


m1 <- Meter(1, 4, actual_number = 3)
m2 <- Meter(3, 8, bar = 11)
m <- Music() + m1 + m2

test_that("Music + Meter", {
  out <- m$meters

  expected <- tibble::tibble(
    bar = c(1L, 11L),
    meter = list(m1, m2),
    notation = c("1/4 (3/4)", "3/8"),
    value = c(3, 1.5),
  )

  expect_identical(out, expected)
})


m3 <- Meter(5, 16, bar = 11)
m4 <- Meter(5, 4)
m <- m + m3 + m4

test_that("replace Meter", {
  out <- m$meters

  expected <- tibble::tibble(
    bar = c(11L, 1L),
    meter = list(m3, m4),
    notation = c("5/16", "5/4"),
    value = c(1.25, 5)
  )

  expect_identical(out, expected)
})
