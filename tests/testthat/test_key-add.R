library(gm)


test_that("Music + Key", {
  m <-
    Music() +
    Line(90) +
    Key(1) +
    Key(2) +
    Key(3, bar = 2) +
    Key(4, to = 1) +
    Key(5, to = 1) +
    Meter(4, 4) +
    Key(6, to = 1, bar = 2) +
    Key(7, to = 1, scope = "staff") +
    Key(0, to = 1, scope = "staff") +
    Key(-1, to = 1, scope = "staff", bar = 2)

  out <- m$keys$value
  expected <- c(2L, 3L, 5L, 6L, 0L, -1L)
  expect_identical(out, expected)
})
