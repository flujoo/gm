test_that("key preparation works", {
  music <-
    Music() +
    Meter(4, 4) +
    Line(90) +
    Line(91, as = "staff") +
    Line(92) +
    Key(1, bar = 3) +
    Key(2, to = 2, scope = "part") +
    Key(3, bar = 4, to = 2, scope = "staff") +
    Key(4, to = 3)

  out <- as.data.frame(prepare_keys(music))
  row.names(out) <- NULL

  expected <- data.frame(
    part = c(1L, 1L, 1L, 2L),
    staff = c(1L, 2L, 2L, 1L),
    bar = c(1L, 4L, 1L, 1L),
    key = c(2L, 3L, 2L, 4L)
  )

  expect_identical(out, expected)
})
