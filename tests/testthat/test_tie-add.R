library(gm)


test_that("start_position_beyond_line_length", {
  e <- expect_error(Music() + Line(90) + Tie(1, 10), "10")
  expect_s3_class(e, "start_position_beyond_line_length")
})


test_that("rest_at_start_position", {
  e <- expect_error(Music() + Line(c(90, NA)) + Tie(1, 2), "2")
  expect_s3_class(e, "rest_at_start_position")
})


test_that("start_position_beyond_chord_length", {
  e <- expect_error(Music() + Line(list(90:91)) + Tie(1, 1, 9), "9")
  expect_s3_class(e, "start_position_beyond_chord_length")
})


test_that("stop_position_beyond_line_length", {
  e <- expect_error(Music() + Line(90) + Tie(1, 1), "1")
  expect_s3_class(e, "stop_position_beyond_line_length")
})


test_that("rest_at_stop_position", {
  e <- expect_error(Music() + Line(c(90, NA)) + Tie(1, 1), "1")
  expect_s3_class(e, "rest_at_stop_position")
})


test_that("no_equivalent_pitch", {
  e <- expect_error(Music() + Line(c(90, 91)) + Tie(1, 1), "1")
  expect_s3_class(e, "no_equivalent_pitch")
})


test_that("stop_position_used", {
  e <- expect_error(
    Music() + Line(list(c(90, 90), 90)) + Tie(1, 1) + Tie(1, 1, 2))
  expect_s3_class(e, "stop_position_used")
})


m <- Music() + Line(list(c(90, 91), c(91, 90)))


test_that("tie a note in a chord", {
  t <- Tie(1, 1, 1)
  out <- (m + t)$ties

  t1 <- t
  t1$type <- "start"

  t2 <- t
  t2$type <- "stop"

  expected <- tibble::tibble(
    line = c(1L, 1L),
    i = c(1L, 2L),
    j = c(1L, 2L),
    type = c("start", "stop"),
    tie = list(t1, t2)
  )

  expect_identical(out, expected)

  out <- (m + t + t)$ties
  expect_identical(out, expected)
})


test_that("tie a chord", {
  t <- Tie(1, 1)
  out <- (m + t)$ties

  t1 <- t
  t1$type <- "start"

  t2 <- t
  t2$type <- "stop"

  expected <- tibble::tibble(
    line = rep(1L, 4),
    i = rep(c(1L, 2L), 2),
    j = c(1L, 2L, 2L, 1L),
    type = rep(c("start", "stop"), 2),
    tie = list(t1, t2, t1, t2)
  )

  expect_identical(out, expected)

  out <- (m + t + Tie(1, 1, 2))$ties
  expect_identical(out, expected)
})
