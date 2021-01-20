library(mr)


test_that("check tie", {
  e <- expect_error(Line(list(90), list(1), c(2, 2)))
  expect_equal(class(e)[1], "position_beyond_line_length")
  expect_match(e$message, "(2, 2)")

  e <- expect_error(Line(list(NA), list(1), c(1, 2)))
  expect_equal(class(e)[1], "rest_at_position")
  expect_match(e$message, "(1, 2)")

  e <- expect_error(Line(list(90:91), list(1), c(1, 10)))
  expect_equal(class(e)[1], "position_beyond_chord_length")
  expect_match(e$message, "(1, 10)")

  e <- expect_error(Line(list(90), list(1), 1))
  expect_equal(class(e)[1], "next_position_beyond_line_length")
  expect_match(e$message, "1")

  e <- expect_error(Line(list(90, NA), list(1, 1), 1))
  expect_equal(class(e)[1], "rest_at_next_position")
  expect_match(e$message, "1")

  e <- expect_error(Line(list(89:90, 91), list(1, 1), 1))
  expect_equal(class(e)[1], "no_equivalent_pitch")
  expect_match(e$message, "1")

  e <- expect_error(
    Line(list(c(90, 90), 90), list(1, 1), list(c(1, 1), c(1, 2)))
  )
  expect_equal(class(e)[1], "no_equivalent_pitch_complicated")
  expect_match(e$message, "(1, 2)")
})


test_that("normalize tie", {
  ps <- list(
    c(90, 91, 92, 93), c(91, 92),
    "C4", "C4",
    NA,
    c(90, 90, 91), 90
  )

  ds <- rep(list(1), length(ps))
  tie <- list(1, c(3, 1), 6)
  line <- Line(ps, ds, tie)

  out <- line$tie$positions
  expected <- list(c(1, 2), c(1, 3), 3, c(6, 1))
  expect_equal(out, expected)
})
