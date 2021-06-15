library(gm)


test_that("initialize and recycle `durations`", {
  out <- Line(90:92)
  expected <- Line(90:92, 1)
  expect_identical(out, expected)
})


test_that("initialize and recycle `pitches`", {
  out <- Line(durations = 1:3)
  expected <- Line(rep(NA, 3), 1:3)
  expect_identical(out, expected)
})


test_that("`$pitches` and `$durations` in Line", {
  l <- Line(list(90, NA, c("C4", 64)), list(1, "q", tuplet("w")))

  expected_pitches <- tibble::tribble(
    ~i, ~j, ~pitch, ~notation, ~value,
    1L, 1L, 90L, NA_character_, 90L,
    2L, 1L, NULL, NA_character_, NA_integer_,
    3L, 1L, Pitch("C", 0, 4), "C4", 60L,
    3L, 2L, 64L, NA_character_, 64L
  )

  expected_durations <- tibble::tribble(
    ~i, ~duration, ~notation, ~value,
    1L, to_Duration(1), "quarter", 1,
    2L, to_Duration("q"), "quarter", 1,
    3L, to_Duration(4), "whole", 4
  )

  expect_identical(l$pitches, expected_pitches)
  expect_identical(l$durations, expected_durations)
})
