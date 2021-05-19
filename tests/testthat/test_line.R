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


test_that("notes", {
  out <- Line(list(90, NA, c("C4", 64)), list(1, "q", tuplet("w")))$notes

  expected <- tibble::tribble(
    ~i, ~j, ~pitch, ~pn, ~pv, ~duration, ~dn, ~dv,

    1L, NA_integer_,
    90L, NA_character_, 90L,
    to_Duration(1), "quarter", 1,

    2L, NA_integer_,
    NA, NA_character_, NA_integer_,
    to_Duration("q"), "quarter", 1,

    3L, 1L,
    Pitch("C", 0, 4), "C4", 60L,
    to_Duration(4), "whole", 4,

    3L, 2L,
    64L, NA_character_, 64L,
    to_Duration(4), "whole", 4
  )

  expect_identical(out, expected)
})
