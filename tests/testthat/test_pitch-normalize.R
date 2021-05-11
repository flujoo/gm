library(gm)


test_that("logical", {
  out <- normalize_pitches(c(NA, NA))
  expected <- list(PitchRest(), PitchRest())
  expect_identical(out, expected)
})


test_that("character", {
  out <- normalize_pitches(c("c3", "90", NA))
  expected <- list(Pitch("C", 0L, 3L), PitchValue(90), PitchRest())
  expect_identical(out, expected)
})


test_that("list", {
  pitches <- list(
    NULL, logical(), character(),
    NA_character_, NA_real_,
    90, 90L, "c3",
    c(80, "e3")
  )

  out <- normalize_pitches(pitches)

  expected <- list(
    PitchRest(), PitchRest(), PitchRest(),
    PitchRest(), PitchRest(),
    PitchValue(90), PitchValue(90), Pitch("C", 0, 3),
    list(PitchValue(80), Pitch("E", 0, 3))
  )

  expect_identical(out, expected)
})
