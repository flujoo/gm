library(gm)


test_that("logical", {
  out <- normalize_pitches(c(NA, NA))
  expected <- list(PitchRest(), PitchRest())
  expect_identical(out, expected)
})


test_that("character", {
  out <- normalize_pitches(c("c3", "90", NA))
  expected <- list(Pitch("C", 0L, 3L), 90L, PitchRest())
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
    90L, 90L, Pitch("C", 0, 3),
    list(80L, Pitch("E", 0, 3))
  )

  expect_identical(out, expected)
})
