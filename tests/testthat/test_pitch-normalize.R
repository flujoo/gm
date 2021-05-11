library(gm)


test_that("logical", {
  out <- normalize_pitches(c(NA, NA))
  expected <- list(NA, NA)
  expect_identical(out, expected)
})


test_that("character", {
  out <- normalize_pitches(c("c3", "90", NA))
  expected <- list("C3", 90L, NA)
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
    NA, NA, NA,
    NA, NA,
    90L, 90L, "C3",
    list(80L, "E3")
  )

  expect_identical(out, expected)
})
