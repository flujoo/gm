library(gm)


test_that("logical", {
  out <- normalize_pitches(c(NA, NA))
  expected <- list(NULL, NULL)
  expect_identical(out, expected)
})


test_that("character", {
  out <- normalize_pitches(c("c3", "90", NA))
  expected <- list(Pitch("C", 0L, 3L), 90L, NULL)
  expect_identical(out, expected)
})


test_that("normalize pitches", {
  out <- list(
    NULL, logical(), character(), double(), integer(),
    NA, NA_character_, NA_real_, NA_integer_,
    90, "C4", "e3", "90",
    c("90", "C4", "e3")
  ) %>% normalize_pitches()

  expected <- list(
    NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,
    90L, Pitch("C", 0, 4), Pitch("E", 0, 3), 90L,
    list(90L, Pitch("C", 0, 4), Pitch("E", 0, 3))
  )

  expect_identical(out, expected)
})


test_that("conversion", {
  ps <- list(NA, "C4", 90) %>% normalize_pitches()

  # signify
  out <- signify(ps)
  expected <- c(NA, "C4", NA)
  expect_identical(out, expected)

  # quantify
  out <- quantify(ps)
  expected <- c(NA, 60L, 90L)
  expect_identical(out, expected)
})
