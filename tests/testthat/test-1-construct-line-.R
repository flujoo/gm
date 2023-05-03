test_that("rests can be represented by `NULL`, `NA`, and empty vectors", {
  pitches <- list(
    NULL,
    NA, NA_real_, NA_integer_, NA_character_,
    logical(), double(), integer(), character()
  )

  out <- Line(pitches)$notes$pitch
  expected <- rep(NA_character_, length(pitches))
  expect_identical(out, expected)
})
