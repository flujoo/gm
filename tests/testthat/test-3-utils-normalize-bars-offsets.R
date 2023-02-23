test_that("round up offsets in Music", {
  music <-
    Music() +
    Meter(3, 4, 4) + Meter(4, 4, 5) + Meter(3, 4, 1, 2) +
    Line(90, bar = 3, offset = 10) +
    Clef("g", bar = 4, offset = 3) +
    Tempo(90, bar = 10, offset = 1)

  music <- normalize_bars_offsets(music)
  lines <- music$lines
  clefs <- music$clefs
  tempos <- music$tempos

  out <- lines$bar
  expected <- 6L
  expect_identical(out, expected)

  out <- lines$offset
  expected <- 1
  expect_identical(out, expected)

  out <- clefs$bar
  expected <- 5L
  expect_identical(out, expected)

  out <- clefs$offset
  expected <- 0
  expect_identical(out, expected)

  out <- tempos$bar
  expected <- 10L
  expect_identical(out, expected)

  out <- tempos$offset
  expected <- 1
  expect_identical(out, expected)
})
