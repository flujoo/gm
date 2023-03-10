test_that("round up offsets in Music", {
  music <-
    Music() +
    Meter(3, 4, 4) + Meter(4, 4, 5) + Meter(3, 4, 1, 2) +
    Line(90, bar = 3, offset = 10.0111) +
    Clef("g", bar = 4, offset = 3) +
    Tempo(90, bar = 10, offset = 1.0111)

  music <- round_offsets(music)
  lines <- music$lines
  clefs <- music$clefs
  tempos <- music$tempos

  out <- lines$bar
  expected <- 6L
  expect_identical(out, expected)

  out <- lines$offset
  expected <- round_duration_value(1.0111, "round")
  expect_equal(out, expected)

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
  expected <- round_duration_value(1.0111, "floor")
  expect_equal(out, expected)
})
