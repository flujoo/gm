test_that("inferring divisions works", {
  music <-
    Music() +
    Line(durations = c("q/3", "h/5", "q/7")) +
    Meter(1, 8) +
    Meter(3, 16, bar = 3)

  notes <- music[["notes"]]
  meters <- music[["meters"]]
  lines <- music[["lines"]]

  notes[["measure_rest"]] <- c(FALSE, FALSE, TRUE)
  out <- infer_divisions(lines, notes, meters)
  expected <- 60
  expect_identical(out, expected)

  notes[["measure_rest"]] <- c(FALSE, FALSE, FALSE)
  out <- infer_divisions(lines, notes, meters)
  expected <- 420
  expect_identical(out, expected)
})
