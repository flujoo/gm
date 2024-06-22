test_that("inferring divisions works", {
  music <-
    Music() +
    Line(durations = c("q/3", "h/5", "q/7")) +
    Meter(1, 8) +
    Meter(3, 16, bar = 3)

  notes <- music[["notes"]]
  meters <- music[["meters"]]
  lines <- music[["lines"]]
  tempos <- music[["tempos"]]
  clefs <- music[["clefs"]]

  notes[["measure_rest"]] <- c(FALSE, FALSE, TRUE)
  out <- infer_divisions(lines, notes, meters, tempos, clefs)
  expected <- 60
  expect_identical(out, expected)

  notes[["measure_rest"]] <- c(FALSE, FALSE, FALSE)
  out <- infer_divisions(lines, notes, meters, tempos, clefs)
  expected <- 420
  expect_identical(out, expected)
})


test_that("voice offsets work", {
  music <-
    Music() +
    Line(durations = "q") +
    Line(durations = "q", offset = 0.125, as = "voice") +
    Meter(4, 4)

  notes <- music[["notes"]]
  meters <- music[["meters"]]
  lines <- music[["lines"]]
  tempos <- music[["tempos"]]
  clefs <- music[["clefs"]]

  lines[["start_offset"]] <- lines[["offset"]]
  notes[["measure_rest"]] <- c(FALSE, FALSE)

  out <- infer_divisions(lines, notes, meters, tempos, clefs)
  expected <- 8
  expect_identical(out, expected)
})
