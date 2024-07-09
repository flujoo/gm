test_that("note metricalization works", {
  music <- Music() + Line("C4", "whole", bar = 2, offset = 2.5) + Meter(3, 4)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, meters)

  out <- metricalize_note(notes, meters)
  row.names(out) <- NULL

  expected <- data_frame(
    line = rep(1L, 3),
    i = rep(1L, 3),
    j = rep(NA_integer_, 3),
    pitch = rep("C4", 3),
    midi = rep(60L, 3),
    duration = rep(NA_character_, 3),
    length = c(0.5, 3, 0.5),
    grace = c(FALSE, FALSE, FALSE),
    start_bar = c(2L, 3L, 4L),
    start_offset = c(2.5, 0, 0),
    end_bar = c(2L, 3L, 4L),
    end_offset = c(3, 3, 0.5)
  )

  expect_identical(unclass(out), unclass(expected))
})
