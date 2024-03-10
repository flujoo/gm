test_that("note metricalization works", {
  music <- Music() + Line("C4", "whole", bar = 2, offset = 2.5) + Meter(3, 4)
  music[["notes"]] <- indicate_grace(music[["notes"]], music[["graces"]])
  music <- delimit_notes(music)
  note <- music$notes
  meters <- music$meters
  out <- metricalize_note(note, meters)

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

  expect_identical(out, expected)
})
