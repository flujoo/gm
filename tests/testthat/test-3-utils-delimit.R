test_that("grace notes and chords work", {
  music <-
    Music() +
    Meter(3, 4) +
    Line(rep("C4", 3), rep(4, 3), bar = 2, offset = 0.5) + Grace(2) +
    Line(list(rep("C4", 3)), 4)

  notes <- indicate_grace(music[["notes"]], music[["graces"]])
  notes <- delimit_notes(notes, music[["lines"]], music[["meters"]])

  out <- notes[, c("start_bar", "start_offset", "end_bar", "end_offset")]
  out <- as.data.frame(out)

  expected <- data.frame(
    start_bar = c(2L, NA, 3L, 1L, 1L, 1L),
    start_offset = c(0.5, NA, 1.5, 0, 0, 0),
    end_bar = c(3L, NA, 4L, 2L, 2L, 2L),
    end_offset = c(1.5, NA, 2.5, 1, 1, 1)
  )

  expect_identical(out, expected)
})
