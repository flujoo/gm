test_that("tie indication works", {
  music <-
    Music() +
    Meter(2, 4) +
    Line(list(90:91, 90:91), 1.75) +
    Tie(1, 1)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, meters)
  lines <- delimit_lines(lines, notes)
  notes <- group_tuplets(notes)
  notes <- indicate_tuplets(notes)
  lines <- sort_lines(lines)
  notes <- metricalize(notes, lines, meters)
  notes <- atomize_notes(notes)
  notes <- indicate_ties(notes, music[["ties"]])

  out <- notes[["tie_start"]]
  expected <- c(rep(TRUE, 4), TRUE, FALSE, rep(TRUE, 4), rep(FALSE, 3))
  expect_identical(out, expected)

  out <- notes[["tie_stop"]]
  expected <- c(rep(FALSE, 2), rep(TRUE, 5), FALSE, rep(TRUE, 4), FALSE)
  expect_identical(out, expected)
})
