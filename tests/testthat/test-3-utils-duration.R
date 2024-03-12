test_that("untying notes works", {
  music <-
    Music() +
    Meter(3, 8) +
    Line(list(90:91, 80:81, 92, 93, 94:95), list(1.5, 1, "q.", 1.5, 1.5)) +
    Grace(2)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_grace(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, meters)
  notes <- atomize_notes(notes)

  out <- notes[["length"]]
  expected <- c(0.5, 0.5, 1, 1, 1, 1, 1.5, 0.5, 1, 0.5, 0.5, 1, 1)
  expect_identical(out, expected)
})
