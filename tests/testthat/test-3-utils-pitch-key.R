test_that("key finding works", {
  music <-
    Meter(1, 4) +
    Music() + Key(0) + Key(1, bar = 2) +
    Line(81:85) + Key(2, to = 1) + Key(3, bar = 3, to = 1) +
    Line(91:95, as = "staff") + Key(4, bar = 4, to = 2, scope = "staff") +
    Line(61:65)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_grace(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, meters)
  lines <- delimit_lines(lines, notes)

  midis <- notes[["midi"]]
  keys <- locate_keys(music[["keys"]], lines)

  out <- find_key(notes[midis == 61, ], notes, keys, lines)
  expected <- 0L
  expect_identical(out, expected)

  out <- find_key(notes[midis == 65, ], notes, keys, lines)
  expected <- 1L
  expect_identical(out, expected)

  out <- find_key(notes[midis == 82, ], notes, keys, lines)
  expected <- 2L
  expect_identical(out, expected)

  out <- find_key(notes[midis == 83, ], notes, keys, lines)
  expected <- 3L
  expect_identical(out, expected)

  out <- find_key(notes[midis == 91, ], notes, keys, lines)
  expected <- 2L
  expect_identical(out, expected)

  out <- find_key(notes[midis == 95, ], notes, keys, lines)
  expected <- 4L
  expect_identical(out, expected)
})
