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

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, meters)
  lines <- delimit_lines(lines, notes)
  keys <- prepare_keys(music)
  midis <- notes[["midi"]]

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


test_that("inferring pitches from the next pitches works", {
  music <-
    Music() +
    Meter(1, 4) +
    Key(0) +
    Line(list(61, "C4", 61, "D4", 61, NA))

  lines <- music[["lines"]]
  notes <- music[["notes"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, music[["meters"]])
  keys <- prepare_keys(music)
  notes <- infer_pitches(notes, lines, keys)

  out <- notes[["pitch"]]
  expected <- c("D-4", "C4", "C#4", "D4", "C#4", NA)
  expect_identical(out, expected)
})


test_that("inferring pitches from keys works", {
  music <-
    Music() +
    Meter(1, 4) +
    Line(c(61, 61)) +
    Key(7, bar = 1) +
    Key(-7, bar = 2)

  lines <- music[["lines"]]
  notes <- music[["notes"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, music[["meters"]])
  keys <- prepare_keys(music)
  notes <- infer_pitches(notes, lines, keys)

  out <- notes[["pitch"]]
  expected <- c("C#4", "D-4")
  expect_identical(out, expected)
})


test_that("grace notes are considered in pitch inferring", {
  music <-
    Music() +
    Meter(1, 4) +
    Line(list(61, "D4", 61, "C4", 61, "C3")) +
    Grace(1) + Grace(2) + Grace(3) + Grace(5) +
    Key(0) + Key(-7, bar = 2)

  lines <- music[["lines"]]
  notes <- music[["notes"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, music[["meters"]])
  keys <- prepare_keys(music)
  notes <- infer_pitches(notes, lines, keys)

  out <- notes[["pitch"]]
  expected <- c("C#4", "D4", "D-4", "C4", "D-4", "C3")
  expect_identical(out, expected)
})


test_that("chords are considered in pitch inferring", {
  music <-
    Music() +
    Meter(4, 4) +
    Key(0) +

    Line(list(
      61, c("G5", "C4"), 61, c("G5", "A5"),
      c(61, "D4"), "C4", c(61, "C4"), NA,
      c(61, 63), "C4", c("D4", "C4")
    )) +

    Grace(10)

  lines <- music[["lines"]]
  notes <- music[["notes"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, music[["meters"]])
  keys <- prepare_keys(music)
  notes <- infer_pitches(notes, lines, keys)

  out <- notes[["pitch"]]

  expected <- c(
    "D-4", "G5", "C4", "C#4", "G5", "A5",
    "D-4", "D4", "C4", "C#4", "C4", NA,
    "C#4", "E-4", "C4", "D4", "C4"
  )

  expect_identical(out, expected)
})
