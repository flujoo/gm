library(mr)


test_that("midi -> Pitch", {
  # C#/D-
  midi <- 61
  c_ <- to_Pitch.pitch_notation("C#4")
  d <- to_Pitch.pitch_notation("D-4")
  # C#4 is the default in C major and sharp keys
  expect_equal(to_Pitch.midi(midi, 0), c_)
  # D-4 is the default in flat keys
  expect_equal(to_Pitch.midi(midi, -2), d)
  # D-4 is in key A- major
  expect_equal(to_Pitch.midi(midi, -4), d)
  # D-4 descends to C4
  expect_equal(to_Pitch.midi(midi, 0, to_Pitch.pitch_notation("C4")), d)
  # C#4 is the sharp 5th of key F
  expect_equal(to_Pitch.midi(midi, -1), c_)
})


test_that("PitchChord", {
  # empty list is valid as input and will be converted to NA
  expect_equal(PitchChord(list(), 0, NULL), NA)
  # but empty lists as items are invalid
  expect_error(PitchChord(list(list()), 0, NULL))
  # valid items:
  pc <- list(to_Pitch.midi(60), to_Pitch.midi(61))
  class(pc) <- "PitchChord"
  ps <- list(
    # midis and pitch notations
    "60", 60:61, "C4", c("C4", "C5"),
    # lists as items, Pitches,
    list(60, 61, to_Pitch.midi(60), pc)
  )
  out <- PitchChord(ps, 0, NULL)
  expected <- list(
    to_Pitch.midi(60), to_Pitch.midi(60), to_Pitch.midi(61),
    to_Pitch.pitch_notation("C4"), to_Pitch.pitch_notation("C4"),
    to_Pitch.pitch_notation("C5"), to_Pitch.midi(60),
    to_Pitch.midi(61), to_Pitch.midi(60), to_Pitch.midi(60),
    to_Pitch.midi(61)
  )
  class(expected) <- "PitchChord"
  expect_equal(out, expected)
})


test_that("PitchLine", {
  ps <- list(
    # NAs
    NULL, NaN, list(), NA_character_, NA,
    # midis
    "60", 61, 62:63,
    # pitch notations
    "C4", c("C5", "C6"),
    # Pitches and PitchChords
    to_Pitch.midi(70), PitchChord(71:72, 0, NULL),
    # lists
    list(90, 91, 92:93, list(94, list(95, 96))),
    list(97)
  )
  out <- PitchLine(ps)
  expected <- list(
    NA, NA, NA, NA, NA,
    to_Pitch.midi(60), to_Pitch.midi(61), PitchChord(62:63, 0, NULL),
    to_Pitch.pitch_notation("C4"), PitchChord(c("C5", "C6"), 0, NULL),
    to_Pitch.midi(70), PitchChord(71:72, 0, NULL),
    PitchChord(90:96, 0, NULL),
    to_Pitch.midi(97)
  )
  class(expected) <- "PitchLine"
  expect_equal(out, expected)
})
