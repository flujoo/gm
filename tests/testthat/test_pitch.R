library(mr)


test_that("midi -> Pitch", {
  # C#/D-
  midi <- 61
  c_ <- to_Pitch.notation("C#4")
  d <- to_Pitch.notation("D-4")
  # C#4 is the default in C major and sharp keys
  expect_equal(to_Pitch.midi(midi, 0), c_)
  # D-4 is the default in flat keys
  expect_equal(to_Pitch.midi(midi, -2), d)
  # D-4 is in key A- major
  expect_equal(to_Pitch.midi(midi, -4), d)
  # D-4 descends to C4
  expect_equal(to_Pitch.midi(midi, 0, to_Pitch.notation("C4")), d)
  # C#4 is the sharp 5th of key F
  expect_equal(to_Pitch.midi(midi, -1), c_)
})


test_that("PitchChord", {
  # empty list is valid as input and will be converted to NA
  expect_equal(PitchChord(list()), NA)

  # but empty lists as items are invalid
  expect_error(PitchChord(list(list())))

  # valid items:
  pc <- list(to_Pitch.midi(60), to_Pitch.midi(61))
  class(pc) <- "PitchChord"
  ps <- list(
    # midis and pitch notations
    "60", 60:61, "C4", c("C4", "C5"),
    # lists as items, Pitches,
    list(60, 61, to_Pitch.midi(60), pc)
  )
  out <- PitchChord(ps)
  expected <- list(
    60L, 60L, 61L,
    to_Pitch.notation("C4"), to_Pitch.notation("C4"), to_Pitch.notation("C5"),
    60L, 61L, to_Pitch.midi(60), to_Pitch.midi(60), to_Pitch.midi(61)
  )
  class(expected) <- "PitchChord"
  expect_equal(out, expected)
})


test_that("PitchLine", {
  pl_1 <- to_Pitch.PitchLine(PitchLine(list(
    # NAs
    NULL, NaN, list(), NA_character_, NA,
    # midis
    "60", 61, 62:63,
    # pitch notations
    "C4", c("C5", "C6"),
    # Pitches and PitchChords
    to_Pitch.midi(70), PitchChord(71:72),
    # lists
    list(90, 91, 92:93, list(94, list(95, 96))),
    list(97)
  )))

  pl_2 <- to_Pitch.PitchLine(PitchLine(list(
    NA, NA, NA, NA, NA,
    to_Pitch.midi(60), to_Pitch.midi(61), PitchChord(62:63),
    to_Pitch.notation("C4"), PitchChord(c("C5", "C6")),
    to_Pitch.midi(70), PitchChord(71:72),
    PitchChord(90:96),
    to_Pitch.midi(97)
  )))

  expect_equal(pl_1, pl_2)
})
