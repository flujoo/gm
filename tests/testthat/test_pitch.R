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
