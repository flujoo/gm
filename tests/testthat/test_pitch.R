library(mr)


test_that("PitchValue -> Pitch", {
  # C#4 or D-4
  v <- PitchValue(61)
  # C#4
  c_sharp <- Pitch("C", 1, 4)
  d_flat <- Pitch("D", -1, 4)

  # C#4 is the default in C major and sharp keys
  expect_equal(
    to_Pitch(v, key = 0),
    c_sharp
  )

  # D-4 is the default in flat keys
  expect_equal(
    to_Pitch(v, key = -2),
    d_flat
  )

  # D-4 is in key A- major
  expect_equal(
    to_Pitch(v, key = -4),
    d_flat
  )

  # D-4 descends to C4
  expect_equal(
    to_Pitch(v, key = 0, after = Pitch("C", 0, 4)),
    d_flat
  )

  # C#4 is the sharp 5th of key F
  expect_equal(
    to_Pitch(v, key = -1),
    c_sharp
  )
})
