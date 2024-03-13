test_that("tie adding validations work", {
  # Start and stop positions
  music <- Music() + Line(90)
  expect_error(music + Tie(2))
  expect_error(music + Tie(1))

  # Rests
  music <- Music() + Line(c(NA, 90, NA))
  expect_error(music + Tie(1))
  expect_error(music + Tie(2))

  # Chord length
  music <- Music() + Line(list(c(90, 90), 90))
  expect_error(music + Tie(1, 3))

  # Equivalent pitches
  music <- Music() + Line(list(90, c(91, 92), 91))
  expect_error(music + Tie(1))
  expect_error(music + Tie(2, 2))
})
