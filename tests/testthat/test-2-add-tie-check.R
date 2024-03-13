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


test_that("tie adding works", {
  # Single note and `j = NA`
  music <- Music() + Line(c(90, 90)) + Tie(1)
  out <- locate(music[["ties"]])
  expected <- c(1L, 1L, NA_integer_)
  expect_identical(out, expected)

  # Single note and `j = 1`
  music <- Music() + Line(c(90, 90)) + Tie(1, 1)
  out <- locate(music[["ties"]])
  expected <- c(1L, 1L, NA_integer_)
  expect_identical(out, expected)

  # Chord of only one matched note and specified `j`
  music <- Music() + Line(list(c(90, 91), 90)) + Tie(1, 1)
  out <- locate(music[["ties"]])
  expected <- c(1L, 1L, 1L)
  expect_identical(out, expected)

  # Chord of only one matched note and `j = NA`
  music <- Music() + Line(list(c(90, 91), 90)) + Tie(1)
  out <- locate(music[["ties"]])
  expected <- c(1L, 1L, 1L)
  expect_identical(out, expected)

  # Chord of two matched but competing notes and `j = NA`
  music <- Music() + Line(list(c(90, 90), 90)) + Tie(1)
  out <- locate(music[["ties"]])
  expected <- c(1L, 1L, 1L)
  expect_identical(out, expected)

  # Chord and `j = NA`
  music <- Music() + Line(list(c(90, 91), c(91, 90))) + Tie(1)
  ties <- music[["ties"]]

  out <- locate(ties[1, ])
  expected <- c(1L, 1L, 1L)
  expect_identical(out, expected)

  out <- locate(ties[2, ])
  expected <- c(1L, 1L, 2L)
  expect_identical(out, expected)

  # Update cases
  music <- Music() + Line(list(90:91, 90:91)) + Tie(1) + Tie(1, 1)
  out <- locate(music[["ties"]][2, ])
  expected <- c(1L, 1L, 1L)
  expect_identical(out, expected)
})
