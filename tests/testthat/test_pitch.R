library(mr)


test_that("create a Pitch object", {
  # legitimate examples
  pns <- c("C0", "D#1", "E##2", "F-3", "G--4")
  for (pn in pns) {
    out <- Pitch(pn)
    expected <- pn
    class(expected) <- "Pitch"
    expect_equal(out, expected)
  }

  # illegitimate examples
  os <- c(
    "G", # only note name
    "G#",
    "G###3", # incorrect accidental
    "A-#2",
    "C---0",
    "D44", # incorrect octave
    "B#009",
    "c#3" # lower case note name
  )
  for (o in os) {
    expect_error(Pitch(o))
  }
})


test_that("convert pitch notations to MIDI note numbers", {
  pns <- c("C0", "D#1", NA, "E##2", "F-3", "G--4")
  out <- to_midis.pitch_notations(pns)
  expected <- c(12, 27, NA, 42, 52, 65)
  expect_equal(out, expected)
})
