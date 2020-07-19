library(mr)


m <- "invalid input to Pitch"


test_that("PitchRest", {
  # any type of NA is acceptable
  nas <- list(NA, NA_character_, NA_integer_, NA_real_, NA_complex_)
  for (na in nas) {
    out <- Pitch(na)
    expected <- na
    class(expected) <- c("PitchRest", "Pitch")
    expect_equal(out, expected)
  }

  # NaN and NULL are not acceptable
  expect_error(Pitch(NaN), m)
  expect_error(Pitch(NULL), m)
})


test_that("PitchNote", {
  # legitimate examples
  pns <- c("C0", "D#1", "E##2", "F-3", "G--4")
  for (pn in pns) {
    out <- Pitch(pn)
    expected <- pn
    class(expected) <- c("PitchNote", "Pitch")
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
    expect_error(Pitch(o), m)
  }
})


test_that("PitchChord", {
  out <- Pitch(c("C0", "D#1", "E##2", "F-3", "G--4"))
  expected <- c("C0", "D#1", "E##2", "F-3", "G--4")
  class(expected) <- c("PitchChord", "Pitch")
  expect_equal(out, expected)

  # can not contain NA
  expect_error(Pitch(c("C0", "D#1", NA)), m)
})


test_that("PitchVoice", {
  pv <- list(NA, "G#4", c("G--3", "A2"))
  out <- Pitch(pv)
  expected <- pv
  class(expected) <- c("PitchVoice", "Pitch")
  class(expected[[1]]) <- c("PitchRest", "Pitch")
  class(expected[[2]]) <- c("PitchNote", "Pitch")
  class(expected[[3]]) <- c("PitchChord", "Pitch")
  expect_equal(out, expected)

  # various kinds of invalidity
  expect_error(
    Pitch(list(NA, "G#4", c("G--3", "A2"), "X")),
    m
  )
  expect_error(
    Pitch(list(NA, "G#4", c(NA, "A2"))),
    m
  )
  expect_error(
    Pitch(list(NA, "G#4", c("G--3", "A2"), list("D#4"))),
    m
  )
})


test_that("PitchVoices", {
  pvs <- list(
    list("G3", NA, c("F-0", "C--9")),
    list(NA, "E3", "F5")
  )
  out <- Pitch(pvs)
  expected <- pvs
  class(expected) <- c("PitchVoices", "Pitch")
  class(expected[[1]]) <- c("PitchVoice", "Pitch")
  class(expected[[2]]) <- c("PitchVoice", "Pitch")
  class(expected[[1]][[1]]) <- c("PitchNote", "Pitch")
  class(expected[[1]][[2]]) <- c("PitchRest", "Pitch")
  class(expected[[1]][[3]]) <- c("PitchChord", "Pitch")
  class(expected[[2]][[1]]) <- c("PitchRest", "Pitch")
  class(expected[[2]][[2]]) <- c("PitchNote", "Pitch")
  class(expected[[2]][[3]]) <- c("PitchNote", "Pitch")
  expect_equal(out, expected)

  # every component must also be a list
  o1 <- list(
    list("G3", NA, c("F-0", "C--9")),
    list(NA, "E3", "F5"),
    "B6"
  )
  expect_error(Pitch(o1), m)
  # two levels maximum
  o2 <- list(
    list("G3", NA, c("F-0", "C--9")),
    list(list(NA, "E3", "F5")),
    "B6"
  )
  expect_error(Pitch(o2), m)
})


test_that("pitch notation -> midi", {
  pns <- c("C0", "D#1", NA, "E##2", "F-3", "G--4")
  out <- to_midis.pitch_notations(pns)
  expected <- c(12, 27, NA, 42, 52, 65)
  expect_equal(out, expected)
})


test_that("print short Pitch", {
  out <- print.Pitch(Pitch(NA))
  expected <- "NA\n"
  expect_equal(out, expected)

  out <- print.Pitch(Pitch("G#4"))
  expected <- "G#4\n"
  expect_equal(out, expected)

  out <- print.Pitch(Pitch(c("F--9", "D#4")))
  expected <- "<F--9, D#4>\n"
  expect_equal(out, expected)
})


# # print long PitchChord
# p <- rep(c("F--9", "D#4"), 10)
# Pitch(p)


test_that("print short PitchVoice", {
  out <- print.Pitch(Pitch(list("F--9")))
  expected <- "[F--9]\n"
  expect_equal(out, expected)

  out <- print.Pitch(Pitch(list(c("F--9", "D#3"))))
  expected <- "[<F--9, D#3>]\n"
  expect_equal(out, expected)

  out <- print.Pitch(Pitch(list(NA, c("F--9", "D#3"), "G2")))
  expected <- "[NA, <F--9, D#3>, G2]\n"
  expect_equal(out, expected)
})


# # print long PitchVoice
# # only PitchRest
# p <- rep(list(NA), 50)
# Pitch(p)
#
# # only PitchNote
# p <- rep(list("C##4"), 50)
# Pitch(p)
#
# # only PitchChord
# p <- rep(list(rep("G2", 10), rep(c("C4", "F3"), 10)), 2)
# Pitch(p)
#
# # mixed
# p <- rep(list(NA, c("G3", "F3"), "D#2"), 40)
# Pitch(p)
