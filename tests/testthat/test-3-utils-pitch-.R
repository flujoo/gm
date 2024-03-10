test_that("inferring pitches from the next pitches works", {
  music <-
    Music() +
    Meter(1, 4) +
    Key(0) +
    Line(list(61, "C4", 61, "D4", 61, NA))

  music[["notes"]] <- indicate_grace(music[["notes"]], music[["graces"]])
  music <- delimit_notes(music)
  music <- infer_pitches(music)

  out <- music[["notes"]][["pitch"]]
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

  music[["notes"]] <- indicate_grace(music[["notes"]], music[["graces"]])
  music <- delimit_notes(music)
  music <- infer_pitches(music)

  out <- music[["notes"]][["pitch"]]
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

  music[["notes"]] <- indicate_grace(music[["notes"]], music[["graces"]])
  music <- delimit_notes(music)
  music <- infer_pitches(music)

  out <- music[["notes"]][["pitch"]]
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

  music[["notes"]] <- indicate_grace(music[["notes"]], music[["graces"]])
  music <- delimit_notes(music)
  music <- infer_pitches(music)

  out <- music[["notes"]][["pitch"]]

  expected <- c(
    "D-4", "G5", "C4", "C#4", "G5", "A5",
    "D-4", "D4", "C4", "C#4", "C4", NA,
    "C#4", "E-4", "C4", "D4", "C4"
  )

  expect_identical(out, expected)
})
