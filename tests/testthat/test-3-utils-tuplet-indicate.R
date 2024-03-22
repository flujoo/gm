test_that("tuplet indication works", {
  durations <- c(
    "h", rep("h/3", 3),
    "w/1/1",
    "w/3", "w/3/3", "w/3/3*(h/q)", "w/3"
  )

  music <- Music() + Line(durations = durations)

  notes <- indicate_graces(music[["notes"]], music[["graces"]])
  notes <- group_tuplets(notes)
  notes <- indicate_tuplets(notes)

  out <- notes[["tuplet_start"]]
  expected <- list(NULL, 1L, NULL, NULL, 1:2, 1L, 2L, NULL, NULL)
  expect_identical(out, expected)

  out <- notes[["tuplet_stop"]]
  expected <- list(NULL, NULL, NULL, 1L, 1:2, NULL, NULL, 2L, 1L)
  expect_identical(out, expected)
})
