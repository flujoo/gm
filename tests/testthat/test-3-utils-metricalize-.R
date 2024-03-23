test_that("chord metricalization works", {
  music <-
    Music() +
    Meter(3, 4) +

    Line(
      list(90, 93:91, 94, 95:96),
      list(1, "w", 4, 4),
      bar = 2, offset = 0.5
    ) +

    Grace(1)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, meters)
  lines <- delimit_lines(lines, notes)
  notes <- group_tuplets(notes)
  notes <- indicate_tuplets(notes)
  lines <- sort_lines(lines)

  out <- metricalize(notes, lines, meters)
  row.names(out) <- NULL

  expected <- data_frame(
    line = rep(1L, 18),

    i = as.integer(c(
      NA, NA, # Fillings before the Line
      1, # Grace note
      rep(2, 3), rep(2, 3), # First chord
      3, 3, # Note
      rep(4, 2), rep(4, 4), # Second chord
      NA # Fillings after the Line
    )),

    j = as.integer(c(
      NA, NA,
      NA,
      1:3, 1:3,
      NA, NA,
      1:2, 1:2, 1:2,
      NA
    )),

    pitch = rep(NA_character_, 18),

    midi = as.integer(c(
      NA, NA,
      90,
      rep(93:91, 2),
      rep(94, 2),
      rep(95:96, 3),
      NA
    )),

    duration = rep(NA_character_, 18),

    length = c(
      3, 0.5,
      1,
      rep(2.5, 3), rep(1.5, 3),
      1.5, 2.5,
      rep(0.5, 2), rep(3, 2), rep(0.5, 2),
      2.5
    ),

    grace = c(FALSE, FALSE, TRUE, rep(FALSE, 15)),

    start_bar = as.integer(c(
      1, 2,
      NA,
      rep(2, 3), rep(3, 3),
      3, 4,
      rep(4, 2), rep(5, 2), rep(6, 2),
      6
    )),

    start_offset = c(
      0, 0,
      NA,
      rep(0.5, 3), rep(0, 3),
      1.5, 0,
      rep(2.5, 2), rep(0, 2), rep(0, 2),
      0.5
    ),

    end_bar = as.integer(c(
      1, 2,
      NA,
      rep(2, 3), rep(3, 3),
      3, 4,
      rep(4, 2), rep(5, 2), rep(6, 2),
      6
    )),

    end_offset = c(
      3, 0.5,
      NA,
      rep(3, 3), rep(1.5, 3),
      3, 2.5,
      rep(3, 2), rep(3, 2), rep(0.5, 2),
      3
    ),

    group = rep(0L, 18)
  )

  # In case of `data.frame()` rather than `tibble()` being used
  expected[["tuplet_start"]] <- rep(list(NULL), 18)
  expected[["tuplet_stop"]] <- rep(list(NULL), 18)

  expect_identical(out, expected)
})


test_that("metricalization of adjacent chords works", {
  music <-
    Music() +
    Meter(3, 4) +
    Line(list(71:72, 73:74, 75:76, 77:78), rep(4, 4)) +
    Grace(2)

  notes <- music[["notes"]]
  lines <- music[["lines"]]
  meters <- music[["meters"]]

  notes <- indicate_graces(notes, music[["graces"]])
  notes <- delimit_notes(notes, lines, meters)
  lines <- delimit_lines(lines, notes)
  notes <- group_tuplets(notes)
  notes <- indicate_tuplets(notes)
  lines <- sort_lines(lines)
  notes <- metricalize(notes, lines, meters)

  out <- notes[["length"]]
  expected <- c(3, 3, 1, 1, 4, 4, 2, 2, 2, 2, 1, 1, 3, 3)
  expect_identical(out, expected)
})
