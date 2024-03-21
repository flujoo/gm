test_that("full start bars and empty end bars are skipped", {
  meters <- (Music() + Meter(4, 4))$meters
  out <- fill_gap(1, 4, 3, 0, meters, 1)

  expected <- data_frame(
    line = 1,
    i = NA_integer_,
    j = NA_integer_,
    pitch = NA_character_,
    midi = NA_integer_,
    duration = NA_character_,
    length = 4,
    grace = FALSE,
    start_bar = 2L,
    start_offset = 0,
    end_bar = 2L,
    end_offset = 4,
    group = 0L
  )

  # In case of `data.frame()` rather than `tibble()` being used
  expected[["tuplet_start"]] <- list(NULL)
  expected[["tuplet_stop"]] <- list(NULL)

  expect_identical(out, expected)
})


test_that("identical start and end positions work", {
  meters <- (Music() + Meter(4, 4))$meters
  out <- fill_gap(1, 0.5, 1, 0.5, meters, 1)
  expect_null(out)
})
