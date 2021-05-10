library(gm)


test_that("invalid types", {
  # some objects of invalid types
  objects <- list(c, check_pitches, complex(), raw(), expression())

  for (object in objects) {
    type <- typeof(object)
    expect_error(check_pitches(object), type)
  }
})


test_that("`NULL`", {
  # accept `NULL`
  expect_silent(check_pitches(NULL))
})


test_that("logical", {
  # accept empty logical vector and `NA`s
  objects <- list(logical(0), c(NA, NA))

  for (object in objects) {
    expect_silent(check_pitches(object))
  }

  # not accept `TRUE` and `FALSE`
  expect_error(check_pitches(c(NA, FALSE, TRUE)), "2*3")
})


test_that("numeric", {
  # accept empty numeric vector, integers between 12 and 127, and `NA`s
  objects <- list(double(), integer(), 90:93, c(90L, NA), c(90, NA))

  for (object in objects) {
    expect_silent(check_pitches(object))
  }

  # some invalid instances
  expect_error(check_pitches(c(NA, -1, 0, 90.1)), "2*3*4")
})


test_that("character", {
  # accept empty character vector, pitch notations,
  # integers between 12 and 127, and `NA`s
  objects <- list(character(), c("c3", "90", NA), NA_character_)

  for (object in objects) {
    expect_silent(check_pitches(object))
  }

  # some invalid instances
  expect_error(check_pitches(c(NA, "c", "0", "90.1")), "2*3*4")
})


test_that("list", {
  # accept empty list
  expect_silent(check_pitches(list()))

  # accept empty vectors, single `NA`s, `NULL`, pitch notations,
  # and integers between 12 and 127
  objects <- list(
    NULL,
    logical(), character(), double(), integer(),
    NA, NA_character_, NA_real_, NA_integer_,
    90, "C4", "e3", "90",
    c("90", "C4", "e3")
  )

  expect_silent(check_pitches(objects))

  # some instances of invalid items
  objects <- list(
    # invalid types
    list(), c, expression(),
    # `TRUE` and `FALSE`
    TRUE, FALSE, c(TRUE, FALSE),
    # non-single `NA`
    c(90, NA), c("C4", NA),
    # invalid MIDI note numbers and pitch notations
    c(90.1, 0), c("c", "0", "90.1")
  )

  for (i in seq_len(length(objects))) {
    expect_error(check_pitches(objects[i]))
  }
})
