library(gm)


m <- Music()

# add first Line
m <- m + Line("C4")

test_that("add first Line", {
  # check the Music's `$lines`
  expected <- tibble::tibble(
    name = NA_character_,
    part = 1L,
    staff = 1L,
    voice = 1L,
    segment = 1L,
    bar = 1L,
    offset = 0
  )

  expect_identical(m$lines, expected)

  # check the Music's `$pitches`
  pitches <- m$pitches

  expect_equal(nrow(pitches), 1L)
  expect_identical(pitches$line[1], 1L)
  expect_identical(pitches$notation[1], "C4")
})


# add more Lines
m <- m + Line("D4", as = "voice", name = "D4", bar = 2) +
  Line("E4", as = "staff", after = FALSE, offset = 10) +
  Line("F4", to = 1, after = FALSE, name = "F4")

test_that("add more Lines", {
  # check the Music's `$lines`
  expected <- tibble::tibble(
    name = c(NA_character_, "D4", NA_character_, "F4"),
    part = c(2L, 2L, 2L, 1L),
    staff = c(2L, 2L, 1L, 1L),
    voice = c(1L, 2L, 1L, 1L),
    segment = c(1L, 1L, 1L, 1L),
    bar = c(1L, 2L, 1L, 1L),
    offset = c(0, 0, 10, 0)
  )

  expect_identical(m$lines, expected)
})


test_that("check `name` and `to`", {
  expect_error(m + Line(90, name = "D4"), "D4")
  expect_error(m + Line(90, to = "xx"), "xx")
  expect_error(m + Line(90, to = 100), "100")
})


test_that("check voice limit", {
  m <- m + Line(90, as = "voice") +
    Line(90, as = "voice") +
    Line(90, as = "voice")

  expect_error(m + Line(90, as = "voice"), "at most")
})
