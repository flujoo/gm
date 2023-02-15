test_that("1024th note is the smallest valid duration value", {
  value <- duration_types[duration_types$abbr == "1024", "value"]

  out <- is_duration_value(value)
  expect_true(out)

  out <- is_duration_value(value * 0.99)
  expect_false(out)
})


test_that("syntactically valid complex tuplets", {
  notation <- "whole./3*(l/16..)/500"

  out <- has_duration_notation_syntax(notation)
  expect_true(out)
})


test_that("duration bases should be multiples of the 1024th note", {
  notation <- "1024."

  # syntactically valid
  out <- has_duration_notation_syntax(notation)
  expect_true(out)

  # semantically invalid
  out <- has_duration_notation_semantics(notation)
  expect_false(out)

  # however, duration bases in tuplet ratios do not have to
  # be multiples of the 1024th note
  notation <- "512./3*(1024./1024.)"

  # syntactically valid
  out <- has_duration_notation_syntax(notation)
  expect_true(out)

  # also semantically valid
  out <- has_duration_notation_semantics(notation)
  expect_true(out)
})


test_that("tuplets should not imply types shorter than the 1024th note", {
  notation <- "1024th/3"

  # syntactically valid
  out <- has_duration_notation_syntax(notation)
  expect_true(out)

  # semantically invalid
  out <- has_duration_notation_semantics(notation)
  expect_false(out)
})


test_that("tuplet ratios should not have values larger than 1", {
  notation <- "w/3*(w/16)"

  # syntactically valid
  out <- has_duration_notation_syntax(notation)
  expect_true(out)

  # semantically invalid
  out <- has_duration_notation_semantics(notation)
  expect_false(out)
})


test_that("units should divide their previous bases", {
  notation <- "h/3*(q./q.)"

  # syntactically valid
  out <- has_duration_notation_syntax(notation)
  expect_true(out)

  # semantically invalid
  out <- has_duration_notation_semantics(notation)
  expect_false(out)
})
