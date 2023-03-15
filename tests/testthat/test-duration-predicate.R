test_that("check if a number is a duration value", {
  out <- is_duration_value(Inf)
  expect_false(out)

  out <- is_duration_value(NA_integer_)
  expect_false(out)

  out <- is_duration_value(1/256/2)
  expect_false(out)

  out <- is_duration_value(1/3)
  expect_false(out)

  out <- is_duration_value(1)
  expect_true(out)
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

  # implied types are always checked even multipliers are specified
  notation <- "512./4*(1024./1024.)"

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
