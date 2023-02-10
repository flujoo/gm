test_that("tuplets with invalid takes or units are nevertheless valid", {
  notation <- "whole./3*(l/16..)/500"

  out <- is_duration_notation(notation)
  expect_true(out)
})


test_that("1024th note is the smallest valid duration value", {
  value <- duration_types[duration_types$abbr == "1024", "value"]

  out <- is_duration_value(value)
  expect_true(out)

  out <- is_duration_value(value * 0.99)
  expect_false(out)
})
