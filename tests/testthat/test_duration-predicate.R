library(gm)


test_that("duration value", {
  invalid <- list(
    c,
    NA_real_, NA_integer_, NULL, NaN, Inf,
    1024, "1024", 1.1
  )

  for (v in invalid) {
    expect_false(is_duration_value(v))
  }
})


test_that("duration notation", {
  expect_true(is_duration_notation("8"))
  expect_false(is_duration_notation(8))

  expect_true(is_duration_notation("quarter"))
  expect_true(is_duration_notation("q"))
  expect_false(is_duration_notation("qq"))

  expect_true(is_duration_notation("q...."))
  expect_false(is_duration_notation("q....."))

  expect_true(is_duration_notation("q/1"))
  expect_false(is_duration_notation("q/0"))
})
