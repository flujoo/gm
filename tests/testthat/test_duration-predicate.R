library(gm)


test_that("duration value", {
  expect_false(is_duration_value(1024))
  expect_false(is_duration_value("1024"))
  expect_false(is_duration_value(1.1))
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
