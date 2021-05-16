library(gm)


test_that("take", {
  expect_false(is_take_valid("w", "q", 3))
  expect_false(is_take_valid(4, 1, 3))
  expect_false(is_take_valid("w", list(type = "eighth", dot = 1L), 3))
})
