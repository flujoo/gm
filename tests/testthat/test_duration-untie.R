library(gm)


test_that("untie", {
  expect_equal(untie(5.25), c(0.25, 1, 4))
  expect_error(untie(1.1))
})


test_that("is_tied", {
  expect_true(is_tied(1))
  expect_true(is_tied(5.25))
  expect_false(is_tied(1.1))
})
