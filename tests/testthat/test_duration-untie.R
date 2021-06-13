library(gm)


test_that("untie", {
  expect_equal(untie(5.25), c(0.25, 1, 4))
  expect_equal(untie(5.25, TRUE), c(4, 1, 0.25))
})
