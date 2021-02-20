library(mr)


m <- Music() + Line(list(90), list(1))

test_that("first Line", {
  expect_equal(m$lines[[1]]$number, c(1, 1, 1))
})


m <- m + Key(0) + Key(1, to = 1) + Key(2, to = 1, scope = "staff")

test_that("KeyLines", {
  expect_equal(m$key_lines[[1]]$number, c(0, 0))
  expect_equal(m$key_lines[[2]]$number, c(1, 0))
  expect_equal(m$key_lines[[3]]$number, c(1, 1))
})


m <- m + Clef("G", to = 1)

test_that("ClefLine", {
  expect_equal(m$clef_lines[[1]]$number, c(1, 1))
})


m <- m +
  Line(list(40), list(2), to = 1, after = FALSE, as = "staff") +
  Line(list(30), list(3), to = 1, after = FALSE, as = "part")

test_that("update numbers", {
  expect_equal(m$lines[[3]]$number, c(2, 2, 1))
  expect_equal(m$key_lines[[3]]$number, c(2, 2))
  expect_equal(m$clef_lines[[1]]$number, c(2, 2))
})
