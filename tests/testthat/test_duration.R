library(mr)


test_that("convert duration type to value", {
  out <- unname(sapply(duration_types, to_value.duration_type))
  expected <- 32 / 2^(0:(length(duration_types) - 1))
  expect_equal(out, expected)
})
