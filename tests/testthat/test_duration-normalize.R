library(gm)


test_that("parse duration notation", {
  out <- parse_duration_notation("8/8/8")
  expected <- list(type = "eighth", dot = 0L, ns = c(8L, 8L))
  expect_identical(out, expected)
})


test_that("value <-> Duration", {
  # all duration values
  vs <- duration_types[-1:-2] %>%
    unlist() %>%
    unname()

  for (expected in vs) {
    out <- to_Duration(expected) %>% quantify()
    expect_equal(out, expected)
  }
})


test_that("notation <-> Duration", {
  out <- to_Duration("w../3/3") %>% signify()
  expected <- "whole.. / 3 / 3"
  expect_equal(out, expected)
})
