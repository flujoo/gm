library(gm)


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
