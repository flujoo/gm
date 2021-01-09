library(mr)


test_that("tuplet group", {
  # a single tuplet
  e <- expect_error(DurationLine(list("q/3")))
  expect_equal(class(e)[1], "incomplete")
  expect_match(e$message, "1")

  # a non-tuplet and a tuplet
  e <- expect_error(DurationLine(list(1, "q/3")))
  expect_equal(class(e)[1], "incomplete")
  expect_match(e$message, "2")

  # incompatible
  e <- expect_error(DurationLine(list(1, "q/3", "w/4")))
  expect_equal(class(e)[1], "incompatible")
  expect_match(e$message, "2")

  # incomplete
  e <- expect_error(DurationLine(list(1, "q/3", "q/3")))
  expect_equal(class(e)[1], "incomplete")
  expect_match(e$message, "3")

  # over-complete
  ds <- list("q/3", "q/3", tuplet("q", Tupler(3, take = "8.")))
  e <- expect_error(DurationLine(ds))
  expect_equal(class(e)[1], "over-complete")
  expect_match(e$message, "2")

  # one-level tuplet group
  expect_s3_class(DurationLine(list("q/3", "q/3", "q/3")), "DurationLine")

  # two complex tuplet groups
  ds <- list(
    1,
    "w/3", "w/3/3", tuplet("w/3", Tupler(3, take = 2)), "w/3",
    1,
    "q/3/1/1", "q/3", "q/3"
  )
  expect_s3_class(DurationLine(ds), "DurationLine")
})
