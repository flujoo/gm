library(gm)


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


test_that("tuplet group over bar", {
  ds <- list(1, "q/2", "q/2", "h/1") %>%
    DurationLine() %>%
    .$durations

  # no group over bar
  ks <- locate_tuplet_groups_over_bar(
    ds, list(Meter(2, 4, 1)), bar = 1, offset = 0
  )
  expect_identical(ks, integer(0))

  # first group over bar
  ks <- locate_tuplet_groups_over_bar(
    ds, list(Meter(1, 8, 1), Meter(4, 4, 5)), bar = 1, offset = 0
  )
  expect_equal(ks, 2)

  # second group over bar
  ks <- locate_tuplet_groups_over_bar(
    ds, list(Meter(3, 4, 1), Meter(1, 8, 3)), bar = 2, offset = 1
  )
  expect_equal(ks, 4)
})
