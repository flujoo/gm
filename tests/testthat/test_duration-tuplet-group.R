library(gm)


test_that("incomplete tuplet group", {
  # a single tuplet
  ds <- lapply("q/3", to_Duration)
  e <- expect_error(check_tuplet_group(ds), "1")
  expect_s3_class(e, "incomplete")

  # a non-tuplet and a tuplet
  ds <- lapply(list(1, "q/3"), to_Duration)
  e <- expect_error(check_tuplet_group(ds), "2")
  expect_s3_class(e, "incomplete")

  # more
  ds <- lapply(list(1, 1, "q/3", "q/3"), to_Duration)
  e <- expect_error(check_tuplet_group(ds), "4")
  expect_s3_class(e, "incomplete")
})


test_that("incompatible incoming tuplet", {
  ds <- lapply(list(1, "q/3", "w/4"), to_Duration)
  e <- expect_error(check_tuplet_group(ds), "2")
  expect_s3_class(e, "incompatible")
})


test_that("over-complete tuplet group", {
  ds <- list("q/3", "q/3", tuplet("q", Tupler(3, take = "8."))) %>%
    lapply(to_Duration)

  e <- expect_error(check_tuplet_group(ds), "2")
  expect_s3_class(e, "over-complete")
})


test_that("complete tuplet group", {
  ds <- lapply(list("q/3", "q/3", "q/3"), to_Duration)
  expect_silent(check_tuplet_group(ds))

  # two complex tuplet groups
  ds <- list(
    1,
    "w/3", "w/3/3", tuplet("w/3", Tupler(3, take = 2)), "w/3",
    1,
    "q/3/1/1", "q/3", "q/3"
  ) %>% lapply(to_Duration)

  expect_silent(check_tuplet_group(ds))
})
