library(gm)


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
