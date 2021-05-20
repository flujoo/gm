library(gm)


test_that("simple tuplet group", {
  ds <- lapply(list("q/3", "q/3", "q/3"), to_Duration) %>% mark_tuplets()

  # check `$tuplet_start`
  out <- lapply(ds, function(t) t$tuplet_start)
  expected <- list(1, NULL, NULL)
  expect_equal(out, expected)

  # check `$tuplet_stop`
  out <- lapply(ds, function(t) t$tuplet_stop)
  expected <- list(NULL, NULL, 1)
  expect_equal(out, expected)
})


test_that("complex tuplet group", {
  ds <- list(
    1,
    "w/3", "w/3/3", tuplet("w/3", Tupler(3, take = 2)), "w/3",
    1,
    "q/3/1/1", "q/3", "q/3"
  ) %>%
    lapply(to_Duration) %>%
    mark_tuplets()

  # check `$tuplet_start`
  out <- lapply(ds, function(t) t$tuplet_start)

  expected <- list(
    NULL,
    1, 2, NULL, NULL,
    NULL,
    c(1, 2, 3), NULL, NULL
  )

  expect_equal(out, expected)

  # check `$tuplet_stop`
  out <- lapply(ds, function(t) t$tuplet_stop)

  expected <- list(
    NULL,
    NULL, NULL, 2, 1,
    NULL,
    c(2, 3), NULL, 1
  )

  expect_equal(out, expected)
})
