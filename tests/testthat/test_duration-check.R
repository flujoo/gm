library(gm)


test_that("invalid classes", {
  # some objects of invalid classes
  objects <- list(c, complex(), raw(), expression())

  for (object in objects) {
    class <- class(object)
    expect_error(check_durations(object), class)
  }
})


test_that("`NULL`", {
  # accept `NULL`
  expect_silent(check_durations(NULL))
})


test_that("numeric", {
  invalid <- list(100L, 1.11, NA_real_, NA_integer_, Inf, NaN)

  for (object in invalid) {
    expect_error(check_durations(object))
  }
})


test_that("list", {
  # accept empty list
  expect_silent(check_durations(list()))

  # some instances of invalid items
  objects <- list(
    # invalid classes
    list(), c, expression(), TRUE, NULL,
    # invalid lengths
    character(), double(), integer(), c(1, 1), c("q", "h"),
    # invalid contents
    "p", 1.001, 0
  )

  for (i in seq_along(objects)) {
    expect_error(check_durations(objects[i]))
  }
})
