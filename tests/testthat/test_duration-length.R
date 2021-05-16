library(gm)


test_that("skip empty vectors and list", {
  empty <- list(NULL, integer(), double(), character(), list())

  for (object in empty) {
    expect_silent(check_duration_lengths(object))
  }
})


test_that("check duration lengths", {
  # skip Durations
  expect_silent(check_duration_lengths(list(to_Duration("1024."))))

  # error message nuances
  expect_error(check_duration_lengths(duration_types[[14, 5]]), "in which")
  expect_error(check_duration_lengths("1024/3"), "which is shorter")
  expect_error(check_duration_lengths("512../3"), "in which")
})
