library(mr)


test_that("Duration", {
  out <- Duration("w/3", Tupler(3), Tupler(3, take = "q."))
  expected <- list(
    type = "whole", dot = 0,
    tuplers = list(
      list(
        n = 3,
        unit = list(type = "half", dot = 0),
        take = list(type = "half", dot = 0)
      ),
      list(
        n = 3,
        unit = list(type = "quarter", dot = 0),
        take = list(type = "quarter", dot = 0)
      ),
      list(
        n = 3,
        unit = list(type = "eighth", dot = 0),
        take = list(type = "quarter", dot = 1)
      )
    )
  )
  class(expected) <- "Duration"
  expect_equal(out, expected)

  # too long take
  expect_error(Duration("h", Tupler(3, take = "whole")))

  # invalid unit
  expect_error(Duration("h.", Tupler(3, "q..")))
})
