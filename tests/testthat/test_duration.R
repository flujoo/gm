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


test_that("validate tuplets", {
  dl <- list(
    Duration("w"),
    Duration("w/4"),
    Duration("w/4/3"), Duration("w/4/3"), # add a deeper level in between later
    Duration("w/4/3"), Duration("w/4", Tupler(3, "8", "q")),
    Duration("w/4")
  )
  expect_error(validate.tuplets(dl))

  # make dl valid
  dl <- append(dl, rep(list(Duration("w/4/3/5")), 5), 3)
  out <- validate.tuplets(dl)
  expected <- list(Duration("w"), Duration("w"))
  expect_equal(out, expected)
})
