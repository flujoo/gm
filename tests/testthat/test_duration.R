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


test_that("DurationLine", {
  ds <- list(
    1, "q", 4.5, 1:5, c("q", "h..", "w/3", "w/3", "w/3"),
    list(Duration("w"), list("q"), 1)
  )
  out <- DurationLine(ds)
  expected <- list(
    to_Duration.value(1), Duration("q"), to_Duration.value(4.5),
    TiedDurations(list(
      to_Duration.value(1), to_Duration.value(2), to_Duration.value(3),
      to_Duration.value(4), to_Duration.value(4), to_Duration.value(1)
    )),
    TiedDurations(list(
      Duration("q"), Duration("h.."), Duration("w/3"), Duration("w/3"),
      Duration("w/3")
    )),
    TiedDurations(list("w", 1, 1))
  )
  class(expected) <- "DurationLine"
  expect_equal(out, expected)
})
