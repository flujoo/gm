test_that("tied complex tuplets", {
  notation <- "w./3 - h/4*(8./q)/5"

  duration <- list(
    # "w./3"
    list(
      type = "whole",
      dot = 1L,
      ratios = list(
        list(n = 3L, take = NULL, unit = NULL)
      )
    ),

    # "h/4*(8./q)/5"
    list(
      type = "half",
      dot = 0L,
      ratios = list(
        list(
          n = 4L,
          take = list(type = "eighth", dot = 1L),
          unit = list(type = "quarter", dot = 0L)
        ),

        list(n = 5L, take = NULL, unit = NULL)
      )
    )
  )

  class(duration) <- "Duration"

  expect_identical(Duration(notation), duration)
})
