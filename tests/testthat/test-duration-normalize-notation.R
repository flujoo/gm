test_that("normalize complex tuplets", {
  notation <- "h / 4 * (8./q) / 5"

  out <- Duration(notation)

  expected <- list(
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

  class(expected) <- "Duration"
  expect_identical(out, expected)
})
