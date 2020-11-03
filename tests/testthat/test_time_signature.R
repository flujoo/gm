library(mr)


test_that("add time signature", {
  ds <- list(
    # no time signature for first measure
    4, "128th",
    # 4/4
    list(3, "q/3"), Duration("q", Tupler(3, take = "q")),
    # 3/4 and incomplete last measure
    1, 4
  )
  ps <- rep(list(60), length(ds))
  v <- Voice(ps, ds)

  # tuplet group cross barline
  expect_error(v + TimeSignature(4, 4, 4))
  # tuplet cross barline
  expect_error(v + TimeSignature(7, 8, 3))
  # incomplete measure
  expect_error(v + TimeSignature(4, 4, 5:6))
  # correct
  expect_s3_class(v + TimeSignature(4, 4, 3) + TimeSignature(2, 4, 5), "Voice")
})
