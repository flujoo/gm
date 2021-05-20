library(gm)


test_that("check `$unit$type`", {
  e <- expect_error(tuplet("1024", Tupler(3)))
  expect_s3_class(e, "check_tuplet_unit")
})


test_that("check `$take`", {
  e <- expect_error(tuplet("q", Tupler(3, take = "w")))
  expect_s3_class(e, "check_tuplet_take")
})


test_that("check divisible", {
  e <- expect_error(tuplet("w", Tupler(3, "q.")))
  expect_s3_class(e, "divisible")
})


test_that("check output duration", {
  e <- expect_error(tuplet("512", Tupler(3)))
  expect_s3_class(e, "check_tuplet_out")
})


test_that("tuplet", {
  expect_identical(
    tuplet("w/3/3"),
    tuplet("w", Tupler(3), Tupler(3, "q", "q"))
  )
})
