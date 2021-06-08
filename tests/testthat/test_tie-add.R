library(gm)


test_that("start_position_beyond_line_length", {
  e <- expect_error(Music() + Line(90) + Tie(1, 10), "10")
  expect_s3_class(e, "start_position_beyond_line_length")
})


test_that("rest_at_start_position", {
  e <- expect_error(Music() + Line(c(90, NA)) + Tie(1, 2), "2")
  expect_s3_class(e, "rest_at_start_position")
})


test_that("start_position_beyond_chord_length", {
  e <- expect_error(Music() + Line(list(90:91)) + Tie(1, c(1, 9)), "9")
  expect_s3_class(e, "start_position_beyond_chord_length")
})


test_that("stop_position_beyond_line_length", {
  e <- expect_error(Music() + Line(90) + Tie(1, 1), "1")
  expect_s3_class(e, "stop_position_beyond_line_length")
})


test_that("rest_at_stop_position", {
  e <- expect_error(Music() + Line(c(90, NA)) + Tie(1, 1), "1")
  expect_s3_class(e, "rest_at_stop_position")
})
