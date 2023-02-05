test_that("tuplets with invalid takes or units are nevertheless valid", {
  expect_true(is_duration_notation("q.... - whole./3*(l/16..)/500"))
})
