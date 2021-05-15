library(gm)


test_that("pitch value", {
  invalid <- list(
    c,
    NULL, NA_real_, NA_integer_, NaN, Inf, "Inf", "NA_real_",
    80.1, "80.1"
  )

  for (v in invalid) {
    expect_false(is_pitch_value(v))
  }

  valid <- list("60", 80)

  for (v in valid) {
    expect_true(is_pitch_value(v))
  }
})
