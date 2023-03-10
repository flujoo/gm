test_that("separate and restore grace notes", {
  expected <-
    Music() +
    Line(list(80, 81:82, 83)) + Grace(2) +
    Line(list(NA, 91, 92)) + Grace(2)

  out <- restore_grace_notes(separate_grace_notes(expected))
  expect_identical(out, expected)
})
