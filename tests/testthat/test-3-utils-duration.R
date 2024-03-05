test_that("untying notes works", {
  music <-
    Music() +
    Meter(3, 8) +
    Line(list(90:91, 80:81, 92, 93, 94:95), list(1.5, 1, "q.", 1.5, 1.5)) +
    Grace(2)

  music <- indicate_grace(music)
  music <- delimit_notes(music)
  music <- untie_notes(music)

  out <- music[["notes"]][["length"]]
  expected <- c(0.5, 0.5, 1, 1, 1, 1, 1.5, 0.5, 1, 0.5, 0.5, 1, 1)
  expect_identical(out, expected)
})
