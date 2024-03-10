test_that("tuplet grouping works", {
  durations_1 <- list(
    1, # 0
    "q/3", "q/3", # 1
    4, # 0
    "q/3", # 1
    "q/3", # -2
    "q", # 0
    "q/3/3", # -2
    "q/3", # - 2
    2, # 0
    "h/3", "h/3", # -3
    "h/3*(h/q)", # -2
    3, # 0
    "q/3", "q/3/3", "q/3/3*(8/16)", "q/3", # 2
    "q/3" # -1
  )

  durations_2 <- "q/3" # -4

  music <-
    Music() +
    Line(90, durations_1) + Grace(4) +
    Line(NULL, durations_2)

  music[["notes"]] <- indicate_grace(music[["notes"]], music[["graces"]])
  out <- group_tuplets(music)$notes$group

  expected <- as.integer(c(
    0, 1, 1, 0, 1, -2, 0, -2, -2, 0, rep(-3, 2), -2, 0, rep(2, 4), -1, -4
  ))

  expect_identical(out, expected)
})
