test_that("parsing `text` works", {
  text <- "a"
  out <- parse_lyric_text(text)
  expected <- list(extend = NULL, syllabic = "single", text = "a")
  expect_identical(out, expected)

  text <- "_"
  out <- parse_lyric_text(text)
  expected <- list(extend = "stop", syllabic = NULL, text = NULL)
  expect_identical(out, expected)

  text <- "a_"
  out <- parse_lyric_text(text)
  expected <- list(extend = "start", syllabic = "single", text = "a")
  expect_identical(out, expected)

  text <- "-a"
  out <- parse_lyric_text(text)
  expected <- list(extend = NULL, syllabic = "end", text = "a")
  expect_identical(out, expected)

  text <- "a-"
  out <- parse_lyric_text(text)
  expected <- list(extend = NULL, syllabic = "begin", text = "a")
  expect_identical(out, expected)

  text <- "-a-"
  out <- parse_lyric_text(text)
  expected <- list(extend = NULL, syllabic = "middle", text = "a")
  expect_identical(out, expected)

  text <- "-a_"
  out <- parse_lyric_text(text)
  expected <- list(extend = "start", syllabic = "end", text = "a")
  expect_identical(out, expected)

  text <- "a_b"
  out <- parse_lyric_text(text)
  expected <- list(extend = NULL, syllabic = "single", text = c("a", "b"))
  expect_identical(out, expected)

  text <- "_b"
  out <- parse_lyric_text(text)
  expected <- list(extend = NULL, syllabic = "single", text = c("", "b"))
  expect_identical(out, expected)

  for (text in c("", "-", "_-_", "-_-")) {
    out <- parse_lyric_text(text)
    expect_false(identical(out[["extend"]], "end"))
    expect_null(out[["text"]])
  }
})
