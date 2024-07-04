

parse_lyric_text <- function(text) {
  # <extend> ---------------------------------------------------

  extend <- NULL

  if (grepl("(?<!\\\\)_$", text, perl = TRUE)) {
    extend <- if (text == "_") "stop" else "start"
  }


  # <syllabic> -------------------------------------------------

  syllabic <- NULL

  if (!grepl("(?<!\\\\)-", text, perl = TRUE) && text != "_") {
    syllabic <- "single"

  } else {
    if (grepl("^(?<!\\\\)-", text, perl = TRUE)) {
      syllabic <- c(syllabic, "end")
    }

    if (grepl("(?<!\\\\)-$", text, perl = TRUE)) {
      syllabic <- c(syllabic, "begin")
    }

    if (length(syllabic) == 2) syllabic <- "middle"
  }


  # <text> -----------------------------------------------------

  if (text %in% c("", "-", "_")) {
    text <- NULL

  } else {
    if (!is.null(extend)) text <- substring(text, 1, nchar(text) - 1)

    if (syllabic %in% c("begin", "middle")) {
      text <- substring(text, 1, nchar(text) - 1)
    }

    if (syllabic %in% c("end", "middle")) {
      text <- substring(text, 2, nchar(text))
    }

    text <- strsplit(text, "(?<!\\\\)_", perl = TRUE)[[1]]
  }


  list(extend = extend, syllabic = syllabic, text = text)
}
