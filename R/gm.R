#' @title gm: Generate Musical Scores Easily and Show Them Anywhere
#'
#' @description Provides a simple and intuitive language, with which you can
#' create complex music easily. Takes care of all the dirty technical
#' details in converting your music to musical scores and audio files.
#' Works in R Markdown documents, R Jupyter Notebooks and RStudio, so you
#' can embed musical scores and audio files anywhere.
#'
#' @section Author:
#' Renfei Mao <renfeimao@gmail.com>
#'
#' @docType package
#' @name gm
NULL


#' @importFrom magrittr %>% %T>% %<>%
NULL


utils::globalVariables(".")


globals <- new.env()
globals$error_messages <- character(0)
globals$env <- NULL
globals$width <- 75


#' for internal use
#' @keywords internal
#' @export
#' @noRd
to_value <- function(x, ...) {
  UseMethod("to_value")
}


# connect `words` with `conjunction`
coordinate <- function(words, conjunction = "or") {
  l <- length(words)

  if (l == 1) {
    return(words)
  }

  paste(
    paste(words[-l], collapse = ", "),
    conjunction,
    words[l]
  )
}


shorten_string <- function(string, width) {
  l <- nchar(string)

  if (l > width) {
    string <- string %>%
      substr(1, width) %>%
      paste("...")
  }

  string
}


generate_string <- function(general, specifics, env) {
  specifics %>%
    sapply(function(s) paste("*", s)) %>%
    paste(collapse = "\n") %>%
    {ifelse(. == "", "", paste0("\n\n", .))} %>%
    glue::glue(general, ., .envir = env)
}


quote_string <- function(x) {
  if (is.character(x)) {
    if (is.na(x)) {
      "NA_character_"
    } else {
      paste0('"', x, '"')
    }

  } else if (is.integer(x) && is.na(x)) {
    "NA_integer_"

  } else if (is.double(x) && is.na(x)) {
    "NA_real_"

  } else {
    x
  }
}
