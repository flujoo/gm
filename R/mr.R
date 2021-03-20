#' @title mr: Generate Musical Scores Easily and Show Them Anywhere
#'
#' @description mr provides a simple and intuitive language, with which you
#' can create musical scores and audio files easily. Also, mr works in R
#' Markdown documents, R Jupyter Notebooks and RStudio, so you can embed
#' scores and audio files anywhere. You only tell mr the structure of your
#' music, and it takes care of all the dirty technical details.
#'
#' @section Author:
#' Renfei Mao <renfeimao@gmail.com>
#'
#' @docType package
#' @name mr
NULL



# import functions --------------------------------------------------------

#' @importFrom magrittr %>% %T>% %<>%
NULL



# globals -----------------------------------------------------------------

utils::globalVariables(".")

globals <- new.env()
globals$error_messages <- character(0)
globals$env <- NULL
globals$width <- 75



# generics ----------------------------------------------------------------

#' for internal use
#' @keywords internal
#' @export
to_value <- function(x, ...) {
  UseMethod("to_value")
}


#' @export
show <- function(x, to) {
  UseMethod("show")
}


#' @export
export <- function(x, dir_path, file_name, formats) {
  UseMethod("export")
}



# utils -------------------------------------------------------------------

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
