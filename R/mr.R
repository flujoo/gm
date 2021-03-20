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


#' @title Show Object
#'
#' @description Show an object as musical score or audio file.
#'
#' @param x An object.
#'
#' @param to Optional. A character vector which contains "score", "audio" or
#' both, which indicates whether to show the object as musical score or
#' audio file.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' m <- Music() + Meter(4, 4) + Line(list("C4"), list(4))
#' show(m, c("score", "audio"))
#' @export
show <- function(x, to) {
  UseMethod("show")
}


#' @title Export Object
#'
#' @description Export an object to various file formats.
#'
#' @param x An object.
#'
#' @param dir_path A single character which specifies the directory to
#' which to export the object.
#'
#' @param file_name A single character which specifies the name of the
#' exported file(s).
#'
#' @param formats A character vector which specifies the file formats.
#' Supported file formats are "mscz", "mscx", "pdf", "png", "svg", "wav",
#' "mp3", "flac", "ogg", "midi", "mid", "musicxml", "mxl", "xml", "metajson",
#' "mlog", "mpos" and "spos".
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' m <- Music() + Meter(4, 4) + Line(list("C4"), list(4))
#' export(m, tempdir(), "x", c("mp3", "png"))
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
