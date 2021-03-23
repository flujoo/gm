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


# https://cran.r-project.org/doc/manuals/r-devel/R-exts.html
# #Package-subdirectories
# if a package pkg contains user-level objects which are for “internal” use
# only, it should provide a file pkg-internal.Rd which documents all such
# objects, and clearly states that these are not meant to be called by the
# user. See e.g. the sources for package grid in the R distribution
#' @title Internal gm Functions
#'
#' @description Internal gm functions.
#' These are not to be called by the user.
#'
#' @aliases
#' to_value
#' to_value.Pitch
#' to_value.Tupler
#' to_value.Duration
#' to_value.Meter
#' add
#' add.Clef
#' add.Key
#' add.Meter
#' add.Tempo
#' add.Line
#' normalize_pitch
#' normalize_pitch.numeric
#' normalize_pitch.character
#' normalize_pitch.logical
#' print.PitchLine
#' print.PitchNotation
#' print.PitchValue
#' print.PitchRest
#' print.PitchChord
#' print.Pitch
#' print.ClefLine
#' print.DurationLine
#' print.KeyLine
#' print.MeterLine
#' print.Move
#' print.Note
#' print.Rest
#' print.Measure
#' print.Attributes
#' print.Score
#' print.Element
#' print.TempoLine
#' print.PositionLine
#' to_Pitch
#' to_Pitch.PitchNotation
#' to_Pitch.PitchValue
#' to_Pitch.PitchChord
#' to_Pitch.Line
#' to_Pitch.Music
#' to_Duration
#' to_Duration.Duration
#' to_Duration.numeric
#' to_Duration.character
#' check_duration
#' check_duration.numeric
#' check_duration.character
#' to_Element
#' to_Element.PitchRest
#' to_Element.Pitch
#' to_Element.Clef
#' to_Element.Key
#' to_Element.Meter
#' to_Element.Score
#' to_Element.Part
#' to_Element.Measure
#' to_Element.Attributes
#' to_Element.Move
#' to_Element.Rest
#' to_Element.Note
#' to_Element.Element
#' to_Element.Tempo
#' check_positions_length
#' check_positions_length.numeric
#' check_positions_content
#' check_positions_content.numeric
#' check_positions_content.list
#' ==.Pitch
#' +.ClefLine
#' +.KeyLine
#' +.MeterLine
#' +.TempoLine
#'
#' @keywords internal
#' @name gm-internal
NULL


#' @importFrom magrittr %>% %T>% %<>%
NULL


utils::globalVariables(".")


globals <- new.env()
globals$error_messages <- character(0)
globals$env <- NULL
globals$width <- 75


#' @keywords internal
#' @export
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
