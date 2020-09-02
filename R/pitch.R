#' @title Validate Pitch Notations
#' @param pitch_notations A character vector or a list of pitch notations.
#' @return A logical vector.
validate.pitch_notations <- function(pitch_notations) {
  reg <- paste0(
    "^",
    # always starts with a note name
    "[A-G]",
    # maybe followed by an accidental
    "(#{0,2}|-{0,2})",
    # followed by an octave
    "[0-9]",
    "$"
  )
  grepl(reg, pitch_notations)
}


#' @title Create Pitch Object
#'
#' @description Create an object of S3 class "Pitch",
#' which is to represent the pitch aspect of musical note.
#'
#' @param pitch_notation A character representing a pitch notation.
#'
#' @return The input pitch notation with class \code{"Pitch"}.
#'
#' @export
Pitch <- function(pitch_notation) {
  v <- is.character(pitch_notation) &&
    length(pitch_notation) == 1 &&
    validate.pitch_notations(pitch_notation)

  if (v) {
    class(pitch_notation) <- "Pitch"
    return(pitch_notation)

  } else {
    m <- paste(
      'argument "pitch_notation" should be a character',
      "representing a pitch notation"
    )
    stop(m)
  }
}


#' @title Analyze Pitch Notations
#'
#' @description Split each pitch notation into three parts representing
#' note name, accidental and octave.
#'
#' @param pitch_notations A character vector of pitch notation(s)
#' or \code{NA}(s). List is acceptable, if all members are pitch notations,
#' or \code{NA}s will be coerced into characters and be split improperly.
#'
#' @return A list with \code{"step"}, \code{"alter"}, and \code{"octave"}
#' as names.
analyze.pitch_notations <- function(pitch_notations) {
  l <- nchar(pitch_notations)
  list(
    step = substr(pitch_notations, 1, 1),
    alter = substr(pitch_notations, 2, l - 1),
    octave = substr(pitch_notations, l, l)
  )
}


#' @title Convert Pitch Notations to MIDI Note Numbers
#' @param pitch_notations A character vector or a list of pitch notation(s)
#' or \code{NA}(s).
#' @return A numeric vector representing MIDI note numbers.
to_midis.pitch_notations <- function(pitch_notations) {
  ps <- analyze.pitch_notations(pitch_notations)

  steps <- c(0, 2, 4, 5, 7, 9, 11)
  names(steps) <- c("C", "D", "E", "F", "G", "A", "B")

  alter <- ps$alter
  # can not use "" as name
  alter <- replace(alter, alter == "", " ")
  alters <- c(0, 1, 2, -1, -2)
  names(alters) <- c(" ", "#", "##", "-", "--")

  ps$octave <- suppressWarnings(as.double(ps$octave))

  midis <- steps[ps$step] + alters[alter] + 12 * (ps$octave + 1)
  unname(midis)
}


#' @export
print.Pitch <- function(x, ...) {
  s <- unclass(x)
  cat(s, "\n")
  invisible(s)
}


#' @title Convert Pitch to Element "Pitch"
#' @details MusicXML element "pitch", "step", "alter" and "octave" have no
#' attributes. See \url{https://usermanuals.musicxml.com/MusicXML/
#' Content/EL-MusicXML-pitch.htm}.
to_Element.Pitch <- function(pitch) {
  p <- analyze.pitch_notations(unclass(pitch))

  step_ <- Element("step", p$step)
  octave <- Element("octave", p$octave)

  alter <- p$alter
  if (alter != "") {
    alter <- switch(
      alter,
      "#" = "1",
      "##" = "2",
      "-" = "-1",
      "--" = "-2"
    )
    alter <- Element("alter", alter)
    content <- list(step_, alter, octave)
  } else {
    content <- list(step_, octave)
  }

  Element("pitch", content)
}


#' @title Convert Chord to List of Elements "Note"
#' @param elements Other Elements to constitute these "note" Elements,
#' which include duration-related Elements as the output of other function.
to_Elements_note.Chord <- function(chord, elements) {
  ps <- chord$pitches
  l <- length(ps)

  notes <- list()
  for (i in 1:l) {
    p <- to_Element.Pitch(ps[[i]])
    if (i == 1) {
      content <- append(list(p), elements)
    } else {
      # remove redundant Elements
      m <- length(elements)
      if (m > 0) {
        es <- elements
        for (j in 1:m) {
          e <- es[[j]]
          if (e$tag %in% c("notations")) {
            es[[j]] <- NULL
          }
        }
      }
      # add Element "chord"
      content <- append(list(Element("chord"), p), es)
    }
    notes[[i]] <- Element("note", content)
  }

  notes
}
