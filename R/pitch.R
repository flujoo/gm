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
#' note name, accidental, octave.
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


#' @param pitch_notations A character vector or a list of pitch notation(s)
#' or \code{NA}(s).
to_midis.pitch_notations <- function(pitch_notations) {
  ps <- split.pitch_notations(pitch_notations)

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
  if (is.atomic(x)) {
    l <- length(x)
    if (l == 1) {
      s <- paste0(x, "\n")
    } else if (l > 1) {
      s <- to_string.vector(x, c("<", ">"))
    }
  } else if (is.list(x)) {
    con <- function(x) length(x) > 1
    if (all(sapply(x, is.atomic))) {
      ss <- unlist(delimit.list(x, con, c("<", ">")))
      s <- to_string.vector(ss, c("[", "]"))
    } else if (all(sapply(x, is.list))) {
      f <- function(x) unlist(delimit.list(x, con, c("<", ">")))
      x <- lapply(x, f)
      s <- to_string.list(x)
    }
  }

  cat(s)
  invisible(s)
}


#' @export
`[.PitchChord` <- function(x, i, ...) {
  ks <- i[i %in% (1:length(x))]
  l <- length(ks)
  if (l > 0) {
    p <- unclass(x)[ks]
    if (l == 1) {
      class(p) <- c("PitchNote", "Pitch")
    } else if (l > 1) {
      class(p) <- c("PitchChord", "Pitch")
    }
    return(p)
  }
}


#' @export
to_Element <- function(object, ...) {
  UseMethod("to_Element")
}


#' @export
to_Element.PitchNote <- function(object, ...) {
  p <- split.pitch_notations(unclass(object))

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


#' @export
to_Element.PitchChord <- function(object, ...) {
  e <- list()
  for (i in 1:length(object)) {
    p <- to_Element.PitchNote(object[i])
    if (i == 1) {
      e[[i]] <- p
    } else {
      e[[i]] <- list(Element("chord"), p)
    }
  }
  e
}
