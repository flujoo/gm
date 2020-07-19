#' @return A logical vector.
validate.pitch_notations <- function(objects) {
  reg <- "^[A-G](#{0,2}|-{0,2})[0-9]$"
  grepl(reg, objects)
}


#' @title Create Pitch Object
#'
#' @description Create an object of S3 class "Pitch",
#' which is to represent the pitch aspect of music.
#'
#' @export
Pitch <- function(object) {

  l <- length(object)
  # match pitch notation
  reg <- "^[A-G](#{0,2}|-{0,2})[0-9]$"
  # error message
  m <- "invalid input to Pitch"

  if (is.atomic(object)) {

    # PitchRest
    # notice that `is.na(NaN) == TRUE`
    # exclude NaN because it coerces into character,
    # when combined into a character vector
    if (is.na(object) && l == 1 && !is.nan(object)) {
      class(object) <- c("PitchRest", "Pitch")
      return(object)

    # PitchNote
    } else if (all(grepl(reg, object))) {
      if (l == 1) {
        class(object) <- c("PitchNote", "Pitch")
        return(object)

      # PitchChord
      # `l > 1` instead of just `else` to exclude NULL,
      # since `all(grepl(reg, NULL)) == TRUE`
      } else if (l > 1) {
        class(object) <- c("PitchChord", "Pitch")
        return(object)
      }
    }
  }

  if (is.list(object) && l > 0) {

    # PitchVoice
    if (all(sapply(object, is.atomic))) {
      for (i in 1:l) {
        object[[i]] <- Pitch(object[[i]])
      }
      class(object) <- c("PitchVoice", "Pitch")
      return(object)

    # PitchVoices
    } else if (all(sapply(object, is.list))) {
      for (i in 1:l) {
        o <- object[[i]]
        if (all(sapply(o, is.atomic))) {
          object[[i]] <- Pitch(o)
        } else {
          stop(m, call. = FALSE)
        }
      }
      class(object) <- c("PitchVoices", "Pitch")
      return(object)
    }
  }

  stop(m, call. = FALSE)
}


split.pitch_notation <- function(pitch_notation) {
  l <- nchar(pitch_notation)
  list(
    step = substr(pitch_notation, 1, 1),
    alter = substr(pitch_notation, 2, l - 1),
    octave = substr(pitch_notation, l, l)
  )
}


to_midi.pitch_notation <- function(pitch_notation) {
  p <- split.pitch_notation(pitch_notation)

  steps <- c(0, 2, 4, 5, 7, 9, 11)
  names(steps) <- c("C", "D", "E", "F", "G", "A", "B")

  alter <- p$alter
  # can not use "" as name
  alter <- replace(alter, alter == "", " ")
  alters <- c(0, 1, 2, -1, -2)
  names(alters) <- c(" ", "#", "##", "-", "--")

  p$octave <- as.double(p$octave)

  midi <- steps[p$step] + alters[alter] + 12 * (p$octave + 1)
  unname(midi)
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
to_Element.Pitch <- function(object, ...) {
  type <- class(object)[1]

  if (type == "PitchNote") {
    object <- unclass(object)
    p <- split.pitch_notation(object)
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

    return(Element("pitch", content))
  }

  if (type == "PitchRest") {
    return(Element("rest"))
  }

  if (type == "PitchChord") {
    e <- list()
    for (i in 1:length(object)) {
      p <- to_Element.Pitch(object[i])
      if (i == 1) {
        e[[i]] <- p
      } else {
        e[[i]] <- list(Element("chord"), p)
      }
    }
    return(e)
  }

  if (type == "PitchVoice") {
    e <- list()
    for (i in 1:length(object)) {
      o <- object[[i]]
      if (class(o)[1] %in% c("PitchRest", "PitchNote")) {
        e[[i]] <- to_Element.Pitch(o)
      } else {
        e <- append(e, to_Element.Pitch(o), i - 1)
      }
    }
    return(e)
  }

  if (type == "PitchVoices") {
    return(lapply(object, to_Element.Pitch))
  }
}
