#' @title Add XML Declaration and DOCTYPE to MusicXML Object
preface.MusicXML <- function(musicxml) {
  m <- paste(
    '<?xml version="1.0" encoding="UTF-8" standalone="no"?>',
    '<!DOCTYPE score-partwise PUBLIC',
    '"-//Recordare//DTD MusicXML 3.1 Partwise//EN"',
    '"http://www.musicxml.org/dtds/partwise.dtd">',
    unclass(musicxml),
    sep = "\n"
  )
  m <- paste0(m, "\n")
  class(m) <- "MusicXML"
  m
}


#' @title Convert Value to Duration Type and Dot Notation
#' @details Only applies to non-tuplets and untied durations.
to_type_dot.value <- function(value) {
  # values of all types
  vs_type <- sapply(duration_types, to_value.duration_type)
  # values of 0-4 dots
  vs_dot <- sapply(0:4, to_value.dot)
  # "undot" the given value
  vs_undot <- value / vs_dot
  # infer the number of dots
  i_dot <- which(vs_undot %in% vs_type)
  # n -> dot notation
  dot <- paste(rep(".", i_dot - 1), collapse = "")
  # infer the type
  i_type <- which(vs_type == value / vs_dot[i_dot])
  type <- duration_types[i_type]

  list(type = type, dot = dot)
}


#' @title Partition Duration Value
#' @description Partition a duration value into many units.
partition.value <- function(value, unit) {
  ratio <- value / unit
  ratio_l <- floor(ratio)
  if (ratio == ratio_l) {
    v <- rep(unit, ratio)
  } else {
    d <- ratio - ratio_l
    v <- c(unit * d, rep(unit, ratio_l))
  }
  v
}


#' @title Untie Duration Value
#' @details Only applies to non-tuplets.
#' @param value A double representing a duration value to untie.
#' @param axis_ A vector of doubles, against which the value is untied.
#' If a string is supplied, it is converted to doubles first.
untie.value <- function(value, axis_ = "type") {
  # convert a keyword to real axis
  if (identical(axis_, "type")) {
    axis_ <- unname(sapply(rev(duration_types), to_value.duration_type))
  } else if (identical(axis_, "dot")) {
    # convert a duration type and its dotted durations to values
    f <- function(t_) {
      to_value.duration_type(t_) * sapply(0:4, to_value.dot)
    }
    axis_ <- unlist(lapply(rev(duration_types), f))
  }

  # recursively
  core <- function(value) {
    if (value %in% axis_) {
      return(value)
    } else {
      is_ <- which(axis_ < value)
      i <- is_[length(is_)]
      v_i <- axis_[i]
      return(c(core(value - v_i), v_i))
    }
  }

  core(value)
}


#' @title Convert Value to TimeSignature Object
#' @details Used in function \code{process.Duration},
#' only applies to value of untied duration type.
to_TimeSignature.value <- function(value) {
  if (value >= 1) {
    numerator <- as.integer(value)
    denominator <- 4L
  } else {
    numerator <- 1L
    denominator <- as.integer(4 / value)
  }

  ts_ <- list(numerator = numerator, denominator = denominator)
  class(ts_) <- "TimeSignature"
  ts_
}
