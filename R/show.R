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


#' @title Infer "Container" Time Signature
#' @description When showing any object with a Duration, there has to
#' be a time signature to "contain" the object (and derived ones).
#' @details Used in function \code{process.Duration}.
#' @param type_value The value of the \code{type} component of a Duration.
#' Because the function is used inside \code{process.Duration}, so the value
#' is already calculated and can be passed directly.
#' @param dot The \code{dot} component of a Duration.
#' @return A list of two components: the value and the Element of the
#' time signature.
get_container_time_signature <- function(type_value, dot) {
  # get value
  v <- type_value * ifelse(dot == "", 1, 2)
  # since 1/64 is the shortest time signature,
  # the value must be not smaller than that
  v_64th <- to_value.duration_type("64th")
  if (v < v_64th) {
    v <- v_64th
  }

  # get time signature
  if (v >= 1) {
    numerator <- as.integer(v)
    denominator <- 4L
  } else {
    numerator <- 1L
    denominator <- as.integer(4 / v)
  }
  content <- list(
    Element("beats", numerator),
    Element("beat-type", denominator)
  )
  e <- Element("time", content, list(`print-object`= "no"))

  list(value = v, element = e)
}
