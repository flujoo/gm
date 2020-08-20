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
#' @details Only apply to non-tuplets and untied durations.
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
#' @details Only apply to non-tuplets.
untie.value <- function(value) {
  # sort duration types in an ascending order
  ts_ <- rev(duration_types)
  # convert duration types and their dotted durations to values
  vs <- unlist(lapply(ts_,
    # convert a duration type and its dotted durations
    function(t_) {
      to_value.duration_type(t_) *
        sapply(0:4, to_value.dot)
    }
  ))

  # recursively
  core <- function(value) {
    if (value %in% vs) {
      return(value)
    } else {
      is_ <- which(vs < value)
      i <- is_[length(is_)]
      v_i <- vs[i]
      return(c(core(value - v_i), v_i))
    }
  }

  core(value)
}


#' @title Find Time Signature to Fit Duration
infer_time_signature <- function(type, dot) {
  # get value
  v <- to_value.duration_type(type)
  if (dot != "") {
    v <- v * 2
  }
  # set the lower limit for value
  v_64th <- to_value.duration_type("64th")
  if (v < v_64th) {
    v <- v_64th
  }

  if (v >= 1) {
    numerator <- as.integer(v)
    denominator <- 4L
  } else {
    numerator <- 1L
    denominator <- as.integer(4 / v)
  }

  list(numerator = numerator, denominator = denominator)
}
