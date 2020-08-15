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


#' @title Convert Value to Duration
#' @details Only apply to non-tuplets.
to_Duration.value <- function(value) {
  # values of all types
  vs_type <- sapply(duration_types, to_value.duration_type)
  # values of 0-4 dots
  vs_dot <- sapply(0:4, to_value.dot)
  # "undot" the given value
  vs_undot <- value / vs_dot
  # infer the number of dots
  n_dot <- which(vs_undot %in% vs_type) - 1
  # n -> dot notation
  dot <- paste(rep(".", n_dot), collapse = "")
  # infer the type
  i_type <- which(vs_type == value / vs_dot[n_dot + 1])
  type <- duration_types[i_type]

  d <- list(type = type, dot = dot, tie = "", tuplets = list())
  class(d) <- "Duration"
  d
}
