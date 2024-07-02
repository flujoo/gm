# Schleifer ----------------------------------------------------

#' @keywords internal
#' @export
to_MusicXML.Schleifer <- function(x, ...) {
  MusicXML("schleifer")
}


#' @keywords internal
#' @export
insert.Schleifer <- function(x, to, ...) {
  insert_ornament(x, to, "first")
}


# Mordent ------------------------------------------------------

#' @keywords internal
#' @export
to_MusicXML.Mordent <- function(x, ...) {
  tag <- if (x[["inverted"]]) "inverted-mordent" else "mordent"
  attributes <- list(long = if (x[["long"]]) "yes" else "no")

  ornament <- x[["ornament"]]

  if (!is.na(ornament)) {
    . <- strsplit(ornament, " ")[[1]]
    position <- switch(.[1], left = "approach", right = "departure")
    direction <- switch(.[2], up = "above", down = "below")
    attributes[[position]] <- direction
  }

  MusicXML(tag, attributes = attributes)
}


#' @keywords internal
#' @export
insert.Mordent <- function(x, to, ...) {
  insert_ornament(x, to, "first")
}


# Turn ---------------------------------------------------------

#' @keywords internal
#' @export
to_MusicXML.Turn <- function(x, ...) {
  tag <- if (x[["inverted"]]) "inverted-turn" else "turn"
  MusicXML(tag)
}


#' @keywords internal
#' @export
insert.Turn <- function(x, to, ...) {
  insert_ornament(x, to, "first")
}


# Trill --------------------------------------------------------

#' @keywords internal
#' @export
to_MusicXML.Trill <- function(x, type = NULL, ...) {
  musicxml_trill <- MusicXML("trill-mark")
  if (is.null(type)) return(musicxml_trill)

  attributes <- list(type = type, number = x[["number"]])
  musicxml_wavy <- MusicXML("wavy-line", attributes = attributes)

  switch(
    type,
    start = list(musicxml_trill, musicxml_wavy),
    stop = musicxml_wavy
  )
}


#' @keywords internal
#' @export
insert.Trill <- function(x, to, ...) {
  j <- x[["j"]]
  if (is.na(j)) return(insert_ornament(x, to, "first"))

  start <- x
  start[["j"]] <- NULL
  to <- insert_ornament(start, to, "first", "start")

  stop <- x
  stop[["i"]] <- j
  stop[["j"]] <- NULL
  insert_ornament(stop, to, "last", "stop")
}
