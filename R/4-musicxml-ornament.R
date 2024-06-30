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
