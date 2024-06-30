#' @keywords internal
#' @export
to_MusicXML.Slur <- function(x, type, ...) {
  attributes <- list(type = type, number = x[["number"]])
  above <- x[["above"]]

  if (!is.na(above) && type == "start") {
    attributes[["placement"]] <- if (above) "above" else "below"
  }

  MusicXML("slur", attributes = attributes)
}


#' @keywords internal
#' @export
insert.Slur <- function(x, to, ...) {
  start <- x
  start[["j"]] <- NULL
  to <- insert_notation(start, to, "first", "start")

  stop <- x
  stop[["i"]] <- stop[["j"]]
  stop[["j"]] <- NULL

  line_j <- stop[["line_j"]]
  if (!is.na(line_j)) stop[["line"]] <- line_j

  insert_notation(stop, to, "last", "stop")
}
