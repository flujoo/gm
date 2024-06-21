#' @keywords internal
#' @export
to_MusicXML.Breath <- function(x, ...) {
  symbol <- x[["symbol"]]

  if (is.na(symbol)) {
    MusicXML("breath-mark")

  } else {
    MusicXML("breath-mark", symbol)
  }
}


#' @keywords internal
#' @export
insert.Breath <- function(x, to, ...) {
  insert_articulation(x, to, "last")
}
