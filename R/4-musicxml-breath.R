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
