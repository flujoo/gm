#' @keywords internal
#' @export
to_MusicXML.Meter <- function(x, ...) {
  contents <- list(
    MusicXML("beats", x[["number"]]),
    MusicXML("beat-type", x[["unit"]])
  )

  attributes <- if (x[["invisible"]]) list(`print-object` = "no") else NULL
  MusicXML("time", contents, attributes)
}


#' @keywords internal
#' @export
insert.Meter <- function(x, to, ...) {
  bar <- x[["bar"]]

  for (i in seq_along(to[["contents"]])[-1]) {
    to <- insert_attribute(x, to, i, bar)
  }

  to
}
