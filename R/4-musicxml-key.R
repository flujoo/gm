#' @keywords internal
#' @export
to_MusicXML.Key <- function(x, ...) {
  MusicXML(
    "key",
    MusicXML("fifths", x[["key"]]),
    list(number = x[["staff"]])
  )
}


#' @keywords internal
#' @export
insert.Key <- function(x, to, ...) {
  insert_attribute(x, to, x[["part"]] + 1, x[["bar"]])
}
