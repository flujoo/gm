#' @keywords internal
#' @export
to_MusicXML.Accidental <- function(x, ...) {
  MusicXML(
    "accidental",
    x[["name"]],
    if (x[["bracket"]]) list(parentheses = "yes") else NULL
  )
}


#' @keywords internal
#' @export
insert.Accidental <- function(x, to, ...) {
  insert_note_child(x, to, "first")
}
