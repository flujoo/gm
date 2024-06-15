#' @keywords internal
#' @export
to_MusicXML.Accidental <- function(x, ...) {
  MusicXML(
    "accidental",
    x[["name"]],
    if (x[["bracket"]]) list(bracket = "yes") else NULL
  )
}
