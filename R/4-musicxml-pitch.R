#' @keywords internal
#' @export
to_MusicXML.Pitch <- function(x, ...) {
  if (!is.list(x)) return(MusicXML("rest"))

  contents <- list(
    MusicXML("step", x[["step"]]),
    MusicXML("octave", x[["octave"]])
  )

  alter <- x[["alter"]]

  if (alter != 0) {
    contents <- append(contents, list(MusicXML("alter", alter)), 1)
  }

  MusicXML("pitch", contents)
}
