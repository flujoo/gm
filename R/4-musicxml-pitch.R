#' @keywords internal
#' @export
to_MusicXML.Pitch <- function(x, measure_rest, ...) {
  if (measure_rest) {
    MusicXML("rest", attributes = list(measure = "yes"))

  } else if (!is.list(x)) {
    MusicXML("rest")

  } else {
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
}
