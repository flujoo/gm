#' @keywords internal
#' @export
to_MusicXML.Instrument <- function(x, ...) {
  id <- sprintf("P%s-I1", x[["part"]])
  attributes <- list(id = id)

  musicxml_score_instrument <- MusicXML(
    "score-instrument",
    MusicXML("instrument-name", ""),
    attributes
  )

  musicxml_midi_instrument <- MusicXML(
    "midi-instrument",

    list(
      MusicXML("midi-program", x[["midi"]]),
      MusicXML("volume", x[["volume"]]),
      MusicXML("pan", x[["pan"]])
    ),

    attributes
  )

  list(
    musicxml_score_instrument,
    musicxml_midi_instrument
  )
}


#' @keywords internal
#' @export
insert.Instrument <- function(x, to, ...) {
  part <- x[["part"]]

  to$contents[[1]]$contents[[part]]$contents <- append(
    to$contents[[1]]$contents[[part]]$contents,
    to_MusicXML(x)
  )

  to
}
