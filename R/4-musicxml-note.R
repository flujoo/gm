#' @keywords internal
#' @export
to_MusicXML.Note <- function(x, divisions, ...) {
  contents_notations <- list()
  contents_note <- list()

  measure_rest <- x[["measure_rest"]]


  # <grace> ----------------------------------------------------

  if (x[["grace"]]) {
    musicxml_grace <- MusicXML("grace", attributes = list(slash = "yes"))
    contents_note <- c(contents_note, list(musicxml_grace))
  }


  # <chord> ----------------------------------------------------

  j <- x[["j"]]

  if (!is.na(j) && j > 1) {
    musicxml_chord <- MusicXML("chord")
    contents_note <- c(contents_note, list(musicxml_chord))
  }


  # <pitch> ----------------------------------------------------

  pitch <- to_Pitch(x[["pitch"]])
  musicxml_pitch <- to_MusicXML(pitch, measure_rest)
  contents_note <- c(contents_note, list(musicxml_pitch))


  # <duration> -------------------------------------------------

  musicxml_duration <- MusicXML("duration", divisions * x[["length"]])
  contents_note <- c(contents_note, list(musicxml_duration))


  # <tie> and <tied> -------------------------------------------

  for (type in c("start", "stop")) {
    if (x[[paste0("tie_", type)]]) {
      musicxml_tie <- MusicXML("tie", attributes = list(type = type))
      contents_note <- c(contents_note, list(musicxml_tie))

      musicxml_tied <- MusicXML("tied", attributes = list(type = type))
      contents_notations <- c(contents_notations, list(musicxml_tied))
    }
  }


  # <voice> ----------------------------------------------------

  musicxml_voice <- MusicXML("voice", x[["voice"]])
  contents_note <- c(contents_note, list(musicxml_voice))


  # <type>, <dot>, <time-modification>, and <tuplet> -----------

  if (!measure_rest) {
    duration <- to_Duration(x[["duration"]])

    for (type in c("start", "stop")) {
      name <- paste0("tuplet_", type)
      duration[[name]] <- x[[name]]
    }

    . <- to_MusicXML(duration)
    contents_note <- c(contents_note, .[["duration"]])
    contents_notations <- c(contents_notations, .[["tuplet"]])
  }


  # <staff> ----------------------------------------------------

  musicxml_staff <- MusicXML("staff", x[["staff"]])
  contents_note <- c(contents_note, list(musicxml_staff))


  # <notations> ------------------------------------------------

  if (length(contents_notations) != 0) {
    musicxml_notations <- MusicXML("notations", contents_notations)
    contents_note <- c(contents_note, list(musicxml_notations))
  }


  # <note> -----------------------------------------------------

  musicxml_note <- MusicXML("note", contents_note)

  # Retain indices for inserting components
  for (index in c("line", "i", "j")) musicxml_note[[index]] <- x[[index]]

  musicxml_note
}
