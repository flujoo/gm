#' @keywords internal
#' @export
to_MusicXML.Note <- function(x, divisions, ...) {
  notations <- list()
  contents <- list()

  if (x[["grace"]]) {
    contents <- c(contents, list(
      MusicXML("grace", attributes = list(slash = "yes"))
    ))
  }

  j <- x[["j"]]
  if (!is.na(j) && j > 1) contents <- c(contents, list(MusicXML("chord")))

  contents <- c(contents, list(to_MusicXML(to_Pitch(x[["pitch"]]))))

  duration <- to_Duration(x[["duration"]])

  contents <- c(contents, list(
    MusicXML("duration", divisions * to_value(duration))
  ))

  for (type in c("start", "stop")) {
    if (x[[paste0("tie_", type)]]) {
      contents <- c(contents, list(
        MusicXML("tie", attributes = list(type = type))
      ))

      notations <- c(notations, list(
        MusicXML("tied", attributes = list(type = type))
      ))
    }
  }

  contents <- c(contents, list(MusicXML("voice", x[["voice"]])))

  for (type in c("start", "stop")) {
    name <- paste0("tuplet_", type)
    duration[[name]] <- x[[name]]
  }

  . <- to_MusicXML(duration)
  contents <- c(contents, .[["duration"]])
  notations <- c(notations, .[["tuplet"]])

  contents <- c(contents, list(MusicXML("staff", x[["staff"]])))
  contents <- c(contents, list(MusicXML("notations", notations)))
  musicxml <- MusicXML("note", contents)

  # Retain indices for inserting components
  for (name in c("line", "i", "j")) musicxml[[name]] <- x[[name]]

  musicxml
}
