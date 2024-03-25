to_MusicXML_non_tuplet <- function(duration) {
  musicxml <- list(MusicXML("type", duration[["type"]]))

  dot <- duration[["dot"]]
  if (dot != 0) musicxml <- c(musicxml, rep(list(MusicXML("dot")), dot))

  musicxml
}
