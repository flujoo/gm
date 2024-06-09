

#' Call MuseScore to Convert MusicXML File
#'
#' @param from,to Input and output file paths.
#' @param musescore MuseScore command line options.
#'
#' @noRd
convert_musicxml <- function(from, to, musescore = NULL) {
  system2(
    getOption("gm.musescore_path"),
    c(from, "-o", to, musescore),
    stderr = NULL,
    stdout = NULL
  )
}
