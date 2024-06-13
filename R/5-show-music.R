#' Show `Music` Object
#'
#' Display a `Music` object as a music score or audio file.
#'
#' This function works in
#'
#' - RStudio
#' - R Markdown files
#' - Jupyter Notebooks
#' - Shiny applications
#' - R.app GUI
#'
#' @param x A `Music` object.
#'
#' @param to Optional. A character vector, which can be `"score"`,
#' `"audio"`, or both. It specifies the output format. By default, both
#' are displayed. You can change the default behavior by setting the
#' `gm.show_to` option with `options()`.
#'
#' @param musescore Optional. A character vector, which represents
#' the command line options passed to MuseScore. See
#' [MuseScore command line usage](
#' https://musescore.org/en/handbook/4/command-line-options) for details.
#'
#' @returns An invisible `NULL`. A music score or audio file will be
#' displayed.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   music <- Music() + Meter(4, 4) + Line("C4")
#'   show(music, musescore = "-r 800 -T 5")
#' }
show.Music <- function(x, to = NULL, musescore = NULL) {
  check_show_to(to)
  to <- normalize_show_to(to)

  set_musescore_path()
  music <- prepare(x)
  musicxml <- to_MusicXML(music)
  show(musicxml, to, musescore = musescore)
}


normalize_show_to <- function(to) {
  if (is.null(to)) to <- getOption("gm.show_to")
  c(audio = "mp3", score = "png")[to]
}
