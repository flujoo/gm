#' Export `Music` Object
#'
#' Export a `Music` object to a file format such as PNG or MP3.
#'
#' Supported file extensions:
#' `r document_items(musescore_extensions)`
#'
#' @param x A `Music` object.
#'
#' @param path A single character, which specifies the output file path.
#' For example, `"my/music/x.mp3"`. See the *Details* section for
#' supported file extensions.
#'
#' @param musescore Optional. A character vector, which represents
#' the command line options passed to MuseScore. See
#' [MuseScore command line usage](
#' https://musescore.org/en/handbook/4/command-line-usage) for details.
#'
#' @param ... Optional arguments to `export()` methods. Should be
#' ignored by the user.
#'
#' @returns An invisible `NULL`. A file is generated in the specified path.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   music <- Music() + Meter(4, 4) + Line("C4")
#'   export(music, tempfile(fileext = ".mp3"), "-r 200 -b 520")
#' }
export.Music <- function(x, path, musescore = NULL, ...) {
  check_export_path(path)
  if (!is.null(musescore)) erify::check_type(musescore, "character")

  set_musescore_path()
  music <- prepare(x)
  musicxml <- to_MusicXML(music)
  export(musicxml, path, musescore = musescore)
}


check_export_path <- function(path) {
  erify::check_string(path)

  dir_path <- dirname(path)

  if (!dir.exists(dir_path)) {
    erify::throw(
      "Folder specified by `path` must exist.",
      sprintf('"%s" does not exist.', dir_path)
    )
  }

  file_name_extension <- strsplit(basename(path), "[.]")[[1]]

  if (length(file_name_extension) == 1) {
    erify::throw(
      "File format must be specified by file extension in `path`.",
      sprintf("`path` does not contain a file extension.")
    )
  }

  extension <- rev(file_name_extension)[1]

  if (!extension %in% musescore_extensions) {
    erify::throw(
      sprintf(
        "File extension in `path` must be %s.",
        erify::join(erify::back_quote(musescore_extensions))
      ),

      sprintf('The file extension is `"%s"`.', extension)
    )
  }
}
