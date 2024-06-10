#' @param formats Extra formats not specified in `path`.
#' @keywords internal
#' @export
#' @noRd
export.MusicXML <- function(x, path, formats = NULL, musescore = NULL, ...) {
  dir_path <- dirname(path)

  file_name_extension <- strsplit(basename(path), "[.]")[[1]]
  n <- length(file_name_extension)
  file_name <- paste0(file_name_extension[-n], collapse = ".")
  formats <- c(formats, file_name_extension[n])

  # File path without the extension
  name_path <- file.path(dir_path, file_name)

  # Other formats are converted from a MusicXML file through MuseScore
  musicxml_path <- if ("musicxml" %in% formats) {
    paste0(name_path, ".musicxml")

  } else {
    tempfile(fileext = ".musicxml")
  }

  writeLines(to_string(x), musicxml_path)

  for (format in setdiff(formats, "musicxml")) {
    file_path <- paste0(name_path, ".", format)

    # Trim the image
    if (format %in% c("png", "svg")) musescore <- c("-T 0", musescore)

    convert_musicxml(musicxml_path, file_path, musescore)

    # MuseScore splits a long image and append "-1", "-2", ... to the file
    # names. If there is only one image, rename it back by removing "-1".
    if (format %in% c("png", "svg")) {
      pattern <- paste0("^", file_name, "-[0-9]+", ".", format, "$")
      ks <- grep(pattern, list.files(dir_path))

      if (length(ks) == 1) {
        file.rename(paste0(name_path, "-1.", format), file_path)
      }
    }
  }

  if (!"musicxml" %in% formats) unlink(musicxml_path)
}
