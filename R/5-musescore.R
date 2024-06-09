#' Check and Store MuseScore Path
#'
#' Users can specify the MuseScore path in .Renviron file by adding
#' `MUSESCORE_PATH=/your/path/to/musescore`. This path will be
#' tested with some sample Music.
#'
#' If not provided or invalid, default paths will be checked. See
#' <https://musescore.org/en/handbook/4/revert-factory-settings>.
#'
#' The MuseScore path will be stored in the package options.
#'
#' @noRd
set_musescore_path <- function() {
  if (!is.null(getOption("gm.musescore_path"))) return(invisible())
  custom_path <- Sys.getenv("MUSESCORE_PATH")

  if (custom_path != "" && file.exists(custom_path)) {
    options(gm.musescore_path = custom_path)

    # Create a Music to test the custom path
    music <- Music() + Meter(1, 4) + Line(60)
    music <- prepare(music)
    musicxml <- to_MusicXML(music)
    export(musicxml, tempdir(), "test", "png")

    if (any(grepl("^test.png$", list.files(tempdir())))) {
      return(invisible())

    } else {
      options(gm.musescore_path = NULL)
    }
  }

  musescore_versions <- c(4, 3)

  default_paths <- switch(
    Sys.info()["sysname"],

    Darwin = sapply(musescore_versions, function(version) sprintf(
      "/Applications/MuseScore\ %s.app/Contents/MacOS/mscore",
      version
    )),

    Windows = sapply(musescore_versions, function(version) sprintf(
      "C:/Program Files/MuseScore %s/bin/MuseScore%s.exe",
      version, version
    )),

    Linux = c("mscore", "musescore", "mscore4portable")
  )

  for (default_path in default_paths) {
    if (file.exists(default_path)) {
      options(gm.musescore_path = default_path)
      return(invisible())
    }
  }

  erify::throw("MuseScore is not found.")
}


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
