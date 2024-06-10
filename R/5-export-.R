#' @rdname export.Music
#' @export
export <- function(x, ...) {
  UseMethod("export")
}


musescore_extensions <- c(
  "flac",
  "metajson",
  "mid",
  "midi",
  "mlog",
  "mp3",
  "mpos",
  "mscx",
  "mscz",
  "musicxml",
  "mxl",
  "ogg",
  "pdf",
  "png",
  "spos",
  "svg",
  "wav",
  "xml"
)
