#' @keywords internal
#' @export
to_MusicXML.Tempo <- function(x, ...) {
  tempo <- x[["tempo"]]
  contents <- list()

  parsed <- parse_tempo_marking(x[["marking"]], tempo)
  before <- parsed[["before"]]
  left <- parsed[["left"]]

  if (!is.null(before)) {
    musicxml_before <- MusicXML("direction-type", MusicXML("words", before))
    contents <- c(contents, list(musicxml_before))
  }

  if (!is.null(left)) {
    musicxml_metronome <- to_MusicXML_metronome(
      left, parsed[["right"]], parsed[["bracket"]]
    )

    contents <- c(contents, list(musicxml_metronome))
  }

  musicxml_sound <- MusicXML("sound", attributes = list(tempo = tempo))
  contents <- c(contents, list(musicxml_sound))

  MusicXML("direction", contents)
}


parse_tempo_marking <- function(marking, tempo) {
  if (is.na(marking)) {
    parsed <- list(
      before = NULL,
      left = to_Duration("quarter"),
      right = tempo,
      bracket = FALSE
    )

    return(parsed)
  }

  pattern_type <- paste(
    c(duration_types[["name"]], duration_types[["abbr"]]),
    collapse = "|"
  )

  pattern_base <- paste0("(", pattern_type, ")", "\\.{0,1}")

  pattern_metronome <- paste0(
    "\\(?", "\\s*",

    # The duration notation can only preceded by "(" or a whitespace
    # For example, "qq = 90" should not be allowed
    "(?<=(\\(|\\s))", pattern_base,

    "\\s*", "=", "\\s*", ".*",
    "\\s*", "\\)?"
  )

  if (!grepl(pattern_metronome, marking, perl = TRUE)) {
    parsed <- list(
      before = marking,
      left = NULL,
      right = NULL,
      bracket = FALSE
    )

    return(parsed)
  }

  . <- regexpr(pattern_metronome, marking, perl = TRUE)
  metronome <- regmatches(marking, .)
  before <- regmatches(marking, ., TRUE)[[1]][1]

  # Remove only the first whitespace on the right
  before <- if (before == "") NULL else gsub(" $", "", before)

  pattern_braket <- paste0("^\\(", "|", "\\)$")
  bracket <- grepl(pattern_braket, metronome)

  # Remove bracket(s)
  if (bracket) {
    . <- regmatches(metronome, gregexpr(pattern_braket, metronome), TRUE)[[1]]
    metronome <- .[. != ""]
  }

  . <- strsplit(metronome, "=")[[1]]
  left <- to_Duration(.[1])
  right <- .[2]

  right <- if (has_duration_notation_syntax(right)) {
    to_Duration(right)

  } else {
    # Remove only the first whitespace on the left
    gsub("^ ", "", right)
  }

  list(
    before = before,
    left = left,
    right = right,
    bracket = bracket
  )
}
