#' @keywords internal
#' @export
to_MusicXML.Tempo <- function(x, divisions, ...) {
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

  musicxml_direction <- MusicXML("direction", contents)
  offset <- x[["offset"]]

  if (offset == 0) {
    # For convenience
    list(musicxml_direction)

  } else {
    duration <- offset * divisions

    list(
      to_MusicXML_forward(duration),
      musicxml_direction,
      to_MusicXML_backup(duration)
    )
  }
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
    pattern_base, "\\s*", "=", "\\s*", ".*",
    "\\s*", "\\)?"
  )

  if (!grepl(pattern_metronome, marking)) {
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


to_MusicXML_metronome <- function(left, right, bracket) {
  musicxml_left <- c(
    list(MusicXML("beat-unit", left[["type"]])),
    rep(list(MusicXML("beat-unit-dot")), left[["dot"]])
  )

  musicxml_right <- if (inherits(right, "Duration")) {
    c(
      list(MusicXML("beat-unit", right[["type"]])),
      rep(list(MusicXML("beat-unit-dot")), right[["dot"]])
    )

  } else {
    list(MusicXML("per-minute", right))
  }

  contents <- c(musicxml_left, musicxml_right)
  bracket <- if (bracket) "yes" else "no"

  MusicXML(
    "direction-type",
    MusicXML("metronome", contents, list(parentheses = bracket))
  )
}


#' @keywords internal
#' @export
insert.Tempo <- function(x, to, divisions, ...) {
  bar <- x[["bar"]]
  measures <- to$contents[[2]]$contents
  if (bar > length(measures)) return(to)
  notes <- measures[[bar]]$contents

  to$contents[[2]]$contents[[bar]]$contents <- append(
    notes,
    to_MusicXML(x, divisions),
    locate_insertion("attributes", notes, "attributes")
  )

  to
}
