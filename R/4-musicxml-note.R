#' @keywords internal
#' @export
to_MusicXML.Note <- function(x, divisions, ...) {
  contents_notations <- list()
  contents_note <- list()
  attributes <- list()

  measure_rest <- x[["measure_rest"]]
  voice <- x[["voice"]]
  staff <- x[["staff"]]


  # <grace> ----------------------------------------------------

  if (x[["grace"]]) {
    musicxml_grace <- MusicXML("grace", attributes = list(slash = "yes"))
    contents_note <- c(contents_note, list(musicxml_grace))
  }


  # <chord> ----------------------------------------------------

  j <- x[["j"]]

  if (!is.na(j) && j > 1) {
    musicxml_chord <- MusicXML("chord")
    contents_note <- c(contents_note, list(musicxml_chord))
  }


  # <pitch> ----------------------------------------------------

  pitch <- to_Pitch(x[["pitch"]])
  musicxml_pitch <- to_MusicXML(pitch, measure_rest)
  contents_note <- c(contents_note, list(musicxml_pitch))


  # <duration> -------------------------------------------------

  musicxml_duration <- MusicXML("duration", divisions * x[["length"]])
  contents_note <- c(contents_note, list(musicxml_duration))


  # <tie> and <tied> -------------------------------------------

  for (type in c("start", "stop")) {
    if (x[[paste0("tie_", type)]]) {
      musicxml_tie <- MusicXML("tie", attributes = list(type = type))
      contents_note <- c(contents_note, list(musicxml_tie))

      musicxml_tied <- MusicXML("tied", attributes = list(type = type))
      contents_notations <- c(contents_notations, list(musicxml_tied))
    }
  }


  # <voice> ----------------------------------------------------

  voice <- (staff - 1) * 4 + voice
  musicxml_voice <- MusicXML("voice", voice)
  contents_note <- c(contents_note, list(musicxml_voice))


  # <type>, <dot>, <time-modification>, and <tuplet> -----------

  if (!measure_rest) {
    duration <- to_Duration(x[["duration"]])

    for (type in c("start", "stop")) {
      name <- paste0("tuplet_", type)
      duration[[name]] <- x[[name]]
    }

    . <- to_MusicXML(duration)
    contents_note <- c(contents_note, .[["duration"]])
    contents_notations <- c(contents_notations, .[["tuplet"]])
  }


  # <staff> ----------------------------------------------------

  musicxml_staff <- MusicXML("staff", staff)
  contents_note <- c(contents_note, list(musicxml_staff))


  # <notations> ------------------------------------------------

  if (length(contents_notations) != 0) {
    musicxml_notations <- MusicXML("notations", contents_notations)
    contents_note <- c(contents_note, list(musicxml_notations))
  }


  # dynamics ---------------------------------------------------

  velocity <- x[["velocity"]]

  if (!is.na(velocity)) {
    dynamics <- round(velocity / 90 * 100, 2)
    attributes[["dynamics"]] <- dynamics
  }


  # <note> -----------------------------------------------------

  musicxml_note <- MusicXML("note", contents_note, attributes)

  # Retain indices for inserting components
  for (index in c("line", "i", "j")) musicxml_note[[index]] <- x[[index]]

  musicxml_note
}


#' Insert Child Element of `<note>`
#'
#' See `to_url("elements/note")`.
#'
#' @param scope Can be `"first"`, `"last"`, or `"all"`. Indicates
#' which note to insert the child to.
#'
#' @noRd
insert_note_child <- function(object, score, scope = "all") {
  tags <- c(
    "type",
    "dot",
    "accidental",
    "time-modification",
    "stem",
    "notehead",
    "staff",
    "beam",
    "notations",
    "lyric"
  )

  locations <- locate_notes(score, object = object)

  locations <- switch(
    scope,
    first = locations[1],
    last = rev(locations)[1],
    all = locations
  )

  musicxml <- to_MusicXML(object)

  for (location in locations) {
    i <- location[1]
    j <- location[2]
    k <- location[3]

    children <- score$contents[[i]]$contents[[j]]$contents[[k]]$contents

    score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- append(
      children,
      list(musicxml),
      locate_element(musicxml[["tag"]], children, tags)
    )
  }

  score
}


#' Get Indices of Matched Notes in MusicXML
#' @param j In current context, it indicates the position in a chord.
#' @param object Where `line`, `i`, and `j` can be extracted.
#' @returns A list of triplets indicating part, measure, and note positions.
#' @noRd
locate_notes <- function(score, line, i, j = NULL, object = NULL) {
  if (!is.null(object)) {
    line <- object[["line"]]
    i <- object[["i"]]
    j <- object[["j"]]
  }

  locations <- list()
  parts <- score[["contents"]]
  is_found <- FALSE

  for (k in seq_along(parts)[-1]) {
    part <- parts[[k]]
    if (!line %in% part[["lines"]]) next
    measures <- part[["contents"]]

    for (l in seq_along(measures)) {
      notes <- measures[[l]][["contents"]]

      for (m in seq_along(notes)) {
        note <- notes[[m]]
        if (note[["tag"]] == "attributes") next

        note_i <- note[["i"]]
        note_j <- note[["j"]]

        is_matched <-
          note[["line"]] == line &&
          !is.na(note_i) && note_i == i &&
          if (is.null(j) || is.na(j)) TRUE else !is.na(note_j) && note_j == j

        if (is_matched) {
          locations <- c(locations, list(c(k, l, m)))
          is_found <- TRUE

        } else if (is_found) {
          # Matched notes should be adjacent
          break
        }
      }
    }
  }

  locations
}
