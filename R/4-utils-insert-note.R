insert_note_child <- function(object, score, scope) {
  musicxml <- to_MusicXML(object)

  # For Lyrics
  if (is.null(musicxml)) return(score)

  locations <- locate_notes(object, score, scope)

  for (location in locations) {
    i <- location[1]
    j <- location[2]
    k <- location[3]

    siblings <- score$contents[[i]]$contents[[j]]$contents[[k]]$contents

    score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- append(
      siblings,
      list(musicxml),
      locate_note_child(musicxml[["tag"]], siblings)
    )
  }

  score
}


#' @details See `to_url("elements/note")`.
#' @noRd
locate_note_child <- function(tag, siblings) {
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

  locate_insertion(tag, siblings, tags)
}


locate_notes <- function(object, score, scope) {
  locate_notes_by_index(
    object[["line"]],
    object[["i"]],
    object[["j"]],
    score,
    scope
  )
}


#' Get Indices of Matched Notes in MusicXML
#'
#' @param j In current context, it indicates the position in a chord.
#'
#' @param scope Can be `"first"`, `"last"`, or `"all"`. Indicates
#' which location(s) to return.
#'
#' @returns A list of triplets indicating part, measure, and note positions.
#'
#' @noRd
locate_notes_by_index <- function(line, i, j, score, scope) {
  locations <- list()
  parts <- score[["contents"]]

  for (k in seq_along(parts)[-1]) {
    part <- parts[[k]]
    if (!line %in% part[["lines"]]) next
    measures <- part[["contents"]]

    for (l in seq_along(measures)) {
      measure <- measures[[l]]
      if (!i %in% measure[["is"]]) next
      notes <- measure[["contents"]]

      for (m in seq_along(notes)) {
        note <- notes[[m]]
        if (note[["tag"]] != "note") next

        note_i <- note[["i"]]
        note_j <- note[["j"]]

        is_matched <-
          note[["line"]] == line &&
          !is.na(note_i) && note_i == i &&
          if (is.null(j) || is.na(j)) TRUE else !is.na(note_j) && note_j == j

        if (is_matched) {
          locations <- c(locations, list(c(k, l, m)))
          if (scope == "first") return(locations)
        }
      }
    }
  }

  if (scope == "last") rev(locations)[1] else locations
}
