#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  # validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_tie(object$i, object$j, line, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # construction
  music$ties <- update_ties(music$ties, object, notes)
  music
}


#' @details A special case of `update_chordal()`.
#' @noRd
update_ties <- function(ties, tie, notes) {
  lines <- notes[["line"]]
  is <- notes[["i"]]
  js <- notes[["j"]]

  line <- tie[["line"]]
  i <- tie[["i"]]

  # Number of notes that are at the position where the tie is added
  n <- NROW(notes[lines == line & is == i, ])

  # If more than one note is involved, make their `j` explicit
  ks <- if (n == 1) NA_integer_ else 1:n

  # All MIDI note numbers that are at the next position
  midis <- notes[lines == line & is == i + 1, ][["midi"]]

  for (j in ks) {
    if (!is.na(j)) {
      # Make sure there is an equivalent pitch at the next position
      midi <- notes[lines == line & is == i & js == j, ][["midi"]]
      if (!midi %in% midis) next

      # Remove the tied MIDI note number,
      # in case more than one note is tied to a same note.
      # There can be more than one equivalent MIDI,
      # so remember to remove only one instance.
      midis <- midis[- which(midis == midi)[1]]
    }

    tie[["j"]] <- j
    ties <- update_cases(ties, tie)
  }

  ties
}


#' @keywords internal
#' @export
locate.Tie <- function(object, ...) {
  c(object$line, object$i, object$j)
}
