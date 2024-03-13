#' Append Accidental, Notehead, or Tie to Component of Music
#' @noRd
update_chordal <- function(cases, object, notes) {
  # For Ties, there are some extra steps
  is_tie <- inherits(object, "Tie")

  lines <- notes[["line"]]
  is <- notes[["i"]]
  js <- notes[["j"]]

  line <- object[["line"]]
  i <- object[["i"]]
  j <- object[["j"]]

  # Number of notes that are at the position `i`
  n <- NROW(notes[lines == line & is == i, ])

  # Normalize `j` to `NA`, if there is only one note
  if (n == 1 && !is.na(j)) object[["j"]] <- NA_integer_

  # Only one note is involved
  if (n == 1 || !is.na(j)) return(update_cases(cases, object))

  # All MIDI note numbers that are at the next position
  if (is_tie) midis <- notes[lines == line & is == i + 1, ][["midi"]]

  # If more than one note is involved, make their `j` explicit
  for (k in 1:n) {
    if (is_tie) {
      # Make sure there is an equivalent pitch at the next position
      midi <- notes[lines == line & is == i & js == k, ][["midi"]]
      if (!midi %in% midis) next

      # Remove the tied MIDI note number,
      # in case more than one note is tied to a same note.
      # There can be more than one equivalent MIDI,
      # so remember to remove only one instance.
      midis <- midis[- which(midis == midi)[1]]
    }

    object[["j"]] <- k
    cases <- update_cases(cases, object)
  }

  cases
}
