#' @description Assign the start positions of the notes where the grace
#' notes attached to the grace notes, for ease of further processing.
#'
#' @noRd
locate_graces <- function(notes) {
  start_bars <- notes[["start_bar"]]
  start_offsets <- notes[["start_offset"]]

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    if (!note[["grace"]]) next

    . <- start_bars[-(1:k)]
    notes[k, ][["start_bar"]] <- .[!is.na(.)][1]

    . <- start_offsets[-(1:k)]
    notes[k, ][["start_offset"]] <- .[!is.na(.)][1]
  }

  notes
}
