check_over_bar_tuplet_groups <- function(notes) {
  general <- "Tuplet groups in `music` must not cross bars."
  specific <- "The tuplet group at position %s of Line %s crosses bars."
  specifics <- character()

  groups <- notes[["group"]]

  for (group in unique(groups)) {
    if (group == 0) next

    # Tuplet group start bar and end bar should be the same
    tuplets <- notes[groups == group, ]
    bar <- unique(c(tuplets[["start_bar"]], tuplets[["end_bar"]]))
    if (length(bar) == 1) next

    specifics <- c(
      specifics,
      sprintf(specific, tuplets[["i"]][1], tuplets[["line"]][1])
    )
  }

  erify::throw(general, specifics)
}


#' Check If Tuplets Form Groups
#'
#' @details This can not be done in `Line()`, because there can be
#' non-tuplet grace notes between tuplets, but at the `Line()` stage,
#' the grace notes have not been indicated. These grace notes would make the
#' tuplet groups incomplete.
#'
#' @noRd
check_tuplet_groups <- function(notes) {
  general <- "Tuplets in `music` must form complete groups."
  specifics <- character()

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    if (note[["group"]] >= 0) next

    specific <- sprintf(
      "The tuplet at position %s of Line %s is not in a complete group.",
      note[["i"]], note[["line"]]
    )

    specifics <- c(specifics, specific)
  }

  erify::throw(general, specifics)
}
