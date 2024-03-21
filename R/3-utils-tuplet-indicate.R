indicate_tuplets <- function(notes) {
  notes[["tuplet_start"]] <- list(NULL)
  notes[["tuplet_stop"]] <- list(NULL)

  groups <- notes[["group"]]

  for (group in unique(groups)) {
    if (group == 0) next

    # Temporarily undecided tuplets
    undecided <- list()

    tuplets <- lapply(
      notes[groups == group, ][["duration"]],
      to_Duration
    )

    for (k in seq_along(tuplets)) {
      tuplet <- tuplets[[k]]
      depth <- length(tuplet[["ratios"]])

      depths <- infer_depths(depth, undecided)
      notes[groups == group, ][["tuplet_start"]][k] <- depths

      undecided <- c(undecided, list(tuplet))
      undecided <- reduce_tuplets(undecided)

      depths <- infer_depths(depth, undecided)
      notes[groups == group, ][["tuplet_stop"]][k] <- depths
    }
  }

  notes
}


infer_depths <- function(depth, undecided) {
  n <- length(undecided)

  if (n == 0) {
    depths <- 1:depth

  } else {
    depth_last <- length(undecided[[n]][["ratios"]])
    depths <- if (depth > depth_last) (depth_last + 1):depth else NULL
  }

  # For ease of operation
  list(depths)
}
