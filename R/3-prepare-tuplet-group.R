#' Check If Tuplets Form Groups
#'
#' @details This can not be done in `Line()`, because there can be
#' non-tuplet grace notes between tuplets, but at the `Line()` stage,
#' the grace notes have not been indicated. These grace notes would make the
#' tuplet groups incomplete.
#'
#' @param music The output of `group_tuplets()`.
#'
#' @noRd
check_tuplet_groups <- function(music) {
  general <- "Tuplets in `music` must form complete groups."
  specifics <- character()
  notes <- music$notes

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    if (note$group >= 0) next

    specific <- sprintf(
      "The tuplet at position %s of Line %s is not in a complete group.",
      note$i, note$line
    )

    specifics <- c(specifics, specific)
  }

  erify::throw(general, specifics)
}


#' Indicate Tuplet Groups With Integers
#'
#' @description Tuplet groups are indicated with positive integers.
#' Tuplets in a same group are indicated with a same integer.
#'
#' Non-tuplets are indicated with 0 for convenience of operation.
#'
#' Tuplets that fail to form groups are indicated with negative integers:
#'
#' - -1: incomplete (type 1)
#' - -2: incompatible
#' - -3: overflowed
#' - -4: incomplete (type 2)
#'
#' This taxonomy is for testing.
#'
#' @noRd
group_tuplets <- function(music) {
  # Initialize tuplet group indicators
  music$notes$group <- 0L
  # Temporarily undecided tuplets
  tuplets <- NULL
  # Row numbers of the undecided tuplets
  ks <- NULL
  # Number of the current Line
  line <- 0L
  # Tuplet group number
  group <- 1L

  notes <- music$notes
  graces <- music$graces

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    line_k <- note$line

    # Check if the Line has changed
    if (line_k != line) {
      line <- line_k

      if (!is.null(tuplets)) {
        # Undecided tuplets can not form a complete group
        music$notes$group[ks] <- -1L

        tuplets <- NULL
        ks <- NULL
      }
    }

    # Skip grace notes
    if (any(graces$line == line_k & graces$i == note$i)) next

    duration <- note$duration

    # Deal with non-tuplets first
    if (!grepl("/", duration)) {
      if (!is.null(tuplets)) {
        # Non-tuplets are trivially incompatible
        music$notes$group[ks] <- -2L

        tuplets <- NULL
        ks <- NULL
      }

      next
    }

    duration <- complete_tuplet(Duration(duration))
    last <- tuplets[[length(tuplets)]]

    # Check the compatibility of the current tuplet
    if (is_compatible(duration, last)) {
      tuplets <- c(tuplets, list(duration))
      ks <- c(ks, k)

    } else {
      music$notes$group[ks] <- -2L
      tuplets <- list(duration)
      ks <- k
    }

    # Try to reduce the undecided tuplets
    reduced <- reduce_tuplets(tuplets)

    if (is.list(reduced)) {
      tuplets <- reduced

    } else if (isFALSE(reduced)) {
      music$notes$group[ks[-length(ks)]] <- -3L
      tuplets <- list(duration)
      ks <- k

    } else if (is.null(reduced)) {
      music$notes$group[ks] <- group
      group <- group + 1L
      tuplets <- NULL
      ks <- NULL
    }
  }

  # All notes are processed,
  # but there are still some undecided tuplets.
  if (!is.null(tuplets)) music$notes$group[ks] <- -4L

  music
}


#' @description Check if the current tuplet is compatible with
#' the last tuplet.
#'
#' @noRd
is_compatible <- function(current, last) {
  if (is.null(last)) return(TRUE)

  depth_current <- length(current$ratios)
  depth_last <- length(last$ratios)

  # the undecided tuplets are stopped by the current tuplet
  # from forming a complete group
  if (depth_current < depth_last) return(FALSE)

  # for the convenience of comparison
  if (depth_current > depth_last) {
    current$ratios[(depth_last + 1):depth_current] <- NULL
  }

  current$ratios[[depth_last]]$take <- NULL
  last$ratios[[depth_last]]$take <- NULL

  identical(current, last)
}


#' @description If the tuplets at the deepest level form a group at that
#' level, reduce that level. Repeat this process until no tuplet is left.
#'
#' @returns `NULL`, `FALSE`, or a list.
#'
#' @noRd
reduce_tuplets <- function(tuplets) {
  repeat {
    depths <- sapply(tuplets, function(tuplet) length(tuplet$ratios))
    depth_max <- max(depths)

    # the tuplets has been reduced completely
    if (depth_max == 0) return(NULL)

    # the tuplets that reach the deepest level
    ks <- which(depths == depth_max)

    # the total ratio at the deepest level
    ratio <- sum(sapply(
      tuplets[ks],
      function(tuplet) to_value_tuplet_ratio(tuplet$ratios[[depth_max]])
    ))

    if (ratio > 1) return(FALSE)
    if (ratio < 1) return(tuplets)

    # reduce the deepest level
    tuplets[[ks[1]]]$ratios[[depth_max]] <- NULL
    tuplets[ks[-1]] <- NULL
  }
}
