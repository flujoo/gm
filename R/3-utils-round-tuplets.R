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
  # initialize tuplet group indicators
  music$notes$group <- 0L
  # temporarily undecided tuplets
  tuplets <- NULL
  # the row numbers of the undecided tuplets
  ks <- NULL
  # the number of the current Line
  line <- 0L

  notes <- music$notes
  graces <- music$graces

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    line_k <- note$line

    # check if the Line has changed
    if (line_k != line) {
      line <- line_k

      if (!is.null(tuplets)) {
        # the undecided tuplets can not form a complete group
        music$notes$group[ks] <- -1L

        tuplets <- NULL
        ks <- NULL
      }
    }

    # skip grace notes
    if (any(graces$line == line_k & graces$i == note$i)) next

    duration <- note$duration

    # deal with non-tuplets first
    if (!grepl("/", duration)) {
      if (!is.null(tuplets)) {
        # non-tuplets are trivially incompatible
        music$notes$group[ks] <- -2L

        tuplets <- NULL
        ks <- NULL
      }

      next
    }

    duration <- complete_tuplet(Duration(duration))
    last <- tuplets[[length(tuplets)]]

    # check the compatibility of the current tuplet
    if (is_compatible(duration, last)) {
      tuplets <- c(tuplets, list(duration))
      ks <- c(ks, k)

    } else {
      music$notes$group[ks] <- -2L
      tuplets <- list(duration)
      ks <- k
    }

    # try to reduce the undecided tuplets
    reduced <- reduce_tuplets(tuplets)

    if (is.list(reduced)) {
      tuplets <- reduced

    } else {
      # the undecided tuplets can not be reduced
      if (isFALSE(reduced)) music$notes$group[ks] <- -3L

      tuplets <- NULL
      ks <- NULL
    }
  }

  # all notes are processed,
  # but there are still some undecided tuplets
  if (!is.null(tuplets)) music$notes$group[ks] <- -4L

  music
}


