#' @keywords internal
#' @export
add.Tie <- function(object, music) {
  to <- object$to
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)

  line <- get_line_row(to, lines)
  i <- object$i
  j <- object$j

  check_i(i, line, notes)
  check_j(j, line, i, notes)
  check_i_rest(i, line, notes)

  tie <- normalize(object, line)
  music$ties <- update_ties(music$ties, tie)
  music
}


#' Check If Tie Is Added to Rest
#' @noRd
check_i_rest <- function(i, line, notes) {
  chord <- notes[notes$line == line & notes$i == i, ]
  pass <- nrow(chord) > 1 || !(is.na(chord$pitch) && is.na(chord$midi))
  if (pass) return(invisible())

  general <- "Can not add a tie to a rest."
  specifics <- sprintf("It is a rest at position %s.", i)
  erify::throw(general, specifics)
}


#' @keywords internal
#' @export
normalize.Tie <- function(object, line, ...) {
  tie <- list(line = line, i = object$i, j = object$j)
  class(tie) <- "Tie"
  tie
}


update_ties <- function(ties, tie) {
  if (is.null(ties)) {
    ties <- to_case(tie)
    return(ties)
  }

  if (is.na(tie$j)) {
    ties <- ties[!(ties$line == tie$line & ties$i == tie$i), ]
    ties <- rbind(ties, to_case(tie))
    return(ties)
  }

  cover <- ties[ties$line == tie$line & ties$i == tie$i & is.na(ties$j), ]

  if (nrow(cover) == 0) {
    rbind(ties, to_case(tie))
  } else {
    ties
  }
}
