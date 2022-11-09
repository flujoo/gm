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
  check_i_rest(object, line, notes)

  tie <- normalize(object, line)
  music$ties <- update_ties(music$ties, tie)
  music
}


#' @keywords internal
#' @export
normalize.Tie <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  object
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
