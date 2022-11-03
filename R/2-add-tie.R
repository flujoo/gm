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
