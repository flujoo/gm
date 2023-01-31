#' Get Line's Row in `lines` of Music
#'
#' Get the row number of the Line that `to` refers to.
#'
#' @noRd
get_line_row <- function(to, lines) {
  if (is.null(to) || is.null(lines)) {
    NA_integer_
  } else if (is.numeric(to)) {
    as.integer(to)
  } else if (is.character(to)) {
    which(lines$name == to)
  }
}


#' @keywords internal
#' @export
locate <- function(object, ...) {
  UseMethod("locate")
}


#' Get Line's Location in Score
#'
#' Get a Line's location in a score. The Line is referred to by `to`.
#'
#' @noRd
locate_to <- function(to, lines) {
  if (is.numeric(to)) {
    line <- lines[to, ]

  } else if (is.character(to)) {
    line <- lines[!is.na(lines$name) & lines$name == to, ]

  } else {
    # get the last Line in the score
    # rather than the last case in `lines`
    lines <- lines[lines$part == max(lines$part), ]
    lines <- lines[lines$staff == max(lines$staff), ]
    lines <- lines[lines$voice == max(lines$voice), ]
    line <- lines[lines$segment == max(lines$segment), ]
  }

  line[, c("part", "staff", "voice", "segment")]
}
