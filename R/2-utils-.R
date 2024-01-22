#' @description Get the row number of the Line that `to` refers to.
#' if `to` is `NULL`, the number of the last Line is returned.
#'
#' @noRd
normalize_to <- function(to, lines) {
  if (is.numeric(to)) {
    as.integer(to)

  } else if (is.character(to)) {
    which(lines$name == to)

  } else if (is.null(to)) {
    nrow(lines)
  }
}


#' @keywords internal
#' @export
locate <- function(object, ...) {
  UseMethod("locate")
}


#' @description Get a Line's location in a score.
#' The Line is referred to by `to`.
#'
#' @noRd
locate_to <- function(to, lines) {
  if (is.numeric(to) && !is.na(to)) {
    line <- lines[to, ]

  } else if (is.character(to)) {
    line <- lines[!is.na(lines$name) & lines$name == to, ]

  } else {
    # This is for adding a Line when `to` is unspecified
    # The current Line will be the next part
    # Get the last Line in the score rather than the last case in `lines`
    lines <- lines[lines$part == max(lines$part), ]
    lines <- lines[lines$staff == max(lines$staff), ]
    lines <- lines[lines$voice == max(lines$voice), ]
    line <- lines[lines$segment == max(lines$segment), ]
  }

  line[, c("part", "staff", "voice", "segment")]
}
