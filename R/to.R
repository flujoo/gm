check_to <- function(to) {
  general <- "`to` must be a string or a positive integer."
  erify::check_type(to, c("character", "double", "integer"), NULL, general)
  valid <- expression(erify::is_string(x) || erify::is_n(x))
  erify::check_content(to, valid, NULL, general)
}


#' Check If Line Exists in Music
#'
#' Check if `to` refers to an existing Line in a Music.
#'
#' @noRd
check_to_exist <- function(object, lines) {
  to <- object$to

  if (is.null(to)) return(invisible())

  n_lines <- NROW(lines)

  if (is.character(to)) {
    if (to %in% lines$name) {
      return(invisible())
    } else {
      specifics <- sprintf('Can not find Line "%s".', to)
    }

  } else if (is.numeric(to)) {
    if (to <= n_lines) {
      return(invisible())
    } else {
      if (n_lines == 0) {
        s_l <- "no Line"
      } else if (n_lines == 1) {
        s_l <- "only one Line"
      } else {
        s_l <- sprintf("only %s Lines", n_lines)
      }

      specifics <- c(
        sprintf("Can not find Line %s.", to),
        i = sprintf("The Music contains %s.", s_l)
      )
    }
  }

  general <- sprintf(
    "`to` in `%s()` must refer to an existing Line in the Music.",
    class(object)
  )
  erify::throw(general, specifics)
}


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
