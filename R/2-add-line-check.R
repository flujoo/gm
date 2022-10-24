#' Check If Line's Name Has Been Used
#' @noRd
check_line_name <- function(name, lines) {
  if (is.null(name) || !(name %in% lines$name)) return(invisible())

  general <- paste(
    "The name of the Line must not have been used by",
    "any Line in the Music."
  )
  specifics <- sprintf('Name "%s" has been used.', name)
  erify::throw(general, specifics)
}


#' Check If Line Exists in Music
#'
#' Check if `to` refers to an existing Line in a Music.
#'
#' @param class The class of the object to be added to the Music.
#'
#' @noRd
check_to_exist <- function(to, lines, class) {
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
    class
  )
  erify::throw(general, specifics)
}


#' Check Number Limit of Voices
#' @noRd
check_voice_limit <- function(lines, target, to) {
  filter <- lines$part == target$part & lines$staff == target$staff
  n_voices <- nrow(lines[filter, ])
  if (n_voices < 4) return(invisible())

  if (is.null(to)) {
    s_to <- "the Line is added to"
  } else if (is.character(to)) {
    s_to <- sprintf('containing Line "%s"', to)
  } else if (is.numeric(to)) {
    s_to <- sprintf("containing Line %s", to)
  }

  general <- "Any staff in a Music can contain at most four voices."
  specifics <- paste("The staff", s_to, "already has four voices.")
  erify::throw(general, specifics)
}
