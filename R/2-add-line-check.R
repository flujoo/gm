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


#' Check Number Limit of Voices
#' @noRd
check_voice_limit <- function(as, lines, target, to) {
  if (as != "voice") return(invisible())

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
