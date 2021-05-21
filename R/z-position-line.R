# `to` must refer to a Line's name or index
check_to <- function(to) {
  if (is.null(to)) {
    return(invisible())
  }

  general <- "`to` must be a single character or a single positive integer."

  if (is.character(to)) {
    erify::check_string(to, general = general)
  } else if (is.numeric(to)) {
    erify::check_n(to, general = general)
  } else {
    erify::check_type(to, c("character", "double", "integer"))
  }
}


check_to_exist <- function(object, music) {
  to <- object$to

  if (is.null(to)) {
    return(invisible())
  }

  lines <- music$lines
  l <- ifelse(is.null(lines), 0, nrow(lines))

  pass <- (is.character(to) && to %in% lines$name) ||
    (is.numeric(to) && to <= l)

  if (pass) {
    return(invisible())
  }

  general <- "`to` must refer to a Line in the Music."

  if (is.character(to)) {
    specific <- 'Can\'t find Line of name "{to}".'

  } else if (is.numeric(to)) {
    if (l == 0) {
      s_l <- "no Line"
    } else if (l == 1) {
      s_l <- "only 1 Line"
    } else {
      s_l <- glue::glue("only {l} Lines")
    }

    specific <- 'Can\'t find Line {to}, the Music contains {s_l}.'
  }

  erify::throw(general, specific, environment())
}
