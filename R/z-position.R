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


# check if `to` refers to a Line's name or row number in the Music
check_to_exist <- function(to, lines, class) {
  if (is.null(to)) {
    return(invisible())
  }

  l <- nrow(lines)

  if (is.null(l)) {
    l <- 0L
  }

  pass <- (is.character(to) && to %in% lines$name) ||
    # `to` has been checked in `check_to()` to be a positive integer
    (is.numeric(to) && to <= l)

  if (pass) {
    return(invisible())
  }

  general <- "`to` in `{class}()` must refer to a Line in the Music."

  if (is.character(to)) {
    specifics <- 'Can\'t find Line of name "{to}".'

  } else if (is.numeric(to)) {
    if (l == 0) {
      s_l <- "no Line"
    } else if (l == 1) {
      s_l <- "only 1 Line"
    } else {
      s_l <- glue::glue("only {l} Lines")
    }

    specifics <- c(
      'Can\'t find Line {to}.',
      i = "The Music contains {s_l}."
    )
  }

  erify::throw(general, specifics, environment())
}


# get the row number of the Line which `to` refers to in Music's `$lines`
locate_line <- function(to, lines) {
  if (is.null(to)) {
    NA_integer_
  } else if (is.numeric(to)) {
    as.integer(to)
  } else if (is.character(to)) {
    which(lines$name == to)
  }
}


signify_to <- function(to) {
  if (is.character(to)) {
    paste0('"', to, '"')
  } else if (is.numeric(to)) {
    as.character(to)
  }
}


normalize_bar <- function(bar) {
  ifelse(is.null(bar), 1L, as.integer(bar))
}
