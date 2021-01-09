#' @export
show.Music <- function(x, to = NULL, width = NULL, ...) {
  # check arguments
  check_type(x, "Music", method = "class")
  check_show_to(to)
  check_show_width(width)
  # `x$lines` must not be empty
  check_show_lines(x$lines)
  # `x$meter_line` must have a Meter at bar 1
  check_show_meter_line(x$meter_line)
}



# check arguments in `show.Music` -----------------------------------------

check_show_to <- function(to) {
  # early return
  if (is.null(to)) {
    return()
  }

  # basic checking
  check_type(to, "character")
  check_length(to, 1:2)

  # check content
  valid <- c("score", "audio")
  general <- '`to` must be "score", "audio" or both, if specified.'
  specifics <- character(0)
  l <- length(to)

  # the wording is more nuanced, don't merge this clause
  if (l == 1) {
    check_content(to, valid, general = general)

  } else {
    for (i in 1:l) {
      to_i <- to[[i]]
      if (!(to_i %in% valid)) {
        specifics[length(specifics) + 1] <-
          '`to[{i}]` is "{to_i}."' %>%
          glue::glue() %>%
          unclass()
      }
    }

    show_errors(general, specifics)
  }
}


check_show_width <- function(width) {
  if (!is.null(width)) {
    check_type(width, c("integer", "double"))
    check_length(width, 1)

    general <- '`width` must be a positive number, if specified.'
    check_content(width, expression(!is.na(x) && x > 0), general = general)
  }
}


check_show_lines <- function(lines) {
  if (is.null(lines)) {
    general <- "`x` must contain some Line."

    specifics <- c(
      "`x` contains no Line.",
      "Use `+ Line()` to add a Line."
    )

    show_errors(general, specifics)
  }
}


check_show_meter_line <- function(meter_line) {
  general <- "`x` must have a Meter at bar 1."
  specifics <- character(0)

  if (is.null(meter_line)) {
    specifics <- "`x` contains no Meter."
  } else if (meter_line$meters[[1]]$bar != 1) {
    specifics <- "`x` has no Meter at bar 1."
  }

  if (length(specifics) != 0) {
    specifics %<>% c("Use `+ Meter()` to add a Meter.")
    show_errors(general, specifics)
  }
}
