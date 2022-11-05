#' @export
Clef <- function(sign,
                 line = NULL,
                 octave = NULL,
                 to = NULL,
                 bar = NULL,
                 offset = NULL) {
  # validation
  check_clef_sign(sign)
  check_clef_line(line, sign)
  check_clef_octave(octave, sign, line)
  if (!is.null(to)) check_to(to)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(offset)) erify::check_positive(offset, zero = TRUE)

  # normalization
  sign <- toupper(sign)

  line <- if (!is.null(line)) {
    as.integer(line)
  } else {
    switch(sign, "G" = 2L, "F" = 4L, "C" = 3L)
  }

  octave <- if (!is.null(octave)) as.integer(octave) else NA_integer_
  bar <- if (!is.null(bar)) as.integer(bar) else NA_integer_
  offset <- if (!is.null(offset)) as.double(offset) else NA_real_

  # construction
  clef <- list(
    sign = sign,
    line = line,
    octave = octave,
    to = to,
    bar = bar,
    offset = offset
  )
  class(clef) <- "Clef"
  clef
}


#' @keywords internal
#' @export
to_string.Clef <- function(x, ...) {
  sign <- x$sign
  line <- as.character(x$line)
  octave <- x$octave

  if (sign == "G") {
    s <- switch(line,
      "1" = "French Clef",
      "2" = "Treble Clef"
    )

  } else if (sign == "F") {
    s <- switch(line,
      "3" = "Baritone F-Clef",
      "4" = "Bass Clef",
      "5" = "Subbass Clef"
    )

  } else if (sign == "C") {
    s <- switch(line,
      "1" = "Soprano Clef",
      "2" = "Mezzo-Soprano Clef",
      "3" = "Alto Clef",
      "4" = "Tenor Clef",
      "5" = "Baritone C-Clef"
    )
  }

  if (!is.na(octave)) {
    s_octave <- if (octave == 1) "Octave Up" else "Octave Down"
    s <- paste(s_octave, s)
  }

  s
}


#' @export
print.Clef <- function(x, ...) {
  cat(to_string(x), "\n")

  to <- x$to
  bar <- x$bar
  offset <- x$offset

  if (!(is.null(to) && is.na(bar) && is.na(offset))) cat("\n")

  if (!is.null(to)) {
    s_to <- sprintf(
      "* to be added to the staff containing Line %s",
      if (is.character(to)) paste0('"', to, '"') else to
    )
    cat(s_to, "\n")
  }

  print_bar_offset(bar, offset)
}
