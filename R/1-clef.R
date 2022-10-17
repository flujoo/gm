#' @export
Clef <- function(sign,
                 line = NULL,
                 octave = NULL,
                 to = NULL,
                 bar = NULL,
                 offset = NULL) {
  # validation
  erify::check_content(sign, c("G", "F", "C", "g", "f", "c"))
  check_clef_line(line, sign)
  check_clef_octave(octave, sign, line)
  check_to(to)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(offset)) erify::check_positive(offset, zero = TRUE)

  # normalization
  sign <- toupper(sign)
  if (is.null(line)) line <- switch(sign, "G" = 2, "F" = 4, "C" = 3)

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


check_clef_line <- function(line, sign) {
  if (is.null(line)) return(invisible())

  valid <- switch(toupper(sign), "G" = 1:2, "F" = 3:5, "C" = 1:5)
  general <- sprintf(
    'When `sign` is `"%s"`, `line` must be %s.',
    sign, erify::join(erify::back_quote(valid))
  )
  erify::check_content(line, valid, NULL, general)
}


check_clef_octave <- function(octave, sign, line) {
  if (is.null(octave)) return(invisible())

  con <- (sign %in% c("g", "G") && (line == 2 || is.null(line))) ||
    (sign %in% c("f", "F") && (line == 4 || is.null(line)))

  if (con) {
    erify::check_content(octave, c(-1, 1))

  } else {
    general <- paste(
      'Only when `sign` is `"G"` and `line` is `2`,',
      'or `sign` is `"F"` and `line` is `4`,',
      'can `octave` be set.'
    )
    specifics <- sprintf(
      '`sign` is `"%s"`, and `line` is `%s`.',
      sign, line
    )
    erify::throw(general, specifics)
  }
}


#' @keywords internal
#' @export
to_string.Clef <- function(x, ...) {
  sign <- x$sign
  line <- as.character(x$line)
  octave <- x$octave

  if (sign == "G") {
    s <- switch(
      line,
      "1" = "French Clef",
      "2" = "Treble Clef"
    )

  } else if (sign == "F") {
    s <- switch(
      line,
      "3" = "Baritone F-Clef",
      "4" = "Bass Clef",
      "5" = "Subbass Clef"
    )

  } else if (sign == "C") {
    s <- switch(
      line,
      "1" = "Soprano Clef",
      "2" = "Mezzo-Soprano Clef",
      "3" = "Alto Clef",
      "4" = "Tenor Clef",
      "5" = "Baritone C-Clef"
    )
  }

  if (!is.null(octave)) {
    s_octave <- if (octave == 1) "Octave up" else "Octave Down"
    s <- paste(s_octave, s)
  }

  s
}
