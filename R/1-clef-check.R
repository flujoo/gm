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
      "can `octave` be set."
    )
    specifics <- sprintf(
      '`sign` is `"%s"`, and `line` is `%s`.',
      sign, line
    )
    erify::throw(general, specifics)
  }
}
