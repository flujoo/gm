#' Create `Clef` Object
#'
#' Create a `Clef` object to represent a clef.
#'
#' See [Wikipedia](https://en.wikipedia.org/wiki/Clef) for more details.
#'
#' @param sign A single character, which can be `"G"`, `"F"` or `"C"`.
#' Case insensitive.
#'
#' @param line Optional. A single integer, which depends on `sign`:
#'
#' - `1` or `2`, if `sign` is `"G"`;
#' - an integer between `3` and `5`, if `sign` is `"F"`;
#' - an integer between `1` and `5`, if `sign` is `"C"`.
#'
#' @param octave Optional. A single integer, which can be `-1` or `1`.
#' `octave` can be specified only when
#'
#' - `sign` is `"G"` and `line` is `2`, or
#' - `sign` is `"F"` and `line` is `4`.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the clef.
#'
#' @param bar Optional. A positive integer, which indicates the number of
#' the measure where to add the clef. By default, the clef will be added at
#' the first measure.
#'
#' @param offset Optional. A non-negative number,
#' which indicates the clef's position in a measure. The default value is `0`.
#'
#' @return A list of class `Clef`.
#'
#' @seealso [gm::+.Music()] for adding a `Clef` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a bass clef
#' clef <- Clef("F")
#' clef
#'
#' # Add the clef to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C3", "D3")) + clef
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Clef <- function(
    sign,
    line = NULL,
    octave = NULL,
    to = NULL,
    bar = NULL,
    offset = NULL) {

  # Validation
  check_clef_sign(sign)
  check_clef_line(line, sign)
  check_clef_octave(octave, sign, line)
  check_to(to)
  if (!is.null(bar)) erify::check_n(bar)
  check_offset(offset)

  # Normalization
  sign <- toupper(sign)

  line <- if (!is.null(line)) {
    as.integer(line)
  } else {
    switch(sign, "G" = 2L, "F" = 4L, "C" = 3L)
  }

  octave <- if (!is.null(octave)) as.integer(octave) else NA_integer_
  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(offset)) offset <- as.double(offset)

  # Construction
  structure(
    list(
      to = to,
      bar = bar,
      offset = offset,
      sign = sign,
      line = line,
      octave = octave
    ),

    class = "Clef"
  )
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
  to <- x$to
  bar <- x$bar
  offset <- x$offset

  cat(to_string(x), "\n")
  if (!(is.null(to) && is.null(bar) && is.null(offset))) cat("\n")
  print_to_i_j(to, scope = "staff")
  print_bar_offset(bar, offset)
}
