#' Create `Line` Object
#'
#' Create a `Line` object to represent a musical line. In gm,
#' the musical line is the basic unit of music. It appears in different
#' forms, such as voices, staffs, and parts in music scores.
#'
#' @param pitches A list or vector which represents the pitches
#' of a musical line. The items of `pitches` can be
#'
#' - single characters like `"C4"`, which represent pitch notations,
#' - single integers between 12 and 127, which represent MIDI note numbers,
#' - single `NA`s, which represent rests, and
#' - vectors of pitch notations and MIDI note numbers, which represent chords.
#'
#' If not provided, the default value is `NA`. If `pitches` and `durations`
#' are not of the same length, the shorter one will be recycled.
#' `pitches` and `durations` can not both be empty.
#'
#' @param durations A list or vector which represents the
#' durations of a musical line. The items of `durations` can be
#'
#' - single numbers, which represent note lengths, and
#' - single characters like `"quarter"`, which represent duration notations.
#'
#' If not provided, the default value is 1.
#'
#' @param tie Deprecated. Was used to add ties to notes. Please use
#' [gm::Tie()] instead.
#'
#' @param name Optional. A single character which represents the name of
#' the musical line. When adding components to a musical line,
#' it can be referred to by its name.
#'
#' @param as Optional. A single character which can be `"part"`, `"staff"`,
#' `"voice"`, and `"segment"`. It specifies how the musical line appears in
#' the music score. The default value is `"part"`.
#'
#' @param to Optional. A single character or integer, which represents the
#' name or row number of a reference musical line to which to add the
#' current musical line. By default, the musical line will be added at the
#' end of the score.
#'
#' @param after Optional. A single logical which indicates whether to add the
#' musical line after or before the reference musical line. The default value
#' is `TRUE`.
#'
#' @param bar Optional. A positive integer, which indicates the number of
#' the measure where to add the musical line. By default, the musical line
#' will be added at the first measure.
#'
#' @param offset Optional. A non-negative number,
#' which indicates the position in a measure where to add the musical line.
#' The default value is `0`.
#'
#' @return A list of class `Line`.
#'
#' @seealso [gm::+.Music()] for adding a musical line to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a musical line
#' line <- Line(c("C4", "D4", "E4"))
#' line
#'
#' # Add it to a music
#' music <- Music() + line
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Line <- function(
    pitches = NULL,
    durations = NULL,
    tie = NULL,
    name = NULL,
    as = NULL,
    to = NULL,
    after = NULL,
    bar = NULL,
    offset = NULL) {

  # Validation
  check_pitches(pitches)
  check_durations(durations)
  check_pitches_durations(pitches, durations)
  deprecate_tie(tie)
  if (!is.null(name)) erify::check_string(name)

  if (!is.null(as)) {
    erify::check_content(as, c("part", "staff", "voice", "segment"))
  }

  check_to(to)
  if (!is.null(after)) erify::check_bool(after)
  if (!is.null(bar)) erify::check_n(bar)
  check_offset(offset)

  # Normalization
  notes <- normalize_notes(pitches, durations)
  if (is.null(to)) to <- NA_integer_
  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(offset)) offset <- as.double(offset)

  # Construction
  structure(
    list(
      notes = notes,
      name = name,
      as = as,
      to = to,
      after = after,
      bar = bar,
      offset = offset
    ),

    class = "Line"
  )
}


#' @export
print.Line <- function(x, ...) {
  name <- x$name
  as <- x$as
  to <- x$to
  after <- x$after
  bar <- x$bar
  offset <- x$offset

  cat("Line", "\n\n")
  cat("* of notes:", "\n\n")
  print(x$notes)

  if (!is.null(c(name, as, after, bar, offset)) || !is.na(to)) cat("\n")

  if (!is.null(name)) cat(sprintf('* of name "%s"', name), "\n")
  if (!is.null(as)) cat(sprintf("* as a %s", as), "\n")

  s_after <- if (isFALSE(after)) "before" else "after"

  if (is.character(to)) {
    s_to <- '* to be inserted %s Line "%s"'
    cat(sprintf(s_to, s_after, to), "\n")

  } else if (is.numeric(to) && !is.na(to)) {
    s_to <- "* to be inserted %s Line %s"
    cat(sprintf(s_to, s_after, to), "\n")

  } else if (!is.null(after)) {
    s_to <- "* to be inserted %s the last Line in the score"
    cat(sprintf(s_to, s_after), "\n")
  }

  print_bar_offset(bar, offset)
}
