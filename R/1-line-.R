#' @export
Line <- function(pitches = NULL,
                 durations = NULL,
                 tie = NULL,
                 name = NULL,
                 as = NULL,
                 to = NULL,
                 after = NULL,
                 bar = NULL,
                 offset = NULL) {
  # validation
  check_pitches(pitches)
  check_durations(durations)
  check_pitches_durations(pitches, durations)
  deprecate_tie(tie)
  if (!is.null(name)) erify::check_string(name)
  if (!is.null(as)) {
    erify::check_content(as, c("part", "staff", "voice", "segment"))
  }
  if (!is.null(to)) check_to(to)
  if (!is.null(after)) erify::check_bool(after)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(offset)) erify::check_positive(offset, zero = TRUE)

  # normalization
  notes <- normalize_notes(pitches, durations)
  if (!is.null(bar)) bar <- as.integer(bar)
  if (!is.null(offset)) offset <- as.double(offset)

  # construction
  line <- list(
    notes = notes,
    name = name,
    as = as,
    to = to,
    after = after,
    bar = bar,
    offset = offset
  )
  class(line) <- "Line"
  line
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

  if (!is.null(c(name, as, to, after, bar, offset))) cat("\n")

  if (!is.null(name)) cat(sprintf('* of name "%s"', name), "\n")
  if (!is.null(as)) cat(sprintf("* as a %s", as), "\n")

  s_after <- if (isFALSE(after)) "before" else "after"

  if (is.character(to)) {
    s_to <- '* to be inserted %s Line "%s"'
    cat(sprintf(s_to, s_after, to), "\n")

  } else if (is.numeric(to)) {
    s_to <- "* to be inserted %s Line %s"
    cat(sprintf(s_to, s_after, to), "\n")

  } else if (!is.null(after)) {
    s_to <- "* to be inserted %s the last Line in the score"
    cat(sprintf(s_to, s_after), "\n")
  }

  print_bar_offset(bar, offset)
}
