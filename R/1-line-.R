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
  check_to(to)
  if (!is.null(after)) erify::check_bool(after)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(offset)) erify::check_positive(offset, zero = TRUE)

  # normalization
  notes <- normalize_notes(pitches, durations)

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
