#' Create `Key` Object
#'
#' Create a `Key` object to represent a key signature.
#'
#' @param key A single integer between -7 and 7, which indicates
#' the number of flat or sharp symbols in the key signature.
#'
#' @param bar Optional. A positive integer, which indicates the number of
#' the measure where to add the key signature. By default, the
#' key signature will be added at the first measure.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the key signature.
#' By default, the key signature will be added to the whole music
#' rather than some specific musical line.
#'
#' @param scope Optional. A single character of `"part"` or `"staff"`,
#' which indicates whether to add the key signature to a whole part or
#' only some staff of the part. Only when `to` is specified, can this
#' argument be specified. The default value is `"part"`.
#'
#' @return A list of class `Key`.
#'
#' @seealso [gm::+.Music()] for adding a key signature to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a G major
#' g <- Key(1, to = 1)
#' g
#'
#' # Add it only to some part of a `Music`
#' music <-
#'   Music() +
#'   Line(c("C4", "D4")) +
#'   Line("G3") +
#'   g
#'
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
#' @export
Key <- function(key, bar = NULL, to = NULL, scope = NULL) {
  # Validation
  erify::check_content(key, -7:7)
  if (!is.null(bar)) erify::check_n(bar)
  check_to(to)
  check_key_scope(scope, to)

  # Normalization
  key <- as.integer(key)
  if (!is.null(bar)) bar <- as.integer(bar)
  if (is.null(to)) to <- NA_integer_
  scope <- normalize_key_scope(scope, to)

  # Construction
  structure(
    list(to = to, scope = scope, bar = bar, key = key),
    class = "Key"
  )
}


check_key_scope <- function(scope, to) {
  if (is.null(scope)) return(invisible())

  if (is.null(to)) {
    general <- "Only when `to` is specified, can `scope` be set."
    specifics <- "`to` is `NULL`."
    erify::throw(general, specifics)

  } else {
    erify::check_content(scope, c("part", "staff"))
  }
}


normalize_key_scope <- function(scope, to) {
  if (!is.na(to)) {
    if (is.null(scope)) scope <- "part"
  } else {
    scope <- NA_character_
  }

  scope
}


#' @keywords internal
#' @export
to_string.Key <- function(x, short = FALSE, ...) {
  steps <- c("F", "C", "G", "D", "A", "E", "B")
  alters <- -2:2
  accidentals <- c("--", "-", "", "#", "##")
  i <- which(x$key == -7:7)

  major_step <- steps[i %% 7 + 1]
  major_accidental <- accidentals[alters == i %/% 7 - 1]
  major <- paste0(major_step, major_accidental)

  minor_step <- steps[(i + 3) %% 7 + 1]
  minor_accidental <- accidentals[alters == (i - 4) %/% 7]
  minor <- paste0(minor_step, minor_accidental)

  s <- if (short) "%s/%sm" else "%s Major (%s Minor)"
  sprintf(s, major, minor)
}


#' @export
print.Key <- function(x, ...) {
  bar <- x$bar
  to <- x$to

  cat("Key", to_string(x), "\n")
  if (!is.null(bar) || !is.na(to)) cat("\n")
  if (!is.null(bar)) cat(sprintf("* to be added at bar %s", bar), "\n")
  print_to_i_j(to, scope = x$scope)
}
