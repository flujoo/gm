#' @export
Key <- function(key, bar = NULL, to = NULL, scope = NULL) {
  # validation
  erify::check_content(key, -7:7)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(to)) check_to(to)
  check_key_scope(scope, to)

  # normalization
  key <- as.integer(key)
  if (!is.null(bar)) bar <- as.integer(bar)
  scope <- normalize_key_scope(scope, to)

  # construction
  key <- list(
    to = to,
    scope = scope,
    bar = bar,
    key = key
  )
  class(key) <- "Key"
  key
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
  if (!is.null(to)) {
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
  cat("Key", to_string(x), "\n")

  bar <- x$bar
  to <- x$to
  scope <- x$scope

  if (!is.null(bar) || !is.null(to)) cat("\n")
  if (!is.null(bar)) cat(sprintf("* to be added at bar %s", bar), "\n")
  if (!is.null(to)) {
    s_to <- if (is.character(to)) paste0('"', to, '"') else to
    cat("* to be added only to the", scope, "containing Line", s_to, "\n")
  }
}
