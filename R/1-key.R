#' @export
Key <- function(key, bar = NULL, to = NULL, scope = NULL) {
  # validation
  erify::check_content(key, -7:7)
  if (!is.null(bar)) erify::check_n(bar)
  if (!is.null(to)) check_to(to)
  check_key_scope(scope, to)

  # normalization
  if (!is.null(to) && is.null(scope)) scope <- "part"

  # construction
  key <- list(
    key = key,
    bar = bar,
    to = to,
    scope = scope
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

  # if to print each component
  print_bar <- !is.null(bar)
  print_to <- !is.null(to)

  if (print_bar || print_to) cat("\n")
  if (print_bar) cat(sprintf("* to be added at bar %s", bar), "\n")
  if (print_to) {
    s_to <- if (is.character(to)) paste0('"', to, '"') else to
    cat("* to be added only to the", scope, "containing Line", s_to, "\n")
  }
}
