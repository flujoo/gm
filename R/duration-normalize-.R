#' Normalize Duration Value or Notation to `Duration` Object
#'
#' @keywords internal
#' @export
Duration <- function(x, ...) {
  UseMethod("Duration")
}


#' @keywords internal
#' @export
to_string.Duration <- function(x, short = FALSE, ...) {
  s_atomics <- character()
  space <- if (short) "" else " "

  for (atomic in x) {
    s_base <- to_string_duration_base(atomic, short)
    tuplets <- atomic$tuplets

    if (length(tuplets) == 0) {
      s_atomic <- s_base
    } else {
      s_tuplets <- sapply(tuplets, to_string, short = short)
      s_atomic <- paste0(c(s_base, s_tuplets), collapse = space)
    }

    s_atomics <- c(s_atomics, s_atomic)
  }

  s_tie <- paste0(space, "-", space)
  paste(s_atomics, collapse = s_tie)
}


#' @keywords internal
#' @export
print.Duration <- function(x, ...) {
  cat(to_string(x), "\n")
}


to_string_duration_base <- function(base, short = FALSE) {
  type <- base$type
  if (short) type <- duration_types$abbr[duration_types$name == type]
  paste0(type, strrep(".", base$dot))
}
