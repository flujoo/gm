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


#' @keywords internal
#' @export
to_value.Duration <- function(x, ...) {
  sum(sapply(x, to_value_atomic_duration))
}


to_value_atomic_duration <- function(atomic) {
  v_base <- to_value_duration_base(atomic)
  tuplets <- atomic$tuplets

  if (length(tuplets) == 0) {
    v_base
  } else {
    v_base * prod(sapply(tuplets, to_value))
  }
}


to_value_duration_base <- function(base) {
  v_type <- duration_types$value[duration_types$name == base$type]
  v_dot <- to_value_dot(base$dot)
  v_type * v_dot
}


to_value_dot <- function(dot) {
  sum(2^(-(0:dot)))
}
