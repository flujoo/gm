#' Normalize Tuplet Notation to `Tuplet` Object
#'
#' @keywords internal
#' @export
Tuplet <- function(tuplet_notation) {
  parts <- strsplit(tuplet_notation, "/|\\*|\\(|\\)|\\s")[[1]]
  parts <- parts[parts != ""]

  n = as.integer(parts[1])

  if (length(parts) == 3) {
    take <- parse_duration_base(parts[2])
    unit <- parse_duration_base(parts[3])
  } else {
    take <- NULL
    unit <- NULL
  }

  tuplet <- list(n = n, take = take, unit = unit)
  class(tuplet) <- "Tuplet"
  tuplet
}


#' @keywords internal
#' @export
to_string.Tuplet <- function(x, short = FALSE, ...) {
  take <- x$take
  is_complex <- !is.null(take)

  if (is_complex) {
    take <- to_string_duration_base(take, short)
    unit <- to_string_duration_base(x$unit, short)
  }

  space <- if (short) "" else " "
  n <- paste0("/", space, x$n)

  if (!is_complex) {
    n
  } else {
    paste0(n, space, "*", space, "(", take, space, "/", space, unit, ")")
  }
}


#' @keywords internal
#' @export
print.Tuplet <- function(x, ...) {
  cat(to_string(x), "\n")
}


#' @keywords internal
#' @export
to_value.Tuplet <- function(x, ...) {
  take <- x$take
  n <- x$n

  if (is.null(take)) {
    1/n
  } else {
    to_value_duration_base(take) / to_value_duration_base(x$unit) / n
  }
}
