#' @keywords internal
#' @export
signify.Tupler <- function(x, type = NULL, dot = NULL, ...) {
  # convert `x$n` to string
  n <- x$n
  s_n <- paste("/", x$n)

  # convert `x$unit` to string
  unit <- x$unit
  if (!is.null(unit)) {
    unit_type <- unit$type
    unit_dot <- unit$dot
    s_unit <- paste0(unit_type, strrep(".", unit_dot))
  }

  # convert `x$take` to string
  take <- x$take
  if (!is.null(take)) {
    take_type <- take$type
    take_dot <- take$dot
    s_take <- paste0(take_type, strrep(".", take_dot))
  }

  # convert `x`
  if (is.null(unit)) {
    if (is.null(take)) {
      s <- s_n

    } else {
      s <- glue::glue("{s_n} * ({s_take} / _)") %>% unclass()
    }

  } else {
    con <-
      !is.null(type) &&
      !is.null(dot) &&
      identical(unit, take) &&
      unit_type == divide_duration_type(type, n) &&
      unit_dot == dot

    if (con) {
      s <- s_n

    } else {
      s <- glue::glue("{s_n} * ({s_take} / {s_unit})") %>% unclass()
    }
  }

  s
}


signify_tuplers <- function(tuplers, type, dot) {
  ss <- character(0)

  for (t in tuplers) {
    ss %<>% c(signify(t, type, dot))

    # re-assign `type` and `dot`
    take <- t$take
    type <- take$type
    dot <- take$dot
  }

  paste(ss, collapse = " ")
}


#' @keywords internal
#' @export
signify.Duration <- function(x, ...) {
  type <- x$type
  dot <- x$dot
  tuplers <- x$tuplers

  s <- paste0(type, strrep(".", dot))

  if (length(tuplers) > 0) {
    s %<>% paste(signify_tuplers(tuplers, type, dot))
  }

  s
}
