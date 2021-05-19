#' @keywords internal
#' @export
signify.Tupler <- function(x, type = NULL, dot = NULL, short = FALSE, ...) {
  # convert `x$n` to string
  n <- x$n
  s_n <- paste0("/", ifelse(short, "", " "), x$n)

  # convert `x$unit` to string
  unit <- x$unit
  if (!is.null(unit)) {
    unit_type <- unit$type
    unit_dot <- unit$dot
    s_unit <- paste0(
      ifelse(short, abbreviate_duration_type(unit_type), unit_type),
      strrep(".", unit_dot)
    )
  }

  # convert `x$take` to string
  take <- x$take
  if (!is.null(take)) {
    take_type <- take$type
    take_dot <- take$dot
    s_take <- paste0(
      ifelse(short, abbreviate_duration_type(take_type), take_type),
      strrep(".", take_dot)
    )
  }

  # convert `x`
  if (is.null(unit)) {
    if (is.null(take)) {
      s <- s_n

    } else {
      s <- ifelse(short, "{s_n}*({s_take}/_)", "{s_n} * ({s_take} / _)") %>%
        glue::glue() %>%
        unclass()
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
      s <- ifelse(
        short,
        "{s_n}*({s_take}/{s_unit})",
        "{s_n} * ({s_take} / {s_unit})"
      ) %>% glue::glue() %>%
        unclass()
    }
  }

  s
}


signify_tuplers <- function(tuplers, type, dot, short) {
  ss <- character(0)

  for (t in tuplers) {
    ss %<>% c(signify(t, type, dot, short))

    # re-assign `type` and `dot`
    take <- t$take
    type <- take$type
    dot <- take$dot
  }

  paste(ss, collapse = ifelse(short, "", " "))
}


#' @keywords internal
#' @export
signify.Duration <- function(x, short = FALSE, ...) {
  type <- x$type
  dot <- x$dot
  tuplers <- x$tuplers

  s <- paste0(
    ifelse(short, abbreviate_duration_type(type), type),
    strrep(".", dot)
  )

  if (length(tuplers) > 0) {
    s %<>% paste0(
      ifelse(short, "", " "),
      signify_tuplers(tuplers, type, dot, short)
    )
  }

  s
}


abbreviate_duration_type <- function(duration_type) {
  dplyr::filter(
    duration_types,
    name == duration_type | abbr == duration_type
  )$abbr
}
