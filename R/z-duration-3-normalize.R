# description -------------------------------------------------------------

# convert duration values and notations to Durations



# Duration ----------------------------------------------------------------

Duration <- function(type, dot, tuplers = list()) {
  list(
    type = type,
    dot = as.integer(dot),
    tuplers = tuplers
  ) %>% `class<-`("Duration")
}


#' @keywords internal
#' @export
to_Duration <- function(duration) {
  UseMethod("to_Duration")
}


#' @keywords internal
#' @export
to_Duration.default <- function(duration) {
  duration
}



# value -> Duration -------------------------------------------------------

#' @keywords internal
#' @export
to_Duration.numeric <- function(duration) {
  # locate `duration` in `duration_types`
  k <- which(duration_types[-1:-2] == duration, TRUE)

  type <- duration_types$name[k[1]]
  dot <- k[2] - 1

  Duration(type, dot, list())
}



# notation -> Duration ----------------------------------------------------

parse_duration_notation <- function(notation) {
  # parse type
  reg <-
    c(duration_types$name, duration_types$abbr) %>%
    paste(collapse = "|") %>%
    # must not extract digits following "/"
    paste0("(?<!/)(", ., ")")

  type <- stringr::str_extract(notation, reg)

  # convert abbreviation
  type <-
    which(duration_types == type, TRUE)[1] %>%
    duration_types$name[.]

  # parse dot
  dot <- stringr::str_extract(notation, "\\.{1,4}") %>% nchar()

  if (is.na(dot)) {
    dot <- 0L
  }

  # parse tuplers
  ns <-
    stringr::str_extract_all(notation, "/[0-9]*")[[1]] %>%
    sapply(function(x) x %>% substr(2, nchar(.))) %>%
    as.integer()

  list(type = type, dot = dot, ns = ns)
}


divide_duration_type <- function(duration_type, n) {
  floor(log2(n)) %>%
    {. + which(duration_types$name == duration_type)} %>%
    duration_types$name[.]
}


#' @keywords internal
#' @export
to_Duration.character <- function(duration) {
  d <- parse_duration_notation(duration)

  type <- d$type
  dot <- d$dot
  ns <- d$ns

  # convert `ns` to tuplers
  tuplers <- list()

  for (n in ns) {
    # `type` shoud be re-assigned every time
    type <- divide_duration_type(type, n)

    tuplers[[length(tuplers) + 1]] <-
      list(type = type, dot = dot) %>%
      {list(n = n, unit = ., take = .)} %>%
      `class<-`("Tupler")
  }

  Duration(d$type, dot, tuplers)
  # `type` is re-assigned, so use the original one
}



# Duration -> value -------------------------------------------------------

# get a duration type or its abbreviation's value
quantify_duration_type <- function(duration_type) {
  dplyr::filter(
    duration_types,
    name == duration_type | abbr == duration_type
  )$value
}


#' @keywords internal
#' @export
quantify.Tupler <- function(x, ...) {
  # unpack
  n <- x$n
  unit <- x$unit
  take <- x$take

  v_unit <- quantify_duration_type(unit$type) * quantify_dot(unit$dot)
  v_take <- quantify_duration_type(take$type) * quantify_dot(take$dot)
  (1 / n) * (v_take / v_unit)
}


quantify_tuplers <- function(tuplers) {
  if (identical(tuplers, list())) {
    return(1)
  }

  prod(quantify(tuplers))
}


#' @keywords internal
#' @export
quantify.Duration <- function(x, ...) {
  prod(
    quantify_duration_type(x$type),
    quantify_dot(x$dot),
    quantify_tuplers(x$tuplers)
  )
}



# Duration -> string ------------------------------------------------------

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
