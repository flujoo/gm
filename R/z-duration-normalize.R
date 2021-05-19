# description -------------------------------------------------------------

# convert duration values and notations to Durations



# normalizer --------------------------------------------------------------

normalize_durations <- function(durations) {
  # add `durations` to list if is Duration
  if (inherits(durations, "Duration")) {
    return(list(durations))
  }

  lapply(durations, to_Duration)
}



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
length.Duration <- function(x) {
  1L
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
