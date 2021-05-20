# check the durations of duration values and notations
#' @keywords internal
#' @export
check_duration_length <- function(duration, name, abort) {
  UseMethod("check_duration_length")
}


#' @keywords internal
#' @export
check_duration_length.numeric <- function(duration, name = NULL,
                                          abort = TRUE) {
  # `duration` must be a multiple of the duration of 1024th note
  pass <- duration %% quantify_duration_type("1024") == 0

  if (pass) {
    return(invisible())
  }

  # capture name
  if (is.null(name)) {
    name <- deparse(substitute(duration))
  }

  # get equivalent duration notation
  notation <- to_Duration(duration) %>% signify()

  specific <- glue::glue(
    '`{name}` is equivalent to `"{notation}"`, ',
    "in which the dot implies duration shorter than 1024th note."
  )

  if (!abort) {
    return(unclass(specific))
  }

  general <-
    "`{name}` must not be or imply duration shorter than 1024th note."

  erify::throw(general, specific, environment())
}


#' @keywords internal
#' @export
check_duration_length.character <- function(duration, name = NULL,
                                            abort = TRUE) {
  # parse `duration` and get values
  d <- parse_duration_notation(duration)
  v_type <- quantify_duration_type(d$type)
  v_dot <- quantify_dot(d$dot)
  v_ns <- prod(d$ns)
  v_total <- v_type * v_dot / v_ns
  v_1024 <-  quantify_duration_type("1024th")

  # capture name
  if (is.null(name)) {
    name <- deparse(substitute(duration))
  }

  general <-
    "`{name}` must not be or imply duration shorter than 1024th note."

  # check total length
  pass <- v_total >= v_1024

  if (!pass) {
    specific <- glue::glue(
      '`{name}` is `"{duration}"`, ',
      'which is shorter than 1024th note.'
    )

    if (abort) {
      erify::throw(general, specific, environment())
    } else {
      return(unclass(specific))
    }
  }

  # check non-tuplet part
  pass <- (v_type * v_dot) %% v_1024 == 0

  if (pass) {
    return(invisible())
  }

  specific <- glue::glue(
    '`{name}` is `"{duration}"`, ',
    'in which the dot implies duration shorter than 1024th note.'
  )

  if (abort) {
    erify::throw(general, specific, environment())
  } else {
    return(unclass(specific))
  }
}


check_duration_lengths <- function(durations) {
  if (is.list(durations)) {
    name <- "durations[[{i}]]"
  } else {
    name <- "durations[{i}]"
  }

  general <- paste(
    "Each item of `durations` must not be or imply duration",
    "shorter than 1024th note."
  )

  specifics <- character(0)

  for (i in seq_along(durations)) {
    d <- durations[[i]]

    # skip Durations
    if (inherits(d, "Duration")) {
      next
    }

    specific <- check_duration_length(d, name, FALSE) %>% glue::glue()
    specifics %<>% c(specific)
  }

  erify::throw(general, specifics, environment())
}
