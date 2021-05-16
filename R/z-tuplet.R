#' @export
tuplet <- function(duration, ...) {
  # check `duration`
  check_tuplet_duration(duration)

  # capture `...`
  tuplers <- list(...)

  # check `...`
  erify::check_classes(tuplers, "Tupler", "list(...)")

  # convert `duration` to Duration
  duration %<>% to_Duration()

  # normalize and check `tuplers`
  tuplers %<>% normalize_tuplet_tuplers(duration)

  # append it to `duration`
  duration$tuplers %<>% c(tuplers)

  # check duration of `duration`
  check_tuplet_out(duration)

  duration
}


check_tuplet_duration <- function(duration) {
  # skip Durations
  if (inherits(duration, "Duration")) {
    return(invisible())
  }

  general <- paste(
    "`duration` must be a duration notation, a duration value",
    "or a Duration object."
  )

  # check type and length
  erify::check_type(
    duration, c("character", "double", "integer"), general = general)

  erify::check_length(duration, 1, general = general)

  # check content
  specific <- character(0)

  if (is.character(duration) && !is_duration_notation(duration)) {
    specific <-
      '`duration` is `"{duration}"`, which is not a duration notation.'

  } else if (is.numeric(duration) && !is_duration_value(duration)) {
    specific <- '`duration` is `{duration}`, which is not a duration value.'
  }

  erify::throw(general, specific, environment())

  # check duration
  check_duration_length(duration)
}


normalize_tuplet_tuplers <- function(tuplers, duration) {
  if (length(tuplers) == 0) {
    return(tuplers)
  }

  # initialize `type` and `dot`
  ts <- duration$tuplers
  m <- length(ts)

  if (m == 0) {
    type <- duration$type
    dot <- duration$dot

  } else {
    . <- ts[[m]]$take
    type <- .$type
    dot <- .$dot
  }

  # normalize `tuplers`
  for (i in seq_along(tuplers)) {
    # unpack tupler
    tupler <- tuplers[[i]]
    n <- tupler$n
    unit <- tupler$unit
    take <- tupler$take

    # convert `unit` if is `NULL`
    if (is.null(unit)) {
      unit_type <- divide_duration_type(type, n)
      # check if `unit_type` is `NA`
      check_tuplet_unit(unit_type, environment())
      # update `unit` and `tupler`
      unit <- list(type = unit_type, dot = dot)
      tupler$unit <- unit

      # convert `take` if it's `NULL`
      if (is.null(take)) {
        take <- unit
        tupler$take <- take

      # check if `take` is valid
      } else {
        check_tuplet_take(take, unit, n, i)
      }

    # check `unit`
    } else {
      check_divisible(type, dot, unit, i)
    }

    # reset `type` and `dot`
    type <- take$type
    dot <- take$dot

    # re-assign `tuplers`
    tuplers[[i]] <- tupler
  }

  tuplers
}


# check if `$unit$type` shorter than 1024th note is generated
check_tuplet_unit <- function(unit_type, env) {
  if (!is.na(unit_type)) {
    return(invisible())
  }

  general <- paste(
    "Applying `...` to `duration` must not result in",
    "duration shorter than 1024th note."
  )

  specific <-
    "`list(...)[[{i}]]$unit$type` would be shorter than 1024th note."

  erify::throw(general, specific, env, class = "check_tuplet_unit")
}


signify_unit <- function(unit) {
  Duration(unit$type, unit$dot) %>%
    signify() %>%
    erify::back_quote()
}


# check if `$take` is longer than `$unit` times `$n`
check_tuplet_take <- function(take, unit, n, i) {
  pass <- is_take_valid(take, unit, n)

  if (pass) {
    return(invisible())
  }

  general <- paste(
    "In each Tupler in `...`,",
    "`$take` must not be longer than `$unit` times `$n`."
  )

  take %<>% signify_unit()
  unit %<>% signify_unit()

  specific <-
    "In `list(...)[[{i}]]`, {take} is longer than {unit} times `{n}`."

  erify::throw(general, specific, environment(), class = "check_tuplet_take")
}


# check if previous "take" is divisible by `unit`
check_divisible <- function(type, dot, unit, i) {
  unit_type <- unit$type
  unit_dot <- unit$dot
  i_type <- which(duration_types$name == type)
  i_unit_type <- which(duration_types$name == unit_type)

  # 2^i
  pass <- unit_dot == dot && i_unit_type >= i_type
  if (dot == 1) {
    # 3 * 2^i
    pass <- pass ||
      unit_dot == 0 && i_unit_type > i_type
  } else if (dot == 2) {
    # 7 * 2^i
    pass <- pass ||
      unit_dot == 0 && i_unit_type > (i_type + 1)
  } else if (dot == 3) {
    # 15 * 2^i
    pass <- pass ||
      unit_dot == 1 && i_unit_type > (i_type + 1)
  } else if (dot == 4) {
    # 31 * 2^i
    pass <- pass ||
      unit_dot == 0 && i_unit_type > (i_type + 3)
  }

  if (pass) {
    return(invisible())
  }

  general <- paste(
    "`$unit` in each Tupler in `...` must divide",
    "`$take` in the previous Tupler or `duration`."
  )

  unit %<>% signify_unit()
  take <- signify_unit(list(type = type, dot = dot))

  specific <- paste(
    "`list(...)[[{i}]]$unit` is {unit}, which can't divide {take}."
  )

  erify::throw(general, specific, environment(), class = "divisible")
}


# check if the output Duration of `tuplet()` is shorter than 1024th note
check_tuplet_out <- function(duration) {
  pass <- quantify(duration) >= quantify_duration_type("1024th")

  if (pass) {
    return(invisible())
  }

  general <- paste(
    "Applying `...` to `duration` must not result in",
    "duration shorter than 1024th note."
  )

  specific <- paste(
    "The output would be { signify(duration) },",
    "which is shorter than 1024th note."
  )

  erify::throw(general, specific, environment(), class = "check_tuplet_out")
}
