#' @rdname tuplet
#' @export
Tupler <- function(n, unit = NULL, take = unit) {
  # check arguments
  erify::check_n(n)
  check_tupler_unit(unit)
  check_tupler_unit(take)
  check_tupler_take(take, unit, n)

  # normalize arguments
  n %<>% as.integer()
  unit %<>% normalize_tupler_unit()
  take %<>% normalize_tupler_unit()

  # create Tupler
  list(
    n = n,
    unit = unit,
    take = take
  ) %>% `class<-`("Tupler")
}


check_tupler_unit <- function(unit) {
  if (is.null(unit)) {
    return(invisible())
  }

  # capture name
  name <- deparse(substitute(unit))

  # check type and length
  erify::check_type(unit, c("character", "double", "integer"), name)
  erify::check_length(unit, 1, name)

  # check content
  general <-
    "`{name}` must be a non-tuplet duration notation or a duration value."

  specific <- character(0)

  if (is.character(unit)) {
    if (!is_duration_notation(unit)) {
      specific <- '`{name}` is `"{unit}"`, which is not a duration notation.'
    } else if (!is_duration_notation(unit, tupler = FALSE)) {
      specific <- '`{name}` is `"{unit}"`, which is a tuplet.'
    }
  } else if (is.numeric(unit)) {
    if (!is_duration_value(unit)) {
      specific <- '`{name}` is `{unit}`, which is not a duration value.'
    }
  }

  erify::throw(general, specific, environment())

  # check duration
  check_duration_length(unit)
}


# `take` must not be longer than `unit` * `n`
is_take_valid <- function(take, unit, n) {
  ds <- list(take = take, unit = unit)

  for (i in 1:2) {
    d <- ds[[i]]

    # convert duration notation and list to duration value
    if (is.character(d)) {
      d <- to_Duration(d)

    } else if (is.list(d)) {
      d <- Duration(d$type, d$dot)
    }

    ds[[i]] <- quantify(d)
  }

  # pass
  ds$take <= ds$unit * n
}


check_tupler_take <- function(take, unit, n) {
  pass <- is.null(take) || is.null(unit) || is_take_valid(take, unit, n)

  if (pass) {
    return(invisible())
  }

  take %<>% erify::back_quote()
  unit %<>% erify::back_quote()

  general <- "`take` must not be longer than `unit` times `n`."
  specific <- "{take} is longer than {unit} times `{n}`."

  erify::throw(general, specific, environment())
}


normalize_tupler_unit <- function(unit) {
  if (!is.null(unit)) {
    unit %>%
      to_Duration() %T>%
      # delete `$tuplers`
      {`<-`(.$tuplers, NULL)} %>%
      unclass()
  }
}
