# duration types ----------------------------------------------------------

duration_types <- c(
  "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
  "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"
)


duration_type_abbrs <- c(
  "m", "l", "b", "w", "h", "q", "8", "16", "32", "64", "128", "256",
  "512", "1024"
)


# some notes:

# 1. don't change the item order of these two constants,
# since some functions depend on the current order

# 2. there is a correspondence between items of these two constants,
# so don't omit any item in `duration_type_abbrs`

# 3. duration value has to be numeric, since for example,
# "8" represents eighth note, but 8 is equivalent to two whole notes



# duration value predicate ------------------------------------------------

to_value.duration_type <- function(duration_type) {
  # quarter note
  v_q <- 1
  i_q <- which(duration_types == "quarter")

  # `duration_type` can come from either `duration_types`
  # or `duration_type_abbrs`
  i <- ifelse(
    duration_type %in% duration_types,
    which(duration_types == duration_type),
    which(duration_type_abbrs == duration_type)
  )

  v_q * 2^(i_q - i)
}


to_value.dot <- function(dot) {
  sum(2^(-(0:dot)))
}


to_values.duration_types <- function(dot = 0:4) {
  duration_types %>%
    sapply(to_value.duration_type) %>%
    sapply(function(value) sapply(dot, to_value.dot) * value) %>%
    sort(decreasing = TRUE)
}


duration_values <- to_values.duration_types()


is_duration_value <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  x %in% duration_values
}



# duration notation predicate ---------------------------------------------

is_duration_notation <- function(x, tupler = TRUE) {
  if (!is.character(x)) {
    return(FALSE)
  }

  reg <- paste0(
    "^",
    # a valid duration notation always starts with a duration type
    # or its abbreviation
    paste0(
      "(",
      paste(c(duration_types, duration_type_abbrs), collapse = "|"),
      ")"
    ),
    # maybe followed by a dot notation
    "(\\.{1,4})?",
    # maybe followed by some tupler notations
    ifelse(tupler, "(/([1-9][0-9]*))*", ""),
    "$"
  )

  grepl(reg, x)
}



# parse duration notation -------------------------------------------------

parse_duration_notation <- function(notation) {
  # parse type ------------------------------------------------------------
  reg <- c(duration_types, duration_type_abbrs) %>%
    paste(collapse = "|") %>%
    # must not extract digits following "/"
    paste0("(?<!/)(", ., ")")

  type <- stringr::str_extract(notation, reg)

  # abbr -> type
  if (type %in% duration_type_abbrs) {
    type %<>%
      {which(duration_type_abbrs == .)} %>%
      duration_types[.]
  }


  # parse dot -------------------------------------------------------------
  dot <- stringr::str_extract(notation, "\\.{1,4}") %>% nchar()

  if (is.na(dot)) {
    dot <- 0L
  }


  # parse tuplers ---------------------------------------------------------
  ns <-
    stringr::str_extract_all(notation, "/[0-9]*")[[1]] %>%
    sapply(function(x) x %>% substr(2, nchar(.))) %>%
    as.integer()


  # parse duration notation -----------------------------------------------
  list(type = type, dot = dot, ns = ns)
}



# Duration ----------------------------------------------------------------

Duration <- function(type, dot, tuplers = list()) {
  list(type = type, dot = dot, tuplers = tuplers) %>% `class<-`("Duration")
}


#' @keywords internal
#' @export
to_Duration <- function(duration) {
  UseMethod("to_Duration")
}


#' @keywords internal
#' @export
to_Duration.Duration <- function(duration) {
  duration
}



# Duration -> value -------------------------------------------------------

#' @keywords internal
#' @export
to_value.Tupler <- function(x, ...) {
  n <- x$n
  unit <- x$unit
  take <- x$take

  v_unit <- to_value.duration_type(unit$type) * to_value.dot(unit$dot)
  v_take <- to_value.duration_type(take$type) * to_value.dot(take$dot)
  (1 / n) * (v_take / v_unit)
}


to_value.tuplers <- function(tuplers) {
  if (identical(tuplers, list())) {
    return(1)
  }

  tuplers %>%
    sapply(to_value) %>%
    prod()
}


#' @keywords internal
#' @export
to_value.Duration <- function(x, ...) {
  prod(
    to_value.duration_type(x$type),
    to_value.dot(x$dot),
    to_value.tuplers(x$tuplers)
  )
}



# duration value -> Duration ----------------------------------------------

#' @keywords internal
#' @export
to_Duration.numeric <- function(duration) {
  i <- which(duration_values == duration)

  type <- ((i - 1) %/% 5 + 1) %>% duration_types[.]
  dot <- ((5 - (i %% 5)) %% 5) %>% as.integer()

  Duration(type, dot, list())
}



# duration notation -> Duration -------------------------------------------

#' @keywords internal
#' @export
to_Duration.character <- function(duration) {
  d <- parse_duration_notation(duration)
  type <- d$type
  dot <- d$dot

  # ns -> tuplers
  tuplers <- list()
  for (n in d$ns) {
    # `type` shoud be re-assigned every time
    type <- divide_duration_type(type, n)

    tuplers[[length(tuplers) + 1]] <-
      type %>%
      {list(type = ., dot = dot)} %>%
      {list(n = n, unit = ., take = .)} %>%
      `class<-`("Tupler")
  }

  Duration(d$type, dot, tuplers)
  # `type` is re-assigned, so use the original one
}


divide_duration_type <- function(duration_type, n) {
  n %>%
    log2() %>%
    floor() %>%
    {. + which(duration_types == duration_type)} %>%
    duration_types[.]
}



# duration ----------------------------------------------------------------

# some notes:

# 1. 1024th note is the shortest note supported by MuseScore and MusicXML

# 2. for dotted note, if the increased duration caused by dot is shorter
# than 1024th note, the entire duration is invalid, even if its total duration
# is longer than 1024th note

# 3. for tuplet, in theory, if the total duration or the increased duration
# caused by dot, after applying tuplers, is shorter than 1024th,
# the tuplet should be considered invalid

# 4. but in MuseScore, there are several bugs in handling tuplet:
# you can type 1024th note, but MuseScore can't play it;
# if you divide dotted 256th to 9 nonuplets,
# the total duration is shorter than 1024th note,
# but MuseScore can still play it

# 5. according to the above information, here are some criteria for
# checking note duration:
# the total duration must be longer than 1024th note
# the increased duration caused by dot must be longer than 1024th note
# for tuplet, only check its total duration and the increased duration caused
# by dot without applying tuplers


#' @keywords internal
#' @export
check_duration <- function(duration, ...) {
  UseMethod("check_duration")
}


#' @keywords internal
#' @export
check_duration.numeric <- function(duration, name = NULL, abort = TRUE) {
  if (duration %% to_value.duration_type("1024th") != 0) {
    if (is.null(name)) {
      name <- deparse(substitute(duration))
    }

    duration %<>%
      to_Duration() %>%
      print("console", TRUE)

    specific <- paste(
      '`{name}` is equivalent to "{duration}", in which the duration',
      "generated by dot is shorter than 1024th note."
    )

    if (abort == FALSE) {
      specific %<>%
        glue::glue() %>%
        unclass()
      return(specific)
    }

    general <- paste(
      "In `{name}`, the additional duration generated by dot",
      "must not be shorter than 1024th note,",
      "which is shortest note supported."
    )

    show_errors(general, specific, env = environment())
  }
}


#' @keywords internal
#' @export
check_duration.character <- function(duration, name = NULL, abort = FALSE) {
  d <- parse_duration_notation(duration)

  type <- d$type
  dot <- d$dot
  ns <- d$ns

  v_type <- to_value.duration_type(type)
  v_dot <- to_value.dot(dot)
  v_ns <- prod(ns)

  v_total <- v_type * v_dot / v_ns
  v_1024 <-  to_value.duration_type("1024th")

  if (is.null(name)) {
    name <- deparse(substitute(duration))
  }

  if (v_total < v_1024) {
    general <- paste(
      "`{name}` must not be shorter than 1024th note,",
      "which is the shortest note supported."
    )

    specific <- '`{name}` is "{duration}", which is shorter than 1024th note.'

    if (abort == FALSE) {
      specific %<>%
        glue::glue() %>%
        unclass()
      return(specific)
    }

    show_errors(general, specific, env = environment())
  }

  if ((v_type * v_dot) %% v_1024 != 0) {
    general <- paste(
      "In `{name}`, the additional duration generated by dot",
      "must not be shorter than 1024th note,",
      "which is shortest note supported."
    )

    if (length(ns) != 0) {
      duration <- list(type = type, dot = dot) %>% print.unit()

      specific <- paste(
        '`{name}` is a tuplet of {duration}, in which the duration',
        "generated by dot is shorter than 1024th note."
      )
    } else {
      specific <- paste(
        '`{name}` is "{duration}", in which the duration',
        "generated by dot is shorter than 1024th note."
      )
    }

    if (abort == FALSE) {
      specific %<>%
        glue::glue() %>%
        unclass()
      return(specific)
    }

    show_errors(general, specific, env = environment())
  }
}



# Tupler ------------------------------------------------------------------

#' @export
Tupler <- function(n, unit = NULL, take = unit) {
  # check arguments
  check_positive_integer(n)
  check_tupler_unit(unit)
  check_tupler_unit(take)
  check_tupler_take(take, unit, n)

  # normalize arguments
  n %<>% as.integer()
  unit %<>% normalize_tupler_unit()
  take %<>% normalize_tupler_unit()

  # create Tupler
  list(n = n, unit = unit, take = take) %>% `class<-`("Tupler")
}


check_tupler_unit <- function(unit) {
  # early return ----------------------------------------------------------
  if (is.null(unit)) {
    return()
  }


  # basic checking --------------------------------------------------------
  name <- deparse(substitute(unit))

  check_type(unit, c("character", "double", "integer"), name)
  check_length(unit, 1, name)


  # check content ---------------------------------------------------------
  general <- paste(
    "`{name}` must be a non-tuplet duration notation",
    "or duration value, and have valid duration."
  )

  specific <- NULL

  if (is.character(unit)) {
    if (!is_duration_notation(unit)) {
      specific <- '`{name}` is "{unit}", which is not a duration notation.'
    } else if (!is_duration_notation(unit, tupler = FALSE)) {
      specific <- '`{name}` is "{unit}", which is a tuplet.'
    }
  } else if (is.numeric(unit)) {
    if (!is_duration_value(unit)) {
      specific <- '`{name}` is {unit}, which is not a duration value.'
    }
  }

  show_errors(general, specific, env = environment())


  # check duration --------------------------------------------------------
  check_duration(unit)
}


is_take_valid <- function(take, unit, n) {
  # duration notation, list -> duration value
  ds <- list(take = take, unit = unit)

  for (i in 1:2) {
    d <- ds[[i]]

    if (is.character(d)) {
      ds[[i]] %<>%
        to_Duration() %>%
        to_value()
    } else if (is.list(d)) {
      ds[[i]] %<>%
        {Duration(.$type, .$dot)} %>%
        to_value()
    }
  }

  # `take` must not be longer than `unit` * `n`
  ds$take <= ds$unit * n
}


check_tupler_take <- function(take, unit, n) {
  if (!is.null(take) && !is.null(unit) && !is_take_valid(take, unit, n)) {
    general <- "`take` must not be longer than `unit` times `n`."

    take %<>% quote_string()
    unit %<>% quote_string()
    specific <- "* {take} is longer than {unit} times {n}."

    glue::glue(general, "\n\n", specific) %>% rlang::abort()
  }
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



# print Tupler ------------------------------------------------------------

#' @export
print.Tupler <- function(x, type = NULL, dot = NULL, silent = FALSE, ...) {
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

  # print or return string
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# print Duration ----------------------------------------------------------

#' @export
print.Duration <- function(x, context = "console", silent = FALSE, ...) {
  # convert `x` to string
  if (context == "inside") {
    s <- x %>%
      to_value() %>%
      MASS::fractions() %>%
      attr("fracs")

  } else {
    type <- x$type
    dot <- x$dot
    tuplers <- x$tuplers
    s <- paste0(type, strrep(".", dot))

    if (length(tuplers) > 0) {
      s %<>% paste(print.tuplers(tuplers, type, dot))
    }
  }

  # print or return string
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


print.tuplers <- function(tuplers, type, dot) {
  ss <- character(0)

  for (t in tuplers) {
    ss[length(ss) + 1] <- print(t, type, dot, silent = TRUE)

    # re-assign `type` and `dot`
    take <- t$take
    type <- take$type
    dot <- take$dot
  }

  paste(ss, collapse = " ")
}



# tuplet ------------------------------------------------------------------

#' @export
tuplet <- function(duration, ...) {
  tuplers <- list(...)

  # check `duration` and `tuplers`
  check_tuplet_duration(duration)
  check_tuplet_tuplers(tuplers)

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
  if (class(duration)[1] == "Duration") {
    return()
  }

  general <- paste(
    "`duration` must be a duration notation, duration value",
    "or Duration."
  )

  check_type(duration, c("character", "double", "integer"), general = general)
  check_length(duration, 1, general = general)


  # check content ---------------------------------------------------------
  specific <- NULL

  if (is.character(duration) && !is_duration_notation(duration)) {
    specific <-
      '`duration` is "{duration}", which is not a duration notation.'
  } else if (is.numeric(duration) && !is_duration_value(duration)) {
    specific <- '`duration` is {duration}, which is not a duration value.'
  }

  show_errors(general, specific, env = environment())


  # check duration --------------------------------------------------------
  check_duration(duration)
}


check_tuplet_tuplers <- function(tuplers) {
  l <- length(tuplers)

  if (l > 0) {
    general <- "`list(...)` must contain only Tuplers, or be empty."
    specifics <- character(0)

    for (i in 1:l) {
      t <- tuplers[[i]]
      c_ <- class(t)[1]

      if (c_ != "Tupler") {
        specifics[[length(specifics) + 1]] <-
          "`list(...)[[{i}]]` is of class {c_}." %>%
          glue::glue() %>%
          unclass()
      }
    }

    show_errors(general, specifics)
  }
}


normalize_tuplet_tuplers <- function(tuplers, duration) {
  l <- length(tuplers)

  # early return ----------------------------------------------------------
  if (l == 0) {
    return(tuplers)
  }


  # get initial `type` and `dot` ------------------------------------------
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


  # normalize `tuplers` ---------------------------------------------------
  for (i in 1:l) {
    # unpack tupler
    tupler <- tuplers[[i]]
    n <- tupler$n
    unit <- tupler$unit
    take <- tupler$take

    # convert `unit` if it's `NULL`
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
      check_divisible_by_unit(type, dot, unit, i)
    }

    # reset `type` and `dot`
    type <- take$type
    dot <- take$dot

    # re-assign `tuplers`
    tuplers[[i]] <- tupler
  }

  tuplers
}


check_tuplet_unit <- function(unit_type, env) {
  if (is.na(unit_type)) {
    general <- paste(
      "In each Tupler in `list(...)`, `unit$type` must not be shorter than",
      '"1024th", which is the shortest duration type supported.'
    )

    specific <- paste(
      'In dividing "{type}" into {n} tuplets,',
      '`list(...)[[{i}]]$unit$type` is shorter than "1024th".'
    )

    show_errors(general, specific, env = env)
  }
}


check_tuplet_take <- function(take, unit, n, i) {
  if (!is_take_valid(take, unit, n)) {
    general <- paste(
      "In each Tupler in `list(...)`,",
      "`take` must not be longer than `unit` times `n`."
    )

    take %<>% print.unit()
    unit %<>% print.unit()

    specific <- paste(
      "In `list(...)[[{i}]]`, {take} is longer than {unit} times {n}."
    )

    show_errors(general, specific, env = environment())
  }
}


check_divisible_by_unit <- function(type, dot, unit, i) {
  unit_type <- unit$type
  unit_dot <- unit$dot
  i_type <- which(duration_types == type)
  i_unit_type <- which(duration_types == unit_type)

  # 2^i
  con <- unit_dot == dot && i_unit_type >= i_type
  if (dot == 1) {
    # 3 * 2^i
    con <- con ||
      unit_dot == 0 && i_unit_type > i_type
  } else if (dot == 2) {
    # 7 * 2^i
    con <- con ||
      unit_dot == 0 && i_unit_type > (i_type + 1)
  } else if (dot == 3) {
    # 15 * 2^i
    con <- con ||
      unit_dot == 1 && i_unit_type > (i_type + 1)
  } else if (dot == 4) {
    # 31 * 2^i
    con <- con ||
      unit_dot == 0 && i_unit_type > (i_type + 3)
  }

  if (!con) {
    general <- paste(
      '`unit` in each Tupler in `list(...)` must divide the previous "take".'
    )

    unit %<>% print.unit()
    take <- list(type = type, dot = dot) %>% print.unit()

    specific <- paste(
      "`list(...)[[{i}]]$unit` {unit} can't divide {take}."
    )

    show_errors(general, specific, env = environment())
  }
}


# convert Tupler's `unit` to string
print.unit <- function(unit) {
  unit %>%
    {Duration(.$type, .$dot)} %>%
    print(context = "console", silent = TRUE) %>%
    quote_string()
}


check_tuplet_out <- function(duration) {
  value <- to_value(duration)
  string <- duration %>%
    print("console", TRUE)

  if (value < to_value.duration_type("1024th")) {
    general <- paste(
      "The duration of the output Duration of `tuplet()`",
      "must not be shorter than 1024th note,",
      "which is the shortest note supported."
    )

    specific <- paste(
      'The output Duration is "{string}",',
      "which is shorter than 1024th note."
    )

    show_errors(general, specific, env = environment())
  }
}



# DurationLine ------------------------------------------------------------

DurationLine <- function(durations) {
  # check `durations`
  check_type(durations, "list")
  check_length(durations, Inf)
  check_durations(durations)

  # normalize `durations`
  durations %<>% lapply(to_Duration)

  # check if tuplets in `durations` form groups
  check_tuplet_group(durations)

  # add `$.value` to each item in `durations` for further use
  durations %<>% add_value()

  # create DurationLine
  list(durations = durations) %>% `class<-`("DurationLine")
}



# check `durations` in `DurationLine` -------------------------------------

check_durations <- function(durations) {
  general <- paste(
    "Each item of `durations` must be a duration notation,",
    "duration value or Duration object, and have valid duration.",
    "See `?tuplet`."
  )

  specifics <- character(0)

  for (i in 1:length(durations)) {
    d <- durations[[i]]
    l <- length(d)
    t <- typeof(d)
    c_ <- class(d)[1]

    # escape Durations
    if (c_ == "Duration") {
      next
    }

    # check type
    if (!(t %in% c("character", "integer", "double"))) {
      specifics[[length(specifics) + 1]] <-
        "`durations[[{i}]]` is of type {t}." %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check length
    if (l != 1) {
      specifics[[length(specifics) + 1]] <-
        "`durations[[{i}]]` is of length {l}." %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check if is a duration alue
    if (is.numeric(d) && !is_duration_value(d)) {
      specifics[[length(specifics) + 1]] <-
        "`durations[[{i}]]` is {d}, which is not a duration value." %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check if is a duration notation
    if (is.character(d) && !is_duration_notation(d)) {
      specifics[[length(specifics) + 1]] <-
        '`durations[[{i}]]` is "{d}", which is not a duration notation.' %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check duration
    e <- check_duration(d, "durations[[{i}]]", FALSE)
    if (!is.null(e)) {
      specifics[[length(specifics) + 1]] <-
        e %>%
        glue::glue() %>%
        unclass()

      next
    }
  }

  show_errors(general, specifics, env = environment())
}



# check tuplet group ------------------------------------------------------

# check if there is any incomplete tuplet group in `durations`
# see also `reduce_tuplets`
check_tuplet_group <- function(durations) {
  # shortcut abort function
  abort <- function(i, class) {
    general <- "Tuplets in `durations` must form complete groups."

    specific <-
      "The tuplet group containing `durations[[{i}]]` is incomplete." %>%
      glue::glue() %>%
      unclass()

    if (i == l) {
      supplement <- NULL
    } else {
      supplement <- "Subsequent tuplet groups, if any, are not checked."
    }

    show_errors(general, specific, supplement, class = class)
  }

  # "working memory" to store temporarily undecided tuplets
  wm <- list()
  l <- length(durations)

  for (i in 1:l) {
    d <- durations[[i]]
    l_wm <- length(wm)

    # skip non-tuplet `d` if `wm` is empty
    if (l_wm == 0 && !is_tuplet(d)) {
      next
    }

    # if `wm` is not empty,
    # and `d` is non-tuplet or incompatible with the last tuplet in `wm`,
    # then the group containing the last tuplet is incomplete,
    # trigger "incompatible" error
    if (l_wm != 0) {
      last <- wm[[l_wm]]

      if (!is_tuplet(d) || (is_tuplet(d) && !is_compatible(d, last))) {
        abort(i - 1, "incompatible")
      }
    }

    # store `d` in `wm`
    wm %<>% c(list(d))

    # try to reduce `wm`
    tryCatch(
      {wm %<>% reduce_tuplets()},
      # trigger "over-complete" error
      error = function(e) abort(i - 1, "over-complete")
    )

    # if `wm` is not totally reduced,
    # but the loop has already reached the end, trigger "incomplete" error,
    if (length(wm) != 0 && i == l) {
      abort(i, "incomplete")
    }
  }
}


is_tuplet <- function(duration) {
  duration %>%
    .$tuplers %>%
    length() %>%
    as.logical()
}


# extra items may be added to Duration for convenience, remove them
clear_duration <- function(duration) {
  original <- names(tuplet(1))

  for (name in names(duration)) {
    if (!(name %in% original)) {
      duration[[name]] <- NULL
    }
  }

  duration
}


# being compatible means two tuplet share a common ancestor, meanwhile,
# the depth of `tuplet` is the same with or larger than `tuplet_0`'s
is_compatible <- function(tuplet, tuplet_0) {
  tuplet %<>% clear_duration()
  tuplet_0 %<>% clear_duration()

  # get depths of these two tuplets
  depth <- tuplet$tuplers %>% length()
  depth_0 <- tuplet_0$tuplers %>% length()

  # the depth of `tuplet` must not be less than `tuplet_0`'s
  if (depth < depth_0) {
    return(FALSE)
  }

  # remove the Tuplers beyond `depth_0` in `tuplet`
  if (depth > depth_0) {
    tuplet$tuplers[(depth_0 + 1):depth] <- NULL
  }

  # set `take` to `NULL` at the last level in both tuplets
  tuplet$tuplers[[depth_0]]$take <- NULL
  tuplet_0$tuplers[[depth_0]]$take <- NULL

  # now compare these two tuplets
  identical(tuplet, tuplet_0)
}


# if the tuplets at the deepest level form a group at that level,
# reduce that level, repeat this process until no tuplet is left in `tuplets`
reduce_tuplets <- function(tuplets) {
  repeat {
    # get depths of `tuplets`
    depths <- sapply(tuplets, function(tuplet) length(tuplet$tuplers))

    # get the largest depth
    depth_max <- max(depths)

    # reset `tuplets` if no tuplet left in `tuplets`,
    if (depth_max == 0) {
      return(list())
    }
    # which means `tuplet` forms a tuplet group,
    # and is reduced to a non-tuplet

    # get the indices of the tuplets of `depth_max`
    ks <- which(depths == depth_max)

    # sum up the last Tuplers of these tuplets
    total <-
      tuplets[ks] %>%
      sapply(function(tuplet) to_value(tuplet$tuplers[[depth_max]])) %>%
      sum()

    # if `total` is 1, then the group is complete at level `depth_max`,
    # reduce that level:
    if (total == 1) {
      # keep the first tuple at that level, remove its last Tupler,
      tuplets[[ks[1]]]$tuplers[[depth_max]] <- NULL
      # remove other tuplets
      tuplets[ks[-1]] <- NULL
      # go to next loop
      next
    }

    # if `total` is less than 1, the reducing process will stop
    if (total < 1) {
      return(tuplets)
    }

    # if `total` is larger than 1,
    # it means the group at current level has not been complete yet,
    # but the next tuplet is already in `tuplets`,
    # then the group containing the last tuplet is incomplete
    if (total > 1) {
      stop()
    }
  }
}



# mark tuplet -------------------------------------------------------------

# leave marks in each tuplet in `durations`,
# to tell if it is the first or last tuplet of a group,
# and of which level
mark_tuplet <- function(durations) {
  wm <- list()

  for (i in 1:length(durations)) {
    d <- durations[[i]]

    # skip non-tuplets
    if (!is_tuplet(d)) {
      next
    }

    # get the depth of `d`
    depth <- length(d$tuplers)

    l <- length(wm)

    # add `$.start` to current tuplet
    if (l == 0) {
      durations[[i]]$.start <- 1:depth
    } else {
      last <- wm[[l]]
      depth_last <- length(last$tuplers)
      if (depth_last < depth) {
        durations[[i]]$.start <- (depth_last + 1):depth
      }
    }

    # add `d` to `wm`
    wm %<>% c(list(d))

    # reduce tuplets in `wm`
    wm %<>% reduce_tuplets()

    # get the length of `wm` again
    l <- length(wm)

    # add `$.end` to current tuplet
    if (l == 0) {
      durations[[i]]$.end <- 1:depth
    } else {
      last <- wm[[l]]
      depth_last <- length(last$tuplers)
      if (depth_last < depth) {
        durations[[i]]$.end <- (depth_last + 1):depth
      }
    }
  }

  durations
}



# print DurationLine ------------------------------------------------------

#' @keywords internal
#' @export
print.DurationLine <- function(x, context = "console", silent = FALSE, ...) {
  s <-
    x$durations %>%
    sapply(print, context = context, silent = TRUE) %>%
    paste(collapse = ", ")

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# untie duration value ----------------------------------------------------

untie_duration_value <- function(value, values = to_values.duration_types(0),
                                 decreasing = TRUE, abort = TRUE) {
  core <- function(value) {
    if (value %in% values) {
      return(value)

    } else if (value < values[length(values)]) {
      if (abort) {
        stop()
      } else {
        return(value)
      }

    } else {
      if (value > values[1]) {
        k <- 1
      } else {
        ks <- which(values > value)
        k <- ks[length(ks)] + 1
      }

      v <- values[k]

      if (decreasing) {
        return(c(v, core(value - v)))
      } else {
        return(c(core(value - v), v))
      }
    }
  }

  core(value)
}


is_tied_duration_value <- function(value) {
  tryCatch(
    {
      untie_duration_value(value)
      TRUE
    },
    error = function(e) FALSE
  )
}
