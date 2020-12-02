# DurationNote ------------------------------------------------------

#' @keywords internal
#' @export
DurationNote <- function(duration) {
  UseMethod("DurationNote")
}


#' @keywords internal
#' @export
DurationNote.Duration <- function(duration) {
  duration
}



# duration notation -------------------------------------------------

# duration types

duration_types <- c(
  "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
  "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"
)


duration_type_abbrs <- c(
  "m", "l", "b", "w", "h", "q", "8", "16", "32", "64", "128", "256",
  "512", "1024"
)



# duration notation validator

is_duration_notation <- function(notation, dot = TRUE, tupler = TRUE) {
  if (!is.character(notation)) {
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
    ifelse(dot, "(\\.{1,4})?", ""),
    # maybe followed by some tupler notations
    ifelse(tupler, "(/([1-9][0-9]*))*", ""),
    "$"
  )

  grepl(reg, notation)
}



# duration notation -> Duration

#' @keywords internal
#' @export
DurationNote.character <- function(duration) {
  parts <- split_duration_notation(duration)

  type <- parts[[1]]
  if (type %in% duration_type_abbrs) {
    type <- to_duration_type.abbr(type)
  }

  dot <- parts[[2]] %>% nchar()

  tuplers <- parts[[3]] %>%
    to_ns.tupler_notations() %>%
    to_Tuplers.ns(type, dot)

  list(type = type, dot = dot, tuplers = tuplers) %>%
    `class<-`(c("Duration", "Printable"))
}


split_duration_notation <- function(notation) {
  # get certain component from a duration notation
  get_part <- function(reg, ...) {
    ks <- gregexpr(reg, notation, ...)[[1]]
    out <- c()

    for (i in 1:length(ks)) {
      out <- substr(
        notation,
        ks[i],
        attr(ks, "match.length")[i] + ks[i] - 1
      ) %>% c(out, .)
    }

    out
  }

  reg_type <- c(duration_types, duration_type_abbrs) %>%
    paste(collapse = "|") %>%
    # must not extract digits following "/"
    paste0("(?<!/)(", ., ")")

  list(
    # duration type or its abbreviation
    get_part(reg_type, perl = TRUE),
    # dot notation
    get_part("\\.{1,4}"),
    # tupler notations
    get_part("/[0-9]*")
  )
}


to_duration_type.abbr <- function(abbr) {
  abbr %>%
    {which(duration_type_abbrs == .)} %>%
    duration_types[.]
}


to_ns.tupler_notations <- function(notations) {
  if (identical(notations, "")) {
    return(NULL)
  }

  sapply(notations, function(notation) {
    notation %>%
      substr(2, nchar(.)) %>%
      as.integer()
  }) %>% unname()
}


divide_duration_type <- function(duration_type, n) {
  n %>%
    log2() %>%
    floor() %>%
    sum(which(duration_types == duration_type)) %>%
    duration_types[.]
}


to_Tuplers.ns <- function(ns, type, dot) {
  ts <- list()

  for (n in ns) {
    # `type` shoud be re-assigned
    type <- divide_duration_type(type, n)

    ts[[length(ts) + 1]] <- type %>%
      {list(type = ., dot = dot)} %>%
      {list(n = n, unit = ., take = .)} %>%
      `class<-`(c("Tupler", "Printable"))
  }

  ts
}



# duration value ----------------------------------------------------

# duration values (duration value validator)

to_value.duration_type <- function(duration_type) {
  # quarter note
  v_q <- 1
  i_q <- which(duration_types == "quarter")

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
    unname() %>%
    sapply(function(value) sapply(dot, to_value.dot) * value) %>%
    sort(decreasing = TRUE)
}


duration_values <- to_values.duration_types()



# duration value -> Duration

#' @keywords internal
#' @export
DurationNote.numeric <- function(duration) {
  i <- which(duration_values == duration)

  type <- ((i - 1) %/% 5 + 1) %>% duration_types[.]

  dot <- ((5 - (i %% 5)) %% 5) %>% as.integer()

  list(type = type, dot = dot, tuplers = list()) %>%
    `class<-`(c("Duration", "Printable"))
}



# -> value

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
  ) %>% MASS::fractions()
}



# duration length validators ----------------------------------------

#' @keywords internal
#' @export
is_duration_short <- function(duration) {
  UseMethod("is_duration_short")
}


#' @keywords internal
#' @export
is_duration_short.character <- function(duration) {
  parts <- split_duration_notation(duration)

  v_type <- parts[[1]] %>% to_value.duration_type()

  v_dot <- parts[[2]] %>%
    nchar() %>%
    to_value.dot()

  n <- parts[[3]] %>%
    to_ns.tupler_notations() %>%
    prod()

  v_total <- v_type * v_dot / n

  v_1024 <- "1024th" %>% to_value.duration_type()

  `||`(
    # total value must be longer than 1024th
    v_total < v_1024,
    # value of dotted type must be a multiple of 1024th
    (v_type * v_dot) %% v_1024 != 0
  )
}


#' @keywords internal
#' @export
is_duration_short.numeric <- function(duration) {
  duration %% to_value.duration_type("1024th") != 0
}


#' @keywords internal
#' @export
is_duration_short.Duration <- function(duration) {
  to_value(duration) < to_value.duration_type("1024th")
}


check_duration_length <- function(duration, what) {
  m <- " and also a multiple of " %>%
    {ifelse(is.character(duration) || is.numeric(duration), ., " ")} %>%
    paste0("{what} must be longer than", ., "1024th note,") %>%
    paste("which is the legitimate shortest note.\n\n")

  if (is_duration_short(duration)) {
    if (is.character(duration)) {
      s <- duration %>%
        paste0('"', ., '"') %>%
        paste0("* You've supplied duration notation ", ., ".")

    } else if (is.numeric(duration)) {
      s <- duration %>%
        DurationNote() %>%
        to_string() %>%
        paste0('"', ., '"') %>%
        paste0("* You've supplied duration value {duration},",
          " which is equivalent to ", ., ".")

    } else {
      s <- duration %>%
        to_string() %>%
        paste0('"', ., '"') %>%
        paste0("* You've supplied Duration object ", ., ".")
    }

    s %>%
      paste0(m, .) %>%
      glue::glue() %>%
      rlang::abort()
  }
}



# Tupler ------------------------------------------------------------

#' @export
Tupler <- function(n, unit = "auto", take = unit) {
  check_tupler_n(n)
  check_tupler_unit(unit, "unit")
  check_tupler_unit(take, "take")

  n <- as.integer(n)

  if (!identical(unit, "auto")) {
    unit <- normalize_tupler_unit(unit)
  }

  if (!identical(take, "auto")) {
    take <- normalize_tupler_unit(take)
  } else {
    take <- unit
  }

  # check length of `take`
  if (!identical(unit, "auto") && !identical(take, "auto")) {
    if (is_take_long(n, unit, take)) {
      s_unit <- unit %>%
        {paste0(.$type, strrep(".", .$dot))} %>%
        paste0('"', ., '"')

      s_take <- take %>%
        {paste0(.$type, strrep(".", .$dot))} %>%
        paste0('"', ., '"')

      "`take` must be no longer than `unit` times `n`.\n\n" %>%
        glue::glue("* {s_take} is longer than {s_unit} times {n}.") %>%
        rlang::abort()
    }
  }

  list(n = n, unit = unit, take = take) %>%
    `class<-`("Tupler")
}



# validators and normalizer

check_tupler_n <- function(n) {
  check_type(supplied = n, valid = c("double", "integer"), name = "n")
  check_length(supplied = n, valid = 1, type = "numeric", name = "n")

  # check if is an integer
  if (as.integer(n) != n || n <= 0) {
    glue::glue(
      "`n` must be a positive integer.\n",
      "* You've supplied {n}."
    ) %>% rlang::abort()
  }
}


check_tupler_unit <- function(unit, argument = "unit") {
  if (identical(unit, "auto")) {
    return(invisible(NULL))
  }

  check_type(
    supplied = unit,
    valid = c("character", "double", "integer"),
    name = argument
  )

  check_length(supplied = unit, valid = 1, name = argument)

  # check if is a duration notation or a duration value
  gm <- paste(
    "`{argument}` must be a duration type followed by 0 to 4 dots,",
    "or a duration value.\n\n"
  )

  con <-
    (is.character(unit) && is_duration_notation(unit, tupler = FALSE)) ||
    (is.numeric(unit) && unit %in% duration_values)

  if (!con) {
    if (is.character(unit)) {
      m <- '* You\'ve supplied "{unit}".'
    } else {
      m <- "* You've supplied {unit}."
    }

    gm %>%
      paste0(m) %>%
      glue::glue() %>%
      rlang::abort()
  }

  # check if has proper length
  check_duration_length(unit, paste0("`", argument, "`"))
}


normalize_tupler_unit <- function(unit) {
  unit %>%
    DurationNote() %T>%
    {`<-`(.$tuplers, NULL)} %>%
    unclass()
}


is_take_long <- function(n, unit, take) {
  v_unit <- to_value.duration_type(unit$type) * to_value.dot(unit$dot)
  v_take <- to_value.duration_type(take$type) * to_value.dot(take$dot)
  v_total <- v_unit * n

  v_take > v_total
}



# tuplet ------------------------------------------------------------

#' @export
Tuplet <- function(duration, ...) {
  check_tuplet_duration(duration)
  d <- DurationNote(duration)

  ts <- list(...)
  l <- length(ts)

  if (l == 0) {
    return(d)
  }

  # check if is Tupler
  if (l > 0) {
    gm <- "When `list(...)` is not empty," %>%
      paste("each item of it must be a Tupler object.\n")
    ms <- c()

    for (i in 1:l) {
      t <- ts[[i]]
      c_ <- class(t)

      if (c_ != "Tupler") {
        article <- ifelse(c_ %in% vowel_types, "an", "a")
        ms <- "* `list(...)[[{i}]]` is {article} {c_}." %>%
          glue::glue() %>%
          unclass() %>%
          c(ms, .)
      }
    }

    show_errors(ms, gm)
  }

  ts0 <- d$tuplers
  l0 <- length(ts0)

  # get initial type and dot
  if (l0 == 0) {
    type <- d$type
    dot <- d$dot

  } else {
    take <- ts0[[l0]]$take
    type <- take$type
    dot <- take$dot
  }

  # check `take` and `unit`
  for (i in 1:l) {
    t <- ts[[i]]
    n <- t$n
    unit <- t$unit
    take <- t$take

    if (i == l) {
      s_sub <- ""
    } else if (i == l - 1) {
      s_sub <- "\n\nThe subsequent Tupler is not checked."
    } else if (i < l - 1) {
      s_sub <- "\n\nSubsequent Tuplers are not checked."
    }

    # convert "auto"
    if (identical(unit, "auto")) {
      unit_type <- divide_duration_type(type, n)

      # check `unit_type` length
      if (is.na(unit_type)) {
        "Outcome duration must be longer than 1024th note," %>%
          paste("which is the legitimate shortest note.\n\n") %>%
          paste("* After applying the Tupler `list(...)[[{i}]]`,") %>%
          paste("the duration is too short.") %>%
          glue::glue() %>%
          rlang::abort()
      }

      unit <- list(type = unit_type, dot = dot)
      t$unit <- unit

      if (identical(take, "auto")) {
        take <- unit
        t$take <- take

      # check `take`
      } else {
        if (is_take_long(n, unit, take)) {
          s_unit <- unit %>%
            {paste0(.$type, strrep(".", .$dot))} %>%
            paste0('"', ., '"')

          s_take <- take %>%
            {paste0(.$type, strrep(".", .$dot))} %>%
            paste0('"', ., '"')

          "In each Tupler object of `list(...)`," %>%
            paste("`take` must be no longer than `unit` times `n`.\n\n") %>%
            paste0("* In `list(...)[[{i}]]`,") %>%
            paste("{s_take} is longer than {s_unit} times {n}.{s_sub}") %>%
            glue::glue() %>%
            rlang::abort()
        }
      }

    # check `unit`
    } else {
      if (is_indivisible(type, dot, unit)) {
        "In each Tupler object of `list(...)`," %>%
          paste("`unit` must be of valid length. See `?Tuplet`.\n\n") %>%
          paste0("* In `list(...)[[{i}]]`,") %>%
          paste("`unit` is not of valid length.{s_sub}") %>%
          glue::glue() %>%
          rlang::abort()
      }
    }

    # reset type and dot
    type <- take$type
    dot <- take$dot

    # add to original tuplers
    ts0[[l0 + 1]] <- t
    l0 <- l0 + 1
  }

  d$tuplers <- ts0

  if (is_duration_short(d)) {
    d %>%
      to_string() %>%
      paste0('"', ., '"') %>%
      paste0("* The outcome duration ", ., " is too short.") %>%
      paste0("Outcome duration must be longer than 1024th note, ",
            "which is the legitimate shortest note.\n\n", .) %>%
      glue::glue() %>%
      rlang::abort()
  }

  d
}


check_tuplet_duration <- function(duration) {
  check_type(
    supplied = duration,
    method = class,
    valid = c("character", "numeric", "integer", "Duration"),
    name = "duration"
  )

  l <- length(duration)

  # check duration notation
  if (is.character(duration)) {
    gm <- "When a character is supplied as `duration`,"

    if (l != 1) {
      gm %>%
        paste("it must be of length 1.\n\n") %>%
        paste0("* You've supplied a character of length {l}.") %>%
        glue::glue() %>%
        rlang::abort()
    }

    if (!is_duration_notation(duration)) {
      gm %>%
        paste("it must be a duration notation.\n\n") %>%
        paste0('* You\'ve supplied "{duration}".') %>%
        glue::glue() %>%
        rlang::abort()
    }
  }

  # check duration value
  if (is.numeric(duration)) {
    gm <- "When a numeric is supplied as `duration`,"

    if (l != 1) {
      gm %>%
        paste("it must be of length 1.\n\n") %>%
        paste0("* You've supplied a numeric of length {l}.") %>%
        glue::glue() %>%
        rlang::abort()
    }

    if (!(duration %in% duration_values)) {
      gm %>%
        paste("it must be a duration value.\n\n") %>%
        paste0('* You\'ve supplied {duration}.') %>%
        glue::glue() %>%
        rlang::abort()
    }
  }

  check_duration_length(duration, "`duration`")
}


is_indivisible <- function(type, dot, unit) {
  unit_type <- unit$type
  unit_dot <- unit$dot
  i_type <- which(duration_types == type)
  i_unit_type <- which(duration_types == unit_type)

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

  !con
}



# -> string ---------------------------------------------------------

to_string.dot <- function(dot) {
  dot %>% strrep(".", .)
}


#' @keywords internal
#' @export
to_string.Tupler <- function(x, type = NULL, dot = NULL, ...) {
  n <- x$n
  s_n <- paste("/", x$n)

  unit <- x$unit
  if (!identical(unit, "auto")) {
    unit_type <- unit$type
    unit_dot <- unit$dot
    s_unit <- paste0(unit_type, to_string.dot(unit_dot))
  }

  take <- x$take
  if (!identical(take, "auto")) {
    take_type <- take$type
    take_dot <- take$dot
    s_take <- paste0(take_type, to_string.dot(take_dot))
  }

  if (identical(unit, "auto")) {
    if (identical(take, "auto")) {
      s_n
    } else {
      paste(s_n, " * ", "(", s_take, " / ", "_", ")")
    }

  } else {
    con <-
      !is.null(type) &&
      !is.null(dot) &&
      identical(unit, take) &&
      unit_type == divide_duration_type(type, n) &&
      unit_dot == dot

    if (con) {
      s_n
    } else {
      paste0(s_n, " * ", "(", s_take, " / ", s_unit, ")")
    }
  }
}


to_string.tuplers <- function(tuplers, type, dot) {
  ss <- c()

  for (t in tuplers) {
    ss <- to_string.Tupler(t, type, dot) %>%
      c(ss, .)

    take <- t$take
    type <- take$type
    dot <- take$dot
  }

  paste(ss, collapse = " ")
}


#' @keywords internal
#' @export
to_string.Duration <- function(x, ...) {
  type <- x$type
  dot <- x$dot
  tuplers <- x$tuplers
  l <- tuplers %>% length

  s <- dot %>%
    strrep(".", .) %>%
    paste0(type, .)

  if (l == 0) {
    s
  } else {
    tuplers %>%
      to_string.tuplers(type, dot) %>%
      paste(s, .)
  }
}



# DurationLine ------------------------------------------------------

#' @export
Duration <- function(durations) {
  durations %T>%
    check_duration_line() %>%
    DurationLine() %T>%
    check_tuplet_groups()
}


check_duration_line <- function(durations) {
  check_type(supplied = durations, valid = "list", name = "durations")

  l <- length(durations)

  check_length(
    l = l,
    valid = "l > 0",
    valid_phrase = "larger than 0",
    name = "durations",
    type = "list"
  )

  # check each item
  m <- paste(
    "Each item of `durations` must be a duration notation,",
    "duration value or Duration object, and of valid length.\n"
  )
  ms <- c()
  cs <- c("character", "integer", "numeric", "Duration")

  for (i in 1:l) {
    d <- durations[[i]]
    c_ <- class(d)
    l_ <- length(d)
    article <- ifelse(c_ %in% vowel_types, "an", "a")

    # check each item's class
    if (!(c_ %in% cs)) {
      ms <- "* `durations[[{i}]]` is {article} {c_}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }

    # check each item's length
    if (c_ != "Duration" && l_ != 1) {
      ms <- "* `durations[[{i}]]` is {article} {c_} of length {l_}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }

    con <-
      (c_ == "Duration" ||
      (is.character(d) && is_duration_notation(d)) ||
      (is.numeric(d) && d %in% duration_values)) &&
      !is_duration_short(d)

    if (!con) {
      if (is.character(d)) {
        d <- d %>%
          paste0('"', ., '"')
      }

      ms <- "* `durations[[{i}]]` is {d}." %>%
        glue::glue() %>%
        unclass() %>%
        c(ms, .)
      next
    }
  }

  show_errors(ms, m)
}


DurationLine <- function(durations) {
  durations %>%
    lapply(DurationNote) %>%
    `class<-`(c("DurationLine", "HalfPart", "Line", "Printable"))
}


check_tuplet_groups <- function(duration_line) {
  dl <- duration_line

  m <- "Tuplets in `durations` must form groups."

  # depth is the length of a tuplet's tuplers
  depths <- sapply(dl, function(d) length(d$tuplers))

  # reduce tuplets' depth if they form a group,
  # repeat this process until all depths are equal to 0,
  # otherwise trigger an error
  while (any(depths) > 0) {
    # start from the deepest level
    js <- which(depths == max(depths))
    l <- length(js)
    # store temporarily ungrouped tuplets,
    ds <- list()
    # and their positions in dl
    ks <- c()
    # positions of items which will be set to NULL later
    rs <- c()

    # do not mix i, j and k
    for (i in 1:l) {
      j <- js[i]
      d <- dl[[j]]
      ts_ <- d$tuplers
      # do not mix l and l_
      l_ <- length(ts_)
      t_ <- ts_[[l_]]
      # the value of the last tupler
      v <- to_value.Tupler(t_)

      # remove the last level if it is complete
      if (v == 1) {
        dl[[j]]$tuplers[[l_]] <- NULL

        # if ds is empty, put d into it,
        # unless d is the last tuplet at current level,
        # since now it is not enough to form a group
      } else if (length(ks) == 0) {
        if (i == l) {
          rlang::abort(m)
        } else {
          ds[[length(ds) + 1]] <- d
          ks <- c(ks, j)
        }

        # d and items in ds should be of the same structure
      } else if (!is_similar_tuplet(d, ds[[length(ds)]])) {
        rlang::abort(m)

        # check if d and ds form a group
      } else {
        ds[[length(ds) + 1]] <- d
        ks <- c(ks, j)
        # total value of the last tuplers of ds
        vs <- sum(sapply(ds, function(d) {
          to_value.Tupler(d$tuplers[[l_]])
        }))

        if (vs > 1) {
          rlang::abort(m)

          # if d and ds form a group,
          # remove the last level of the first tuplet,
          # and set the rest to NULL
        } else if (vs == 1) {
          dl[[ks[1]]]$tuplers[[l_]] <- NULL
          # add to rs first, set them to NULL later,
          # or there would be "subscript out of bounds" error
          rs <- c(rs, ks[-1])
          # reset ds and ks
          ds <- list()
          ks <- c()

        } else if (vs < 1 && i == l) {
          rlang::abort(m)
        }
      }
    }

    dl[rs] <- NULL
    # re-measure depths
    depths <- sapply(dl, function(d) length(d$tuplers))
  }
}


is_similar_tuplet <- function(tuplet, tuplet_0) {
  # assign each tuplet's `unit` of the last Tupler to `take`
  # at the same level, then compare these two tuplets

  ds <- list(tuplet, tuplet_0)

  for (i in 1:2) {
    d <- ds[[i]]
    ts <- d$tuplers
    l <- length(ts)
    t <- ts[[l]]
    unit <- t$unit
    ds[[i]]$tuplers[[l]]$take <- unit
  }

  identical(ds[[1]], ds[[2]])
}
