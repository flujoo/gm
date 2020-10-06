# globals -----------------------------------------------------------

#' @export
GLOBALS <- new.env()

GLOBALS$TYPE <- "quarter"
GLOBALS$VALUE <- 1



# notation -> Duration ----------------------------------------------

duration_types <- c(
  "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
  "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"
)


duration_type_abbrs <- c(
  "m", "l", "b", "w", "h", "q", "8", "16", "32", "64", "128", "256",
  "512", "1024"
)


validate.duration_notation <- function(notation, tupler = TRUE) {
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
    ifelse(tupler, "(/([2-9]|[1-9][0-9]+))*", ""),
    "$"
  )
  grepl(reg, notation)
}


split.duration_notation <- function(notation) {
  # get different parts from a duration notation
  get_part <- function(reg) {
    ks <- gregexpr(reg, notation)[[1]]
    out <- c()
    for (i in 1:length(ks)) {
      out_i <- substr(
        notation,
        ks[i],
        attr(ks, "match.length")[i] + ks[i] - 1
      )
      out <- c(out, out_i)
    }
    out
  }

  list(
    # duration type or its abbreviation
    get_part(paste(c(duration_types, duration_type_abbrs), collapse = "|")),
    # dot notation
    get_part("\\.{1,4}"),
    # tupler notations
    get_part("/[0-9]*")
  )
}


to_type.abbr <- function(abbr) {
  i <- which(duration_type_abbrs == abbr)
  duration_types[i]
}


to_ns.tupler_notations <- function(notations) {
  if (identical(notations, "")) {
    return(c())
  }
  sapply(notations, function(n) {
    as.double(substr(n, 2, nchar(n)))
  })
}


divide_type <- function(type, n) {
  i_type <- which(duration_types == type)
  d <- floor(log2(n))
  i <- i_type + d
  duration_types[i]
}


to_tuplers.ns <- function(ns, type, dot) {
  ts_ <- list()
  for (n in ns) {
    type <- divide_type(type, n)
    unit <- list(type = type, dot = dot)
    t_ <- list(n = n, unit = unit, take = unit)
    ts_[[length(ts_) + 1]] <- t_
  }
  ts_
}


to_Duration.notation <- function(notation) {
  parts <- split.duration_notation(notation)

  type <- parts[[1]]
  if (type %in% duration_type_abbrs) {
    type <- to_type.abbr(type)
  }

  dot <- nchar(parts[[2]])

  ts_ <- to_tuplers.ns(to_ns.tupler_notations(parts[[3]]), type, dot)

  d <- list(type = type, dot = dot, tuplers = ts_)
  class(d) <- "Duration"
  d
}



# Duration -> value -------------------------------------------------

to_value.type <- function(type) {
  i <- which(duration_types == GLOBALS$TYPE)
  j <- which(duration_types == type)
  GLOBALS$VALUE * 2^(i - j)
}


to_value.dot <- function(dot) {
  sum(2^(-(0:dot)))
}


to_value.tupler <- function(tupler) {
  n <- tupler$n
  unit <- tupler$unit
  take <- tupler$take
  v_unit <- to_value.type(unit$type) * to_value.dot(unit$dot)
  v_take <- to_value.type(take$type) * to_value.dot(take$dot)
  (1 / n) * (v_take / v_unit)
}


to_value.tuplers <- function(tuplers) {
  v <- sapply(tuplers, to_value.tupler)
  if (!length(v)) {
    return(1)
  }
  prod(v)
}


to_value.Duration <- function(duration) {
  to_value.type(duration$type) * to_value.dot(duration$dot) *
    to_value.tuplers(duration$tuplers)
}



# value -> Duration -------------------------------------------------

dot_type <- function(type, dot) {
  paste0(type, strrep(".", dot))
}


sort.types <- function(types) {
  is_ <- sapply(types, function(type) which(duration_types == type))
  names(sort(is_, FALSE))
}


to_dotted_types <- function(types, dots) {
  dts <- list()
  for (type in types) {
    dts_ <- sapply(dots, dot_type, type = type)
    dts <- append(dts, dts_)
  }
  unlist(dts)
}


to_axis <- function(types, dots) {
  l <- length(types)
  for (i in 1:l) {
    type <- types[i]
    if (type %in% duration_type_abbrs) {
      types[i] <- to_type.abbr(type)
    }
  }
  types <- sort.types(types)
  dots <- sort(dots, TRUE)
  dts <- to_dotted_types(types, dots)
  sapply(dts, function(dt) {
    to_value.Duration(to_Duration.notation(dt))
  })
}


#' @param axis_ A numeric vector in descending order.
split.value <- function(value, axis_, decreasing = TRUE, error = FALSE) {
  # recursively
  core <- function(value) {
    if (value %in% axis_) {
      return(value)
    } else if (value < axis_[length(axis_)]) {
      if (error) {
        stop()
      } else {
        return(value)
      }
    } else {
      if (value > axis_[1]) {
        k <- 1
      } else {
        ks <- which(axis_ > value)
        k <- ks[length(ks)] + 1
      }
      v <- axis_[k]
      if (decreasing) {
        return(c(v, core(value - v)))
      } else {
        return(c(core(value - v), v))
      }
    }
  }

  core(value)
}


to_Duration.value <- function(value, axis_ = to_axis(duration_types, 4:0)) {
  vs <- split.value(value, axis_, TRUE, TRUE)

  ds <- list()
  for (v in vs) {
    i <- which(axis_ == v)
    n <- names(axis_[i])
    d <- to_Duration.notation(n)
    ds[[length(ds) + 1]] <- d
  }

  if (length(ds) == 1) {
    return(ds[[1]])
  } else {
    class(ds) <- "TiedDurations"
    return(ds)
  }
}



# print -------------------------------------------------------------

to_string.Duration <- function(duration) {
  attr(MASS::fractions(to_value.Duration(duration)), "fracs")
}


#' @export
print.Duration <- function(duration) {
  s <- to_string.Duration(duration)
  cat(s, "\n")
}


to_string.TiedDurations <- function(durations) {
  ss <- sapply(durations, to_string.Duration)
  paste0("(", paste(ss, collapse = ", "), ")")
}


#' @export
print.TiedDurations <- function(durations) {
  s <- to_string.TiedDurations(durations)
  cat(s, "\n")
}


to_string.DurationLine <- function(duration_line) {
  l <- length(duration_line)
  if (l == 0) {
    s <- ""
  } else {
    for (i in 1:l) {
      d <- duration_line[[i]]
      if (class(d) == "Duration") {
        duration_line[[i]] <- to_string.Duration(d)
      } else {
        duration_line[[i]] <- to_string.TiedDurations(d)
      }
    }
    s <- paste(duration_line, collapse = ", ")
  }
  s
}


#' @export
print.DurationLine <- function(duration_line) {
  s <- to_string.DurationLine(duration_line)
  cat(s, "\n")
}



# Tupler ------------------------------------------------------------

validate.n <- function(n) {
  v <- class(n) %in% c("integer", "numeric") && length(n) == 1 &&
    n > 1 && as.integer(n) == n
  ifelse(is.na(v), FALSE, v)
}


validate.take_duration <- function(n, unit, take) {
  v_unit <- to_value.type(unit$type) * to_value.dot(unit$dot)
  v_take <- to_value.type(take$type) * to_value.dot(take$dot)
  v_unit * n >= v_take
}


#' @export
Tupler <- function(n, unit = "auto", take = unit) {
  # validate n
  if (!validate.n(n)) {
    stop('argument "n" should be a whole number larger than 1')
  }

  # validate unit
  if (!identical(unit, "auto")) {
    v_unit <- is.character(unit) && length(unit) == 1 &&
      validate.duration_notation(unit, tupler = FALSE)
    if (!v_unit) {
      m <- paste(
        'argument "unit" should be a character starting with',
        "a duration type followed by 0-4 dots"
      )
      stop(m)
    }
  }

  # validate take
  if (!identical(take, "auto")) {
    v_take <- is.character(take) && length(take) == 1 &&
      validate.duration_notation(take, tupler = FALSE)
    if (!v_take) {
      m <- paste(
        'argument "take" should be a character starting with',
        "a duration type followed by 0-4 dots"
      )
      stop(m)
    }
  }

  # normalize unit
  if (!identical(unit, "auto")) {
    d_unit <- to_Duration.notation(unit)
    unit <- list(type = d_unit$type, dot = d_unit$dot)
  }

  # normalize take
  if (!identical(take, "auto")) {
    d_take <- to_Duration.notation(take)
    take <- list(type = d_take$type, dot = d_take$dot)
  }

  # validate the duration of take
  if (!identical(unit, "auto") && !identical(take, "auto")) {
    if (!(validate.take_duration(n, unit, take))) {
      stop('the duration of "take" is too long')
    }
  }

  # convert take when it is "auto"
  if (!identical(unit, "auto") && identical(take, "auto")) {
    take <- unit
  }

  t_ <- list(n = n, unit = unit, take = take)
  class(t_) <- "Tupler"
  t_
}



# tuplet ------------------------------------------------------------

validate.unit_ratio <- function(type, dot, unit) {
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

  con
}


Duration <- function(notation, ...) {
  # validate notation
  v_n <- is.character(notation) && length(notation) == 1 &&
    validate.duration_notation(notation)
  if (!v_n) {
    stop("invalid duration notation")
  }

  # notation -> Duration
  d <- to_Duration.notation(notation)

  ts_ <- list(...)
  l <- length(ts_)

  # early return if ts_ is empty
  if (l == 0) {
    return(d)
  }

  tuplers <- d$tuplers

  # get initial type and dot
  m <- length(tuplers)
  if (m == 0) {
    type <- d$type
    dot <- d$dot
  } else {
    take <- tuplers[[m]]$take
    type <- take$type
    dot <- take$dot
  }

  # process Tuplers from ...
  for (i in 1:l) {
    t_ <- ts_[[i]]

    # not accept non-Tuplers now
    if (class(t_) != "Tupler") {
      stop("non-Tupler at position ", i)
    }

    n <- t_$n
    unit <- t_$unit
    take <- t_$take

    # convert "auto"
    if (identical(unit, "auto")) {
      t_$unit <- list(type = divide_type(type, n), dot = dot)
      # remember to re-assign unit
      unit <- t_$unit
      if (identical(take, "auto")) {
        t_$take <- unit
        # remember to re-assign take
        take <- t_$take
      } else if (!validate.take_duration(n, unit, take)) {
        stop('too long duration of "take" of Tupler at position ', i)
      }
    # validate unit
    } else if (!validate.unit_ratio(type, dot, unit)) {
      stop('invalid ratio of "unit" of Tupler at position ', i)
    }

    # reset type and dot
    type <- take$type
    dot <- take$dot

    tuplers[[m + 1]] <- unclass(t_)
    m <- m + 1
  }

  d$tuplers <- tuplers
  d
}



# DurationLine ------------------------------------------------------

#' @param durations A list of duration notations, values, Durations,
#' TiedDurations and lists of these data structures.
#' @return A list of Durations and TiedDurations.
DurationLine <- function(durations) {
  l <- length(durations)

  # empty list
  if (l == 0) {
    class(durations) <- "DurationLine"
    return(durations)
  }

  # error item positions
  is_ <- c()

  for (i in 1:l) {
    d <- durations[[i]]
    c_ <- class(d)
    l_ <- length(d)

    # pass lists and long vectors to TiedDurations
    if (c_ == "list" || (l_ > 1 && is.atomic(d))) {
      tryCatch(
        {durations[[i]] <- TiedDurations(d)},
        error = function(e) {is_ <<- c(is_, i)}
      )
    # convert duration notations
    } else if (c_ == "character" && validate.duration_notation(d)) {
      durations[[i]] <- to_Duration.notation(d)
    # convert values
    } else if (c_ %in% c("integer", "numeric") && l_ == 1) {
      tryCatch(
        {durations[[i]] <- to_Duration.value(d)},
        error = function(e) {is_ <<- c(is_, i)}
      )
    # keep Durations and TiedDurations untouched and deal with invalid items
    } else if (!(c_ %in% c("Duration", "TiedDurations"))) {
      is_ <- c(is_, i)
    }
  }

  # report invalid item positions
  l_is <- length(is_)
  if (l_is > 0) {
    if (l_is == 1) {
      m <- paste('invalid item of argument "durations" at position', is_)
    } else {
      m <- paste(
        'invalid items of argument "durations" at positions',
        paste(paste(is_[-l_is], collapse = ", "), "and", is_[l_is])
      )
    }
    stop(m)
  }

  # validate tuplet groups
  validate.tuplets(durations)

  class(durations) <- "DurationLine"
  durations
}


TiedDurations <- function(durations) {
  # add vectors to lists to make it easy for the below recursive function
  if (class(durations) != "list") {
    durations <- list(durations)
  }

  core <- function(durations) {
    if (identical(durations, list())) {
      stop()
    }

    d <- durations[[1]]
    c_ <- class(d)

    # notations
    if (c_ == "character" && all(validate.duration_notation(d))) {
      ds <- lapply(d, to_Duration.notation)
    # values
    } else if (c_ %in% c("numeric", "integer")) {
      ds <- list()
      for (d_ in d) {
        d_ <- to_Duration.value(d_)
        if (class(d_) == "TiedDurations") {
          ds <- append(ds, unclass(d_))
        } else {
          ds[[length(ds) + 1]] <- d_
        }
      }
    # Durations
    } else if (c_ == "Duration") {
      ds <- list(d)
    # TiedDurations
    } else if (c_ == "TiedDurations") {
      ds <- unclass(d)
    } else if (c_ == "list") {
      ds <- core(d)
    } else {
      stop()
    }

    durations <- durations[-1]
    if (identical(durations, list())) {
      return(ds)
    }
    append(ds, core(durations))
  }

  ds <- core(durations)
  if (length(ds) == 1) {
    return(ds[[1]])
  }
  class(ds) <- "TiedDurations"
  ds
}



# validate tuplets --------------------------------------------------

validate.similar_tuplets <- function(tuplet_1, tuplet_2) {
  # change "take" of the last tupler to "unit" at the same level,
  # then compare these two tuplets
  ds <- list(tuplet_1, tuplet_2)
  for (i in 1:2) {
    d <- ds[[i]]
    ts_ <- d$tuplers
    l <- length(ts_)
    t_ <- ts_[[l]]
    unit <- t_$unit
    ds[[i]]$tuplers[[l]]$take <- unit
  }
  identical(ds[[1]], ds[[2]])
}


#' @title Check If Tuplets in a DurationLine Can Form Groups
#' @details Used in \code{DurationLine}.
validate.tuplets <- function(duration_line) {
  # untie TiedDurations
  dl <- list()
  for (d in duration_line) {
    if (class(d) == "Duration") {
      dl[[length(dl) + 1]] <- d
    } else {
      dl <- append(dl, unclass(d))
    }
  }

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
      v <- to_value.tupler(t_)

      # remove the last level if it is complete
      if (v == 1) {
        dl[[j]]$tuplers[[l_]] <- NULL

      # if ds is empty, put d into it,
      # unless d is the last tuplet at current level,
      # since now it is not enough to form a group
      } else if (length(ks) == 0) {
        if (i == l) {
          stop()
        } else {
          ds[[length(ds) + 1]] <- d
          ks <- c(ks, j)
        }

      # d and items in ds should be of the same structure
      } else if (!validate.similar_tuplets(d, ds[[length(ds)]])) {
        stop()

      # check if d and ds form a group
      } else {
        ds[[length(ds) + 1]] <- d
        ks <- c(ks, j)
        # total value of the last tuplers of ds
        vs <- sum(sapply(ds, function(d) {
          to_value.tupler(d$tuplers[[l_]])
        }))

        if (vs > 1) {
          stop()

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
          stop()
        }
      }
    }

    dl[rs] <- NULL
    # re-measure depths
    depths <- sapply(dl, function(d) length(d$tuplers))
  }

  # for test
  dl
}
