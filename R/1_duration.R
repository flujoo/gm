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

  list(n = n, unit = unit, take = take)
}
