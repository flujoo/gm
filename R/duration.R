#' @title Duration Types
#' @description Values to MusicXML "type" element.
#' @details See \url{https://usermanuals.musicxml.com/MusicXML/
#' Content/EL-MusicXML-type.htm}.
duration_types <- c(
  "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
  "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"
)


#' @title Convert Duration Type to Value
#' @param duration_type A character representing a duration type.
#' @return A numeric.
to_value.duration_type <- function(duration_type) {
  v_quarter = 1
  i_quarter <- which(duration_types == "quarter")
  i_type <- which(duration_types == duration_type)
  v_quarter * 2^(i_quarter - i_type)
}


#' @title Convert Dot to Value
#' @param dot A numeric between 0 and 4, or a character consisting of
#' 0 to 4 periods.
#' @return A numeric.
to_value.dot <- function(dot) {
  if (is.character(dot)) {
    dot <- nchar(dot)
  }
  sum(2^(-(0:dot)))
}


#' @title Validate Duration Notations
#' @param duration_notations A character vector or a list of
#' duration notations.
#' @param dot,tupletor,tie Bools indicating if duration notations can
#' contain dot, tupletor, and tie respectively.
#' @return A logical vector.
validate.duration_notations <- function(
  duration_notations, dot = TRUE, tupletor = TRUE, tie = TRUE) {
  reg <- paste0(
    "^",
    # always starts with a type
    paste0("(", paste(duration_types, collapse = "|"), ")"),
    # maybe followed by a dot block
    ifelse(dot, "(\\.{1,4})?", ""),
    # followed by 0-n tupletor notations
    ifelse(tupletor, "(/([2-9]|[1-9][0-9]+))*", ""),
    # maybe followed by a tie
    ifelse(tie, "-?", ""),
    "$"
  )
  grepl(reg, duration_notations)
}


#' @title Analyze Duration Notation
#' @description Split a duration notation into four parts representing
#' type, dot, tupletors and tie.
#' @param duration_notation A character representing a duration notation.
#' @return A list with \code{"type"}, \code{"dot"}, \code{"ns"} and
#' \code{"tie"} as names.
analyze.duration_notation <- function(duration_notation) {
  core <- function(reg) {
    ks <- gregexpr(reg, duration_notation)[[1]]
    out <- c()
    for (i in 1:length(ks)) {
      out_i <- substr(
        duration_notation,
        ks[i],
        attr(ks, "match.length")[i] + ks[i] - 1
      )
      out <- c(out, out_i)
    }
    out
  }

  list(
    type = core(paste(duration_types, collapse = "|")),
    dot = core("\\.{1,4}"),
    ns = as.numeric(sapply(
      strsplit(core("/[1-9][0-9]*"), "/"),
      function(x) x[2]
    )),
    tie = core("-")
  )
}


#' @title Infer Tuplet's Type
divide.type <- function(type, n) {
  i_type <- which(duration_types == type)
  d <- floor(log2(n))
  i <- i_type + d
  duration_types[i]
}


#' @title Validate Duration of Argument \code{take}
#' @description Used in function \code{Tupletor}.
validate.take <- function(n, unit_type, unit_dot, take_type, take_dot) {
  v_take <- to_value.duration_type(take_type) * to_value.dot(take_dot)
  v_unit <- to_value.duration_type(unit_type) * to_value.dot(unit_dot)
  v_take / v_unit <= n
}


#' @title Create Tupletor Object
#'
#' @description Create an object of S3 class "Tupletor". Used in function
#' \code{Duration} to specify complex tuplets.
#'
#' @param n A numeric which is an integer larger than 1.
#' @param unit A character representing a duration notation, indicating
#' the duration of the basic unit of the tuplets.
#' @param take A character representing a duration notation, indicating
#' the duration the tuplet actually takes.
#'
#' @return A list with \code{"n"}, \code{"unit"} and \code{"take"} as names,
#' whose class is \code{"Tupletor"}.
#'
#' @export
Tupletor <- function(n, unit, take = unit) {
  v_n <- n > 1 && as.integer(n) == n
  if (!v_n) {
    stop('argument "n" should be an integer larger than 1')
  }

  v_unit <- is.character(unit) &&
    length(unit) == 1 &&
    validate.duration_notations(unit, tupletor = FALSE, tie = FALSE)
  if (!v_unit) {
    m <- paste(
      'argument "unit" should be a character starting with',
      "a duration type followed by 0-4 dots"
    )
    stop(m)
  }

  if (take != unit) {
    v_take <- is.character(take) &&
      length(take) == 1 &&
      validate.duration_notations(take, tupletor = FALSE, tie = FALSE)
    if (!v_take) {
      m <- paste(
        'argument "take" should be a character starting with',
        "a duration type followed by 0-4 dots"
      )
      stop(m)
    }

    unit <- analyze.duration_notation(unit)
    unit_type <- unit$type
    unit_dot <- unit$dot

    take <- analyze.duration_notation(take)
    take_type <- take$type
    take_dot <- take$dot

  } else {
    unit <- analyze.duration_notation(unit)
    take_type <- unit_type <- unit$type
    take_dot <- unit_dot <- unit$dot
  }

  if (!validate.take(n, unit_type, unit_dot, take_type, take_dot)) {
    stop('the duration of "take" is too long')
  }

  t_ <- list(
    n = n,
    unit = c(unit_type, unit_dot),
    take = c(take_type, take_dot)
  )
  class(t_) <- "Tupletor"
  t_
}


#' @title Generate Tupletors from Numeric Vector
to_Tupletors.ns <- function(type, dot, ns) {
  t_ <- list()
  for (i in 1:length(ns)) {
    n <- ns[i]
    type <- divide.type(type, n)
    unit <- c(type, dot)
    t_i <- list(n = n, unit = unit, take = unit)
    class(t_i) <- "Tupletor"
    t_[[i]] <- t_i
  }
  t_
}


#' @title Validate \code{unit} Part of Tupletor
validate.unit <- function(type, dot, unit_type, unit_dot) {
  i_type <- which(duration_types == type)
  i_unit_type <- which(duration_types == unit_type)

  con <- unit_dot == dot && i_unit_type >= i_type
  if (dot == ".") {
    # 3/2
    con <- con ||
      unit_dot == "" && i_unit_type > i_type
  } else if (dot == "..") {
    # 7/4
    con <- con ||
      unit_dot == "" && i_unit_type > (i_type + 1)
  } else if (dot == "...") {
    # 15/8
    con <- con ||
      unit_dot == "." && i_unit_type > (i_type + 1)
  } else if (dot == "....") {
    # 31/16
    con <- con ||
      unit_dot == "" && i_unit_type > (i_type + 3)
  }

  con
}


#' @title Validate List of Dependent Tupletors
#' @description Stop if any of the Tupletors is invalid, or return nothing.
validate.Tupletors <- function(type, dot, tupletors) {
  l <- length(tupletors)
  if (l) {
    for (i in 1:length(tupletors)) {
      t_ <- tupletors[[i]]
      unit <- t_$unit
      unit_type <- unit[1]
      unit_dot <- unit[2]

      v <- validate.unit(type, dot, unit_type, unit_dot)
      if (!v) {
        if (l == 1) {
          stop("invalid Tupletor")
        } else {
          m <- paste("invalid Tupletor at position", i)
          stop(m)
        }
      }

      take <- t_$take
      type <- take[1]
      dot <- take[2]
    }
  }
}


#' @title Create Duration Object
#'
#' @description Duration is to represent the durational aspect of
#' musical note, rest, or chord.
#'
#' @param duration_notation A character representing a duration notation.
#' @param ... 0 or more Tupletor objects.
#'
#' @return A list with \code{"type"}, \code{"dot"}, \code{"tie"} and
#' \code{"tupletors"} as names, whose class is \code{"Duration"}.
#'
#' @export
Duration <- function(duration_notation, ...) {

  # validate duration_notation
  v_dn <- is.character(duration_notation) &&
    length(duration_notation) == 1 &&
    validate.duration_notations(duration_notation)
  if (!v_dn) {
    stop("invalid duration notation")
  }

  # split duration_notation
  dn <- analyze.duration_notation(duration_notation)
  type <- dn$type
  dot <- dn$dot
  ns <- dn$ns

  if (!is.na(ns[1])) {
    # tupletor notations
    tns <- to_Tupletors.ns(type, dot, ns)
    take <- tns[[length(tns)]]$take
    type <- take[1]
    dot <- take[2]
  } else {
    tns <- list()
  }

  # Tupletors
  tos <- list(...)
  validate.Tupletors(type, dot, tos)

  ts_ <- append(tns, tos)
  dn$ns <- NULL
  dn$tupletors <- ts_
  class(dn) <- "Duration"
  dn
}


#' @title Convert Tupletor to Value
to_value.Tupletor <- function(tupletor) {
  n <- tupletor$n
  v <- 1 / n

  unit <- tupletor$unit
  take <- tupletor$take
  if (!identical(unit, take)) {
    v_unit <- to_value.duration_type(unit[1]) * to_value.dot(unit[2])
    v_take <- to_value.duration_type(take[1]) * to_value.dot(take[2])
    v <- v * (v_take / v_unit)
  }

  v
}


#' @title Convert Duration to Value
to_value.Duration <- function(duration) {
  v_type <- to_value.duration_type(duration$type)
  v_dot <- to_value.dot(duration$dot)
  v <- v_type * v_dot

  ts_ <- duration$tupletors
  if (length(ts_)) {
    v_ts <- prod(sapply(ts_, to_value.Tupletor))
    v <- v * v_ts
  }

  v
}


#' @title Convert Duration to Printable String
to_string.Duration <- function(duration) {
  v <- to_value.Duration(duration)
  attr(MASS::fractions(v), "fracs")
}


#' @export
print.Duration <- function(x, ...) {
  s <- to_string.Duration(x)
  cat(s, "\n")
  invisible(s)
}


#' @title Convert Dot to Elements
to_Elements.dot <- function(dot) {
  if (is.character(dot)) {
    dot <- nchar(dot)
  }
  rep(list(Element("dot")), dot)
}


#' @title Get Tuplet's Ratios
#' @details Used in function \code{to_Elements.Duration}. Ratios are
#' related to MusicXML element "actual-notes" and "normal-notes"
#' in "time-modification".
get_ratios <- function(type, dot, tupletors, l) {
  rs <- list()
  for (i in 1:l) {
    tupletor <- tupletors[[i]]
    unit <- tupletor$unit
    unit_type <- unit[1]
    unit_dot <- unit[2]
    d <- (to_value.duration_type(type) * to_value.dot(dot)) /
      (to_value.duration_type(unit_type) * to_value.dot(unit_dot))
    rs[[i]] <- c(tupletor$n, d)
    take <- tupletor$take
    type <- take[1]
    dot <- take[2]
  }
  rs
}
