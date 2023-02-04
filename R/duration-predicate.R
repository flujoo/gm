#' Check If Object Is Duration Notation
#'
#' @description A **duration notation** can be a string of tied
#' atomic duration notations, e.g. "q / 3 - w - half..".
#'
#' An **atomic duration notation** has two parts:
#'
#' 1. a base
#' 2. zero or more tuplet notations
#'
#' A **base** has two parts:
#'
#' 1. a **duration type** or its abbreviation
#' 2. zero to four dots
#'
#' A **tuplet notation** has two parts:
#'
#' 1. a "/" followed by a positive integer
#' 2. an optional "* (base / base)"
#'
#' @noRd
is_duration_notation <- function(x) {
  if (!is.character(x)) return(FALSE)

  re_type <- paste(c(duration_types$name, duration_types$abbr), collapse = "|")
  re_base <- paste0("(", re_type, ")", "\\.{0,4}")
  re_tuplet <- paste0(
    "(",
    # the basic tuplet notation, e.g. "/ 3"
    "\\s*", "/", "\\s*", "[1-9][0-9]*", "\\s*",
    "(",
    # for complex tuplets, e.g. "* (h / q)"
    "\\*", "\\s*", "\\(", "\\s*", re_base, "\\s*", "/",
    "\\s*", re_base, "\\s*", "\\)",
    ")?",
    ")*"
  )
  re_tied <- paste0(
    "^", "\\s*",
    re_base, re_tuplet,
    # tied duration notations
    "(", "\\s*", "-", "\\s*", re_base, re_tuplet, ")*",
    "\\s*", "$"
  )

  grepl(re_tied, x)
}


duration_types <- rbind(
  data.frame(name = character(), abbr = character(), value = double()),

  list(name = "maxima" , abbr = "m"   , value = 32   ),
  list(name = "long"   , abbr = "l"   , value = 16   ),
  list(name = "breve"  , abbr = "b"   , value = 8    ),
  list(name = "whole"  , abbr = "w"   , value = 4    ),
  list(name = "half"   , abbr = "h"   , value = 2    ),
  list(name = "quarter", abbr = "q"   , value = 1    ),
  list(name = "eighth" , abbr = "8"   , value = 1/2  ),
  list(name = "16th"   , abbr = "16"  , value = 1/4  ),
  list(name = "32nd"   , abbr = "32"  , value = 1/8  ),
  list(name = "64th"   , abbr = "64"  , value = 1/16 ),
  list(name = "128th"  , abbr = "128" , value = 1/32 ),
  list(name = "256th"  , abbr = "256" , value = 1/64 ),
  list(name = "512th"  , abbr = "512" , value = 1/128),
  list(name = "1024th" , abbr = "1024", value = 1/256)
)
