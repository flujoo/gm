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


duration_types <- data.frame(
  name = c(
    "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
    "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"
  ),
  abbr = c(
    "m", "l", "b", "w", "h", "q", "8", "16", "32", "64", "128", "256",
    "512", "1024"
  ),
  value = 2^(6 - 1:14)
)
