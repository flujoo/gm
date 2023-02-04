#' Check If Object Is Duration Notation
#'
#' @description A **duration notation** can be a string of tied
#' atomic duration notations, e.g. "q/3 - w - half..".
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
#' 1. a **divisor**, e.g. "/3"
#' 2. an optional **multiplier**, e.g. "*(q./w..)"
#'
#' @noRd
is_duration_notation <- function(x) {
  if (!is.character(x)) return(FALSE)

  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_base <- paste0("(", re_type, ")", "\\.{0,4}")

  re_tuplet <- paste0(
    "(",
    # divisor
    "\\s*", "/", "\\s*", "[1-9][0-9]*", "\\s*",
    "(",
    # multiplier
    "\\*", "\\s*", "\\(", "\\s*", re_base, "\\s*", "/",
    "\\s*", re_base, "\\s*", "\\)",
    ")?",
    ")*"
  )

  re <- paste0(
    "^", "\\s*",
    re_base, re_tuplet,
    # tied duration notations
    "(", "\\s*", "-", "\\s*", re_base, re_tuplet, ")*",
    "\\s*", "$"
  )

  grepl(re, x)
}
