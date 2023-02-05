#' Check If Object Is Duration Notation
#'
#' @description A **duration notation** can be a string of tied
#' simple durations, e.g. "q/3 - w - half..".
#'
#' A **simple duration** has two parts:
#'
#' 1. a base
#' 2. zero or more tuplet ratios
#'
#' A **base** has two parts:
#'
#' 1. a **duration type** or its abbreviation
#' 2. zero to four dots
#'
#' A **tuplet ratio** has two parts:
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

  re_ratio <- paste0(
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
    re_base, re_ratio,
    # tied durations
    "(", "\\s*", "-", "\\s*", re_base, re_ratio, ")*",
    "\\s*", "$"
  )

  grepl(re, x)
}
