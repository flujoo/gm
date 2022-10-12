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

  base <- paste0(
    # a duration type or its abbreviation
    "(",
    paste(c(duration_types$name, duration_types$abbr), collapse = "|"),
    ")",
    # 0-4 dots
    "\\.{0,4}"
  )
  tuplet <- paste0(
    "(",
    # the basic tuplet notation, e.g. "/ 3"
    "\\s*", "/", "\\s*", "[1-9][0-9]*", "\\s*",
    "(",
    # for complex tuplets, e.g. "* (h / q)"
    "\\*", "\\s*", "\\(", "\\s*", base, "\\s*", "/",
    "\\s*", base, "\\s*", "\\)",
    ")?",
    ")*"
  )
  notation <- paste0(
    "^", "\\s*",
    base, tuplet,
    # tied duration notations
    "(", "\\s*", "-", "\\s*", base, tuplet, ")*",
    "\\s*", "$"
  )

  grepl(notation, x)
}
