#' Check If Object Is Duration Notation
#'
#' @description A **duration notation** has two parts:
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

  x <- gsub(" ", "", x)

  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_base <- paste0("(", re_type, ")", "\\.{0,4}")

  re_ratio <- paste0(
    "(",
    # divisor
    "/", "[1-9][0-9]*",
    # multiplier
    "(", "\\*", "\\(", re_base, "/", re_base, "\\)", ")?",
    ")*"
  )

  re <- paste0("^", re_base, re_ratio, "$")
  grepl(re, x)
}
