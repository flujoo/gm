#' Check If Object Is Duration Value
#'
#' A **duration value** is a number not less than the 1024th note.
#'
#' @noRd
is_duration_value <- function(x) {
  x >= 0.00390625
}


is_duration_notation <- function(string) {
  has_duration_notation_syntax(string) &&
    (!grepl("/", string) || is_tuplet_notation(string))
}


#' Check If String Has Duration Notation Syntax
#'
#' @description A **duration notation** has two parts:
#'
#' 1. a duration base
#' 2. zero or more tuplet ratios
#'
#' A **duration base** has two parts:
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
has_duration_notation_syntax <- function(string) {
  string <- gsub(" ", "", string)

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
  grepl(re, string)
}


#' Check If Tuplet Notation Is Valid
#'
#' @description `has_duration_notation_syntax()` only checks the syntax of
#' a duration notation. This is not enough for tuplet notations.
#' For example, the syntactically valid tuplet notation "1024th/3"
#' implies a duration type shorter than the 1024th note,
#' which is not semantically valid.
#'
#' `is_tuplet_notation()` checks further
#'
#' 1. if any duration type shorter than the 1024th note is implied,
#' 2. if any ratio has a value larger than 1, and
#' 3. if any unit can not divide its previous duration base.
#'
#' @param notation A duration notation validated by
#' `has_duration_notation_syntax()`.
#'
#' @noRd
is_tuplet_notation <- function(notation) {
  duration <- parse_duration_notation(notation)
  type <- duration$type
  dot <- duration$dot

  for (ratio in duration$ratios) {
    take <- ratio$take

    if (is.null(take)) {
      type <- divide_duration_type(type, ratio$n)

      # validation of implied duration type
      if (is.na(type)) return(FALSE)

    } else {
      # validation of ratio value
      if (to_value_tuplet_ratio(ratio) > 1) return(FALSE)

      # validation of divisibility
      remainder <- to_value_duration_base(list(type = type, dot = dot)) %%
        to_value_duration_base(ratio$unit)

      if (remainder != 0) return(FALSE)

      type <- take$type
      dot <- take$dot
    }
  }

  TRUE
}
