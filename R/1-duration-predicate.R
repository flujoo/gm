#' Check If Number Is Duration Value
#'
#' A **duration value** is a positive number that is a multiple of
#' the 1024th note.
#'
#' @noRd
is_duration_value <- function(number) {
  value_1024 <- 1/256

  is.finite(number) &
    number >= value_1024 &
    number %% value_1024 == 0
}


is_duration_notation <- function(string) {
  has_duration_notation_syntax(string) &&
    has_duration_notation_semantics(string)
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

    # Divisor
    "/", "[1-9][0-9]*",

    # Multiplier
    "(", "\\*", "\\(", re_base, "/", re_base, "\\)", ")?",

    ")*"
  )

  re <- paste0("^", re_base, re_ratio, "$")
  grepl(re, string)
}


#' Check If String Has Duration Notation Semantics
#'
#' @description `has_duration_notation_syntax()` only checks if a string
#' is syntactically a duration notation. This is not enough.
#'
#' For example, the syntactically valid tuplet notation "1024th/3"
#' implies a duration type shorter than the 1024th note,
#' which is not semantically valid.
#'
#' Therefore, `has_duration_notation_semantics()` checks further
#'
#' 1. if the duration base is a multiple of the 1024th note,
#' 2. if any duration type shorter than the 1024th note is implied,
#' 3. if any ratio has a value larger than 1, and
#' 4. if any unit can not divide its previous duration base.
#'
#' Please note that the duration bases in tuplet ratios do not have to
#' be multiples of the 1024th note. For example, "512./3*(1024./1024.)"
#' is valid, although it contains "1024.".
#'
#' @param string A string validated by `has_duration_notation_syntax()`.
#'
#' @noRd
has_duration_notation_semantics <- function(string) {
  duration <- parse_duration_notation(string)

  # Validation of being a multiple of the 1024th note
  if (to_value_duration_base(duration) %% (1/256) != 0) return(FALSE)

  type <- duration$type
  dot <- duration$dot

  for (ratio in duration$ratios) {
    # Validation of implied duration type
    implied_type <- divide_duration_type(type, ratio$n)
    if (is.na(implied_type)) return(FALSE)

    take <- ratio$take

    if (is.null(take)) {
      type <- implied_type

    } else {
      # Validation of ratio value
      if (to_value_ratio(ratio) > 1) return(FALSE)

      # Validation of divisibility
      remainder <- to_value_duration_base(list(type = type, dot = dot)) %%
        to_value_duration_base(ratio$unit)
      if (remainder != 0) return(FALSE)

      type <- take$type
      dot <- take$dot
    }
  }

  TRUE
}
