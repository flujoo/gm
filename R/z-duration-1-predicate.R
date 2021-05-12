# duration types ----------------------------------------------------------

tabulate_duration_types <- function() {
  duration_types <- tibble::tibble(
    name = c(
      "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
      "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"),

    abbr = c(
      "m", "l", "b", "w", "h", "q", "8", "16", "32", "64", "128", "256",
      "512", "1024"),

    value = 1 * 2^(which(name == "quarter") - seq_along(name))
  )

  for (dot in 1:4) {
    duration_types[[paste0("value", strrep(".", dot))]] <-
      duration_types$value * quantify_dot(dot)
  }

  duration_types
}


# `dot` is between 0 and 4
quantify_dot <- function(dot) {
  sum(2^(-(0:dot)))
}


duration_types <- tabulate_duration_types()



# predicates --------------------------------------------------------------

is_duration_value <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  any(x == duration_types[-1:-2])
}


# if `tupler` is `FALSE`,
# the duration notation must not contain tupler notation
is_duration_notation <- function(x, tupler = TRUE) {
  if (!is.character(x)) {
    return(FALSE)
  }

  reg <- paste0(
    "^",
    # a valid duration notation always starts with a duration type
    # or its abbreviation
    paste0(
      "(",
      paste(c(duration_types$name, duration_types$abbr), collapse = "|"),
      ")"
    ),
    # maybe followed by a dot notation
    "(\\.{1,4})?",
    # maybe followed by some tupler notations
    ifelse(tupler, "(/([1-9][0-9]*))*", ""),
    "$"
  )

  grepl(reg, x)
}
