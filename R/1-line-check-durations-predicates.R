#' Check If Object Is Duration Notation
#' @noRd
is_duration_notation <- function(x) {
  if (!is.character(x)) return(FALSE)

  base <- paste0(
    "(",
    paste(c(duration_types$name, duration_types$abbr), collapse = "|"),
    ")",
    "\\.{0,4}"
  )
  tuplet <- paste0(
    "(",
    "\\s*", "/", "\\s*", "[1-9][0-9]*", "\\s*",
    "(",
    "\\*", "\\s*", "\\(", "\\s*", base, "\\s*", "/",
    "\\s*", base, "\\s*", "\\)",
    ")?",
    ")*"
  )
  notation <- paste0("^", "\\s*", base, tuplet, "\\s*", "$")
  grepl(notation, x)
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
