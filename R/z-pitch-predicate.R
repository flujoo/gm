# check if `x` is a MIDI note number between 12 and 127
# `NA` is not acceptable
# character MIDI note numbers, e.g. `"60"`, are acceptable
is_pitch_value <- function(x) {
  core <- function(x) {
    is.finite(x) && # also excludes `NA`
      x >= 12 &&
      x <= 127 &&
      x == as.integer(x)
  }

  if (is.numeric(x)) {
    core(x)

  } else if (is.character(x)) {
    tryCatch(
      { core(as.double(x)) },
      warning = function(w) FALSE
    )

  } else {
    FALSE
  }
}


# check if `x` is a pitch notation
# `NA` is not acceptable
is_pitch_notation <- function(x) {
  if (!is.character(x)) {
    return(FALSE)
  }

  reg <- paste0(
    "^",
    # a valid pitch notation always starts with a note name
    # either in uppercase or lowercase
    "([A-G]|[a-g])",
    # maybe followed by an accidental
    "(#{0,2}|-{0,2})",
    # followed by an octave
    "[0-9]",
    "$"
  )

  grepl(reg, x)
}
