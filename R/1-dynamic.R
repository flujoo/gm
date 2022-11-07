#' @export
Dynamic <- function(marking, to, i, velocity = NULL, above = NULL) {
  # validation
  erify::check_string(marking)
  check_to(to)
  erify::check_n(i)

  if (!is.null(velocity) && !is.na(velocity)) {
    erify::check_interval(velocity, c(0L, 127L))
  }

  if (!is.null(above)) erify::check_bool(above)

  # normalization
  i <- as.integer(i)
  velocity <- normalize_dynamic_velocity(velocity, marking)
  if (is.null(above)) above <- NA

  # construction
  dynamic <- list(
    marking = marking,
    to = to,
    i = i,
    velocity = velocity,
    above = above
  )
  class(dynamic) <- "Dynamic"
  dynamic
}


normalize_dynamic_velocity <- function(velocity, marking) {
  if (!is.null(velocity)) return(as.integer(velocity))

  # according to MuseScore
  switch(marking,
    "pppppp" = 1L,
    "ppppp" = 5L,
    "pppp" = 10L,
    "ppp" = 16L,
    "pp" = 33L,
    "p" = 49L,
    "mp" = 64L,

    "mf" = 80L,
    "f" = 96L,
    "ff" = 112L,
    "fff" = 126L,
    "ffff" = 127L,
    "fffff" = 127L,
    "ffffff" = 127L,

    "fp" = 96L,
    "pf" = 49L,
    "sf" = 112L,
    "sfz" = 112L,
    "sff" = 126L,
    "sffz" = 126L,
    "sfp" = 112L,
    "sfpp" = 112L,
    "rfz" = 112L,
    "rf" = 112L,
    "fz" = 112L,

    "m" = 96L,
    "r" = 112L,
    "s" = 112L,
    "z" = 80L,
    "n" = 49L,

    NA_integer_
  )
}


#' @export
print.Dynamic <- function(x, ...) {
  marking <- x$marking
  velocity <- x$velocity
  to <- x$to
  i <- x$i
  above <- x$above

  cat("Dynamic", marking, "\n")
  cat("\n")
  if (!is.na(velocity)) cat("* of velocity", velocity, "\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")
  cat("* to be added at position", i, "\n")

  if (!is.na(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}
