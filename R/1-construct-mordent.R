#' @export
Mordent <- function(i,
                    to = NULL,
                    inverted = NULL,
                    long = NULL,
                    ornament = NULL) {
  # validation
  check_to(to)
  erify::check_n(i)
  if (!is.null(inverted)) erify::check_bool(inverted)
  if (!is.null(long)) erify::check_bool(long)
  check_mordent_ornament(ornament, long)

  # normalization
  i <- as.integer(i)
  if (is.null(inverted)) inverted <- FALSE
  long <- normalize_mordent_long(long, ornament)
  if (is.null(ornament)) ornament <- NA_character_

  # construction
  mordent <- list(
    to = to,
    i = i,
    inverted = inverted,
    long = long,
    ornament = ornament
  )
  class(mordent) <- "Mordent"
  mordent
}


check_mordent_ornament <- function(ornament, long) {
  if (is.null(ornament)) return(invisible())

  if (isFALSE(long)) {
    erify::throw("Can not set `ornament` when `long` is `FALSE`.")
  }

  ornaments <- c("left up", "left down", "right up", "right down")
  erify::check_content(ornament, ornaments)
}


normalize_mordent_long <- function(long, ornament) {
  if (!is.null(long)) return(long)
  if (is.null(ornament)) FALSE else TRUE
}


#' @export
print.Mordent <- function(x, ...) {
  ornament <- x$ornament

  if (x$long) cat("Long ")
  if (x$inverted) cat("Inverted ")
  cat("Mordent", "\n\n")

  if (!is.na(ornament)) {
    . <- strsplit(ornament, " ")[[1]]
    s_vertical <- switch(.[2], "up" = "an upward", "down" = "a downward")
    cat("* with", s_vertical, "ornament on the", .[1], "\n")
  }

  print_to_i_j(x$to, x$i)
}
