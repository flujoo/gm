#' @export
Instrument <- function(instrument, to = NULL, volume = NULL, pan = NULL) {
  # Validation
  erify::check_interval(instrument, c(1L, 128L))
  check_to(to)
  if (!is.null(volume)) erify::check_interval(volume, c(0L, 127L))
  if (!is.null(pan)) erify::check_interval(pan, c(0L, 127L))

  # Normalization
  midi <- as.integer(instrument)
  name <- instruments[midi]
  if (!is.null(volume)) volume <- as.integer(volume)
  if (!is.null(pan)) pan <- as.integer(pan)

  # Construction
  structure(
    list(
      to = to,
      midi = midi,
      name = name,
      volume = volume,
      pan = pan
    ),

    class = "Instrument"
  )
}


#' @export
print.Instrument <- function(x, ...) {
  to <- x$to
  volume <- x$volume
  pan <- x$pan

  cat(x$name, "\n")
  if (!is.null(c(to, volume, pan))) cat("\n")

  print_to_i_j(x$to, scope = "part")
  if (!is.null(volume)) cat("* of volume", volume, "\n")
  if (!is.null(pan)) cat("* of pan", pan, "\n")
}
