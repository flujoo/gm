#' @export
Instrument <- function(instrument, to, volume = NULL, pan = NULL) {
  # validation
  erify::check_interval(instrument, c(1L, 128L))
  check_to(to)
  if (!is.null(volume)) erify::check_interval(volume, c(0L, 127L))
  if (!is.null(pan)) erify::check_interval(pan, c(0L, 127L))

  # normalization
  midi <- as.integer(instrument)
  name <- instruments[midi]
  if (!is.null(volume)) volume <- as.integer(volume)
  if (!is.null(pan)) pan <- as.integer(pan)

  # construction
  instrument <- list(
    midi = midi,
    name = name,
    to = to,
    volume = volume,
    pan = pan
  )
  class(instrument) <- "Instrument"
  instrument
}


#' @export
print.Instrument <- function(x, ...) {
  to <- x$to
  volume <- x$volume
  pan <- x$pan

  cat(x$name, "\n\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to the part containing Line", s_to, "\n")

  if (!is.null(volume)) cat("* of volume", volume, "\n")
  if (!is.null(pan)) cat("* of pan", pan, "\n")
}
