#' Create `Instrument` Object
#'
#' Create an `Instrument` object to represent an instrument.
#'
#' Supported instruments:
#' `r document_items(instruments)`
#'
#' @param instrument A single integer between `1` and `128`, which indicates
#' the instrument. See the *Details* section.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the instrument.
#'
#' @param volume Optional. A single integer between `0` and `127`, which
#' represents the volume of the instrument. The default value is `100`.
#'
#' @param pan Optional. A single integer between `0` and `127`, which
#' represents the panning of the instrument. The default value is `64`.
#'
#' @returns A list of class `Instrument`.
#'
#' @seealso [gm::+.Music()] for adding an instrument to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a piano
#' piano <- Instrument(1, 1)
#' piano
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + piano
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
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
