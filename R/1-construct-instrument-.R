#' Create `Instrument` Object
#'
#' Create an `Instrument` object to represent an instrument.
#'
#' Supported instruments:
#' `r document_items(instruments)`
#'
#' @param instrument A single integer between `1` and `128`, which indicates
#' the [program number
#' ](https://en.wikipedia.org/wiki/General_MIDI#Program_change_events)
#' of the instrument. See the *Details* section for all instruments.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the instrument.
#'
#' @param volume Optional. A single integer between `0` and `100`, which
#' represents the volume of the instrument. The default value is `80`.
#' Please note that `volume` and `pan` only work in MuseScore 3.
#'
#' @param pan Optional. A single integer between `-90` and `90`, which
#' represents the panning of the instrument. The default value is `0`.
#'
#' @returns A list of class `Instrument`.
#'
#' @seealso [gm::+.Music()] for adding an instrument to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a flute
#' flute <- Instrument(74, pan = -90)
#' flute
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C5", "D5", "E5", "F5")) + flute
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
  if (!is.null(volume)) erify::check_interval(volume, c(0, 100))
  if (!is.null(pan)) erify::check_interval(pan, c(-90, 90))

  # Normalization
  midi <- as.integer(instrument)
  name <- instruments[midi]
  if (!is.null(volume)) volume <- as.double(volume)
  if (!is.null(pan)) pan <- as.double(pan)

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
