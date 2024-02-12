#' Create `Mordent` Object
#'
#' Create a `Mordent` object to represent a mordent ornament.
#'
#' @param i A single positive integer, which represents the position
#' of the mordent in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the mordent.
#'
#' @param inverted Optional. A single logical, which indicates whether the
#' mordent is inverted or not. The default value is `FALSE`. See MusicXML
#' specification of [mordent](`r to_url("elements/mordent/")`) and
#' [inverted mordent](`r to_url("elements/inverted-mordent/")`).
#'
#' @param long Optional. A single logical, which indicates whether the
#' mordent is long or not. The default value is `FALSE`.
#'
#' @param ornament Optional. A single character, which can be `"left up"`,
#' `"left down"`, `"right up"`, or `"right down"`. It indicates the
#' direction of the mordent's left or right part.
#'
#' @returns A list of class `Mordent`.
#'
#' @seealso [gm::+.Music()] for adding a `Mordent` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a mordent
#' mordent <- Mordent(1)
#' mordent
#'
#' # Add it to a `Music`
#' music <- Music() + Line(c("C4", "D4")) + mordent
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Mordent <- function(
    i,
    to = NULL,
    inverted = NULL,
    long = NULL,
    ornament = NULL) {

  # Validation
  erify::check_n(i)
  check_to(to)
  if (!is.null(inverted)) erify::check_bool(inverted)
  if (!is.null(long)) erify::check_bool(long)
  check_mordent_ornament(ornament, long)

  # Normalization
  i <- as.integer(i)
  if (is.null(inverted)) inverted <- FALSE
  long <- normalize_mordent_long(long, ornament)
  if (is.null(ornament)) ornament <- NA_character_

  # Construction
  structure(
    list(
      to = to,
      i = i,
      inverted = inverted,
      long = long,
      ornament = ornament
    ),

    class = "Mordent"
  )
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
