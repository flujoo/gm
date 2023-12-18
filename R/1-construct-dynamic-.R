#' Create `Dynamic` Object
#'
#' Create a `Dynamic` object to represent a dynamic marking.
#'
#' Common used dynamic markings and their velocities in MuseScore:
#' `r document_dynamics()`
#'
#' @param marking A single character, which represents the dynamic symbol on
#' the score. If `marking` is on the list in the *Details* section, and
#' `velocity` is not specified, the corresponding velocity on the list will
#' be used. Otherwise, `velocity` must be specified, or the `Dynamic` will
#' have no sound effect.
#'
#' @param i A single positive integer, which represents the position
#' of the `Dynamic` object in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the `Dynamic`.
#'
#' @param velocity Optional. A single integer between `0` and `127`,
#' which indicates the loudness of the `Dynamic`.
#'
#' @param above Optional. A single logical, which indicates whether the
#' dynamic symbol should appear above or below the staff.
#'
#' @returns A list of class `Dynamic`.
#'
#' @seealso [gm::+.Music()] for adding an `Dynamic` to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create an `Dynamic`
#' f <- Dynamic("f", 1)
#' f
#'
#' # Add it to a `Music`
#' music <- Music() + Line(c("C4", "D4")) + f
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Dynamic <- function(
    marking,
    i,
    to = NULL,
    velocity = NULL,
    above = NULL) {

  # Validation
  erify::check_string(marking)
  check_to(to)
  erify::check_n(i)

  if (!is.null(velocity) && !is.na(velocity)) {
    erify::check_interval(velocity, c(0L, 127L))
  }

  if (!is.null(above)) erify::check_bool(above)

  # Normalization
  i <- as.integer(i)
  velocity <- normalize_dynamic_velocity(velocity, marking)

  # Construction
  structure(
    list(
      to = to,
      i = i,
      marking = marking,
      velocity = velocity,
      above = above
    ),

    class = "Dynamic"
  )
}


normalize_dynamic_velocity <- function(velocity, marking) {
  if (!is.null(velocity)) return(as.integer(velocity))

  markings <- dynamics$marking

  if (marking %in% markings) {
    (dynamics$velocity)[which(markings == marking)]
  } else {
    NA_integer_
  }
}


#' @export
print.Dynamic <- function(x, ...) {
  velocity <- x$velocity
  above <- x$above

  cat("Dynamic", sprintf('"%s"', x$marking), "\n\n")
  if (!is.na(velocity)) cat("* of velocity", velocity, "\n")
  print_to_i_j(x$to, x$i)

  if (!is.null(above)) {
    s_above <- if (above) "above" else "below"
    cat("* to be placed", s_above, "the staff", "\n")
  }
}


document_dynamics <- function() {
  docs <- sprintf("- %s: %s", dynamics$marking, dynamics$velocity)
  paste(docs, collapse = "\n")
}
