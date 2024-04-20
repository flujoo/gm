#' Create `Velocity` Object
#'
#' Create a `Velocity` object to set some notes' velocities.
#'
#' @param velocity A single integer between `0` and `127`, which indicates
#' the velocity to apply.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to apply the velocity. If not
#' provided, the velocity will be applied to all notes.
#'
#' @param i Optional. A single positive integer, which represents the
#' position of the velocity in a musical line.
#'
#' @param j Optional. A single positive integer, which represents the
#' position of the velocity in a chord.
#'
#' @returns A list of class `Velocity`.
#'
#' @seealso
#'
#' - [gm::+.Music()] for adding a `Velocity` to a `Music` object
#' - [gm::Dynamic()] for adding dynamic markings
#'
#' @export
#'
#' @examples
#' # Create a `Velocity`
#' velocity <- Velocity(10)
#' velocity
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + velocity
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Velocity <- function(velocity, to = NULL, i = NULL, j = NULL) {
  # Validation
  erify::check_interval(velocity, c(0L, 127L))
  check_to(to)
  check_velocity_i(i, to)
  check_velocity_j(j, i)

  # Normalization
  velocity <- as.integer(velocity)
  if (is.null(to)) to <- NA_integer_
  i <- if (!is.null(i)) as.integer(i) else NA_integer_
  j <- if (!is.null(j)) as.integer(j) else NA_integer_

  # Construction
  structure(
    list(to = to, i = i, j = j, velocity = velocity),
    class = "Velocity"
  )
}


check_velocity_i <- function(i, to) {
  if (is.null(i)) return(invisible())
  if (is.null(to)) erify::throw("Can't set `i` when `to` is unspecified.")
  erify::check_n(i)
}


check_velocity_j <- function(j, i) {
  if (is.null(j)) return(invisible())
  if (is.null(i)) erify::throw("Can't set `j` when `i` is unspecified.")
  erify::check_n(j)
}


#' @export
print.Velocity <- function(x, ...) {
  to <- x$to
  i <- x$i
  j <- x$j

  cat("Velocity", x$velocity, "\n\n")
  s_to <- if (is.character(to)) paste0('"', to, '"') else to

  if (!is.na(j)) {
    s_ij <- sprintf("(%s, %s)", i, j)
    cat("* to be applied to position", s_ij, "of Line", s_to, "\n")

  } else if (!is.na(i)) {
    cat("* to be applied to position", i, "of Line", s_to, "\n")

  } else if (!is.na(to)) {
    cat("* to be applied to Line", s_to, "\n")

  } else {
    cat("* to be applied to all notes", "\n")
  }
}
