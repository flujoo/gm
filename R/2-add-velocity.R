#' @keywords internal
#' @export
add.Velocity <- function(object, music) {
  to <- object[["to"]]
  lines <- music[["lines"]]
  notes <- music[["notes"]]

  # Validation
  check_add_to(to, lines)

  line <- normalize_to(to, lines)
  i <- object[["i"]]
  j <- object[["j"]]

  check_i(i, line, notes)
  check_j(j, line, i, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object[["line"]] <- line

  . <- notes[["line"]] == line & notes[["i"]] == i
  if (NROW(notes[., ]) == 1) object[["j"]] <- NA_integer_

  # Construction
  music[["velocities"]] <- update_cases(music[["velocities"]], object)
  music
}


#' @keywords internal
#' @export
locate.Velocity <- function(object, ...) {
  location <- c(object[["line"]], object[["i"]], object[["j"]])
  location[is.na(location)] <- 0L
  location
}
