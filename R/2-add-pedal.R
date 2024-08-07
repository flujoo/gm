#' @keywords internal
#' @export
add.Pedal <- function(object, music) {
  to <- object[["to"]]
  i <- object[["i"]]
  j <- object[["j"]]
  lines <- music[["lines"]]
  notes <- music[["notes"]]

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, notes)
  check_i(j, line, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object[["line"]] <- line

  . <- sort(c(i, j))
  object[["i"]] <- .[1]
  object[["j"]] <- .[2]

  # Construction
  music[["pedals"]] <- update_2d(music[["pedals"]], object)
  music
}
