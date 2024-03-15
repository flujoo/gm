#' @keywords internal
#' @export
add.Breath <- function(object, music) {
  to <- object[["to"]]
  i <- object[["i"]]
  lines <- music[["lines"]]

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, music[["notes"]])

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object[["line"]] <- line

  # Construction
  music[["breaths"]] <- update_cases(music[["breaths"]], object)
  music
}


#' @keywords internal
#' @export
locate.Breath <- function(object, ...) {
  c(object[["line"]], object[["i"]])
}
