#' @keywords internal
#' @export
add.Stem <- function(object, music) {
  to <- object[["to"]]
  i <- object[["i"]]
  lines <- music[["lines"]]
  notes <- music[["notes"]]

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, notes)
  check_i_rest(object, line, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object[["line"]] <- line

  # Construction
  music[["stems"]] <- update_cases(music[["stems"]], object)
  music
}


#' @keywords internal
#' @export
locate.Stem <- function(object, ...) {
  c(object[["line"]], object[["i"]])
}
