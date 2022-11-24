#' @keywords internal
#' @export
add.Articulation <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines
  notes <- music$notes

  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)
  check_articulation_rest(i, line, notes)

  articulation <- normalize(object, line)
  music$articulations <- update_cases(music$articulations, articulation)
  music
}


check_articulation_rest <- function(i, line, notes) {
  if (is_not_rest(i, line, notes)) return(invisible())

  general <- "Can not add an Articulation to a rest."
  specifics <- sprintf("It is a rest at position %s of Line %s.", i, line)
  erify::throw(general, specifics)
}


#' @keywords internal
#' @export
normalize.Articulation <- function(object, line, ...) {
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  object
}


#' @keywords internal
#' @export
locate.Articulation <- function(object, ...) {
  c(object$line, object$i, object$name)
}
