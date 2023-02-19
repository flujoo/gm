#' @keywords internal
#' @export
add.Trill <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  lines <- music$lines
  notes <- music$notes

  # validation
  check_add_to(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)
  if (is.na(j)) check_i_rest(object, line, notes)
  check_i(j, line, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  if (!is.na(j)) {
    . <- sort(c(i, j))
    object$i <- .[1]
    object$j <- .[2]
  }

  # construction
  if (is.na(j) || j == i) music <- remove_ornaments(music, object)
  music$trills <- update_2d(music$trills, object)
  music
}
