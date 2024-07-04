#' @keywords internal
#' @export
add.Lyric <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines
  lyrics <- music$lyrics

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, music$notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line
  if (is.null(object$verse)) object$verse <- 1L

  # Construction
  to_remove <-
    lyrics$line == line &
    lyrics$verse == object$verse &
    lyrics$i == i

  music$lyrics <- rbind(lyrics[!(to_remove), ], to_case(object))
  music
}
