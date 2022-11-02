#' Check If Index Exceeds Line Length
#' @noRd
check_i <- function(i, line, notes) {
  if (is.na(i)) return(invisible())

  # the length of the Line
  n <- max(notes[notes$line == line, ]$i)
  if (i <= n) return(invisible())

  general <- "`i` must not exceed the Line length."
  specifics <- sprintf("`i` is %s, while the Line length is %s.", i, n)
  erify::throw(general, specifics)
}
