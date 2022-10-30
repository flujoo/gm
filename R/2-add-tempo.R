#' @keywords internal
#' @export
add.Tempo <- function(object, music) {
  tempo <- to_case(object)
  music$tempos <- update_cases(music$tempos, tempo)
  music
}


#' @keywords internal
#' @export
to_case.Tempo <- function(object, ...) {
  bar <- object$bar
  offset <- object$offset

  # normalization
  if (is.null(bar)) bar <- NA_integer_
  if (is.null(offset)) offset <- NA_real_

  tempo <- data_frame(
    tempo = object$tempo,
    unit = object$unit,
    bpm = object$bpm,
    bar = bar,
    offset = offset
  )
  class(tempo) <- c(class(tempo), "tempo")
  tempo
}


#' @keywords internal
#' @export
locate.tempo <- function(case, ...) {
  bar <- case$bar
  offset <- case$offset

  if (is.na(bar)) bar <- 1L
  if (is.na(offset)) offset <- 0

  c(bar, offset)
}
