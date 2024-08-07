#' @keywords internal
#' @export
add.Tempo <- function(object, music) {
  # Normalization
  if (is.null(object[["bar"]])) object[["bar"]] <- 1L
  if (is.null(object[["offset"]])) object[["offset"]] <- 0

  # Construction
  music[["tempos"]] <- update_cases(music[["tempos"]], object)
  music
}


#' @keywords internal
#' @export
locate.Tempo <- function(object, ...) {
  c(object[["bar"]], object[["offset"]])
}
