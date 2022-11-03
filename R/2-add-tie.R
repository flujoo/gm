#' @keywords internal
#' @export
normalize.Tie <- function(object, line, ...) {
  tie <- list(line = line, i = object$i, j = object$j)
  class(tie) <- "Tie"
  tie
}


