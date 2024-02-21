Duration <- function(duration) {
  structure(duration, class = "Duration")
}


#' @keywords internal
#' @export
print.Duration <- function(x, ...) {
  cat(to_string(x), "\n")
}
