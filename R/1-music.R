#' @export
Music <- function() {
  music <- list()
  class(music) <- "Music"
  music
}


#' @export
print.Music <- function(x, ...) {
  cat("Music", "\n")

  if (length(x) > 0) {
    cat("\n")
    print(unclass(x))
  }
}
