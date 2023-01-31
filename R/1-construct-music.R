#' @export
Music <- function() {
  music <- list()
  class(music) <- "Music"
  music
}


#' @export
print.Music <- function(x, ...) {
  cat("Music", "\n")
  if (length(x) == 0) return(invisible())
  cat("\n")

  if (requireNamespace("tibble", quietly = TRUE)) {
    cs <- class(tibble::tibble())
  } else {
    cs <- class(data.frame())
  }

  # show components as data frames
  for (name in names(x)) {
    if (is.data.frame(x[[name]])) class(x[[name]]) <- cs
  }

  print(unclass(x))
}
