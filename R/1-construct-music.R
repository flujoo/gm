#' Initialize `Music` Object
#'
#' Initialize a `Music` object. Other components can be added to it.
#'
#' @returns A list of class `Music`.
#'
#' @seealso [gm::+.Music()] for adding components to a `Music` object.
#'
#' @export
#'
#' @examples
#' # Initialize a `Music`
#' Music()
Music <- function() {
  structure(list(), class = "Music")
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

  # Show components as data frames
  for (name in names(x)) {
    if (is.data.frame(x[[name]])) class(x[[name]]) <- cs
  }

  print(unclass(x))
}
