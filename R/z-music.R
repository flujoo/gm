#' @export
Music <- function() {
  list() %>% `class<-`("Music")
}


#' @export
`+.Music` <- function(music, object) {
  cs <- c("Line", "Meter", "Key", "Clef", "Tempo", "Tie", "Meta")
  erify::check_binary_classes(music, object, "Music", cs, "+")

  # normalize argument order
  if (inherits(music, cs) && inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  add(object, music)
}


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}


#' @export
print.Music <- function(x, ...) {
  cat("Music", "\n")

  # arrange the order of printing the components of `x`
  ordered_names <- c("lines", "pitches", "durations", "meters", "keys")
  names <- names(x) %>% {.[order(match(., ordered_names))]}

  for (name in names) {
    cat("\n")
    cat(paste0("$", name), "\n\n")
    print(x[[name]])
  }
}
