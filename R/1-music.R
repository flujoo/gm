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

  cs <- c(
    "Line", "Meter", "Key", "Clef", "Tempo",
    "Tie", "Instrument", "Dynamic", "Pedal"
  )

  # show components as data frames
  for (name in names(x)) class(x[[name]]) <- setdiff(class(x[[name]]), cs)
  print(unclass(x))
}
