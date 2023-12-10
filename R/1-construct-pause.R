#' @export
Pause <- function(type, i, to = NULL) {
  # validation
  erify::check_content(type, c("breath", "caesura"))
  check_to(to)
  erify::check_n(i)

  # normalization
  i <- as.integer(i)

  # construction
  pause <- list(to = to, i = i, type = type)
  class(pause) <- "Pause"
  pause
}


#' @export
print.Pause <- function(x, ...) {
  cat(switch(x$type, "breath" = "Breath Mark", "caesura" = "Caesura"))
  cat("\n\n")
  print_to_i_j(x$to, x$i)
}
