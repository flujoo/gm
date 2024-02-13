#' @export
Pause <- function(type, i, to = NULL) {
  # Validation
  erify::check_content(type, c("breath", "caesura"))
  erify::check_n(i)
  check_to(to)

  # Normalization
  i <- as.integer(i)

  # Construction
  structure(
    list(to = to, i = i, type = type),
    class = "Pause"
  )
}


#' @export
print.Pause <- function(x, ...) {
  cat(switch(x$type, "breath" = "Breath Mark", "caesura" = "Caesura"))
  cat("\n\n")
  print_to_i_j(x$to, x$i)
}
