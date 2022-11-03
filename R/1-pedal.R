#' @export
Pedal <- function(to, i, j) {
  # validation
  check_to(to)
  erify::check_n(i)
  erify::check_n(j)

  # normalization
  i <- as.integer(i)
  j <- as.integer(j)

  # construction
  pedal <- list(to = to, i = i, j = j)
  class(pedal) <- "Pedal"
  pedal
}


#' @export
print.Pedal <- function(x, ...) {
  to <- x$to
  i <- x$i
  j <- x$j

  cat("Pedal", "\n\n")
  cat(
    "* to be added to Line",
    if (is.character(to)) paste0('"', to, '"') else to,
    "\n"
  )
  cat("* from note", i, "to", j, "\n")
}
