#' @export
Pause <- function(type, to, i) {
  # validation
  erify::check_content(type, c("breath", "caesura"))
  check_to(to)
  erify::check_n(i)

  # normalization
  i <- as.integer(i)

  # construction
  pause <- list(type = type, to = to, i = i)
  class(pause) <- "Pause"
  pause
}


#' @export
print.Pause <- function(x, ...) {
  type <- x$type
  to <- x$to
  i <- x$i

  s_type <- switch(type,
    "breath" = "Breath Mark",
    "caesura" = "Caesura"
  )

  cat(s_type, "\n\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")
  cat("* to be added at position", i, "\n")
}
