#' @export
Tie <- function(to, position, above = NULL) {
  # check arguments
  check_to(to)
  check_ij(position)

  if (!is.null(above)) {
    erify::check_bool(above)
  }

  # create Tie
  list(
    to = to,
    position = position,
    above = above
  ) %>% `class<-`("Tie")
}


# check `position` in `Tie()`
check_ij <- function(position) {
  general <-
    "`position` must be a numeric vector of one or two positive integers."

  erify::check_type(position, c("double", "integer"), NULL, general)
  erify::check_length(position, c(1, 2), NULL, general)

  if (length(position) == 1) {
    erify::check_n(position, general = general)
  } else {
    erify::check_contents(position, erify::is_n, NULL, general)
  }
}


#' @keywords internal
#' @export
signify.Tie <- function(x, ...) {
  type <- x$type

  if (!is.null(type)) {
    paste0("Tie ", "(", type, ")")

  } else {
    "Tie"
  }
}


#' @export
print.Tie <- function(x, ...) {
  cat(signify(x), "\n")

  # unpack
  to <- x$to
  position <- x$position
  above <- x$above

  # if to print each component
  print_to <- !is.null(to)
  print_position <- !is.null(position)
  print_above <- !is.null(above)

  # if to add enter
  if (print_to || print_position || print_above) {
    cat("\n")
  }

  # `$to`
  if (print_to) {
    cat("* to be added to Line", signify_to(to), "\n")
  }

  # `$position`
  if (print_position) {
    cat("* to be added at position", signify_position(position), "\n")
  }

  # `$above`
  if (print_above) {
    cat("* to be placed", ifelse(above, "above", "below"), "the notes\n")
  }
}
