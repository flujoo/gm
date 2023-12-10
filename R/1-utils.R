#' Check Argument `to`
#'
#' `to` is used in most of the constructor functions.
#' It indicates a line, where an object will be added, by its number or name.
#'
#' @noRd
check_to <- function(to) {
  if (is.null(to)) return(invisible())

  # For `to_j` in `Slur()`
  name <- deparse(substitute(to))

  general <- sprintf("`%s` must be a string or a positive integer.", name)
  erify::check_type(to, c("character", "double", "integer"), name, general)
  valid <- expression(erify::is_string(x) || erify::is_n(x))
  erify::check_content(to, valid, name, general)
}


check_offset <- function(offset) {
  if (is.null(offset)) return(invisible())
  erify::check_type(offset, c("double", "integer"))
  erify::check_length(offset, 1)

  general <- paste(
    "`offset` must be a non-negative multiple of 1/256",
    "which is the shortest valid duration."
  )

  valid <- expression(x == 0 || is_duration_value(x))
  erify::check_content(offset, valid, NULL, general)
}


print_to_i_j <- function(to = NULL,
                        i = NULL,
                        j = NULL,
                        scope = NULL,
                        line = FALSE) {
  if (!is.null(to)) {
    if (is.na(to)) return(invisible())
    if (!is.null(scope)) scope <- sprintf("the %s containing", scope)
    if (is.character(to)) to <- sprintf('"%s"', to)
    cat("* to be added to", scope, "Line", to, "\n")
  }

  if (is.null(i)) return(invisible())

  if (line) {
    cat("* from position", i, "to", j, "\n")

  } else {
    if (!is.null(j) && !is.na(j)) i <- sprintf("(%s, %s)", i, j)
    cat("* to be added at position", i, "\n")
  }
}


print_bar_offset <- function(bar, offset) {
  if (is.null(bar) && is.null(offset)) return(invisible())

  if (is.null(offset)) {
    cat(sprintf("* to be added at bar %s", bar), "\n")

  } else {
    if (is.null(bar)) bar <- 1L
    cat(sprintf("* to be added at bar %s with offset %s", bar, offset), "\n")
  }
}


data_frame <- if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::tibble
} else {
  data.frame
}


#' @keywords internal
#' @export
to_string <- function(x, ...) {
  UseMethod("to_string")
}


#' @keywords internal
#' @export
to_value <- function(x) {
  UseMethod("to_value")
}
