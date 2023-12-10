#' Print Component `to`, `i`, and `j`
#'
#' `to`, `i`, and `j` are components of most objects.
#'
#' @param scope For `Key`s.
#'
#' @param line A bool that indicates if `j` indicates the end position of
#' objects like `Hairpin`s, or the position in a chord.
#'
#' @noRd
print_to_i_j <- function(
    to = NULL,
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
