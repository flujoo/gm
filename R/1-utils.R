check_to <- function(to) {
  if (is.null(to)) return(invisible())

  # for `to_j` in `Slur()`
  name <- deparse(substitute(to))

  general <- sprintf("`%s` must be a string or a positive integer.", name)
  erify::check_type(to, c("character", "double", "integer"), name, general)
  valid <- expression(erify::is_string(x) || erify::is_n(x))
  erify::check_content(to, valid, name, general)
}


print_to_ij <- function(to = NULL,
                        i = NULL,
                        j = NULL,
                        scope = NULL,
                        line = FALSE) {
  if (is.null(to) || is.na(to)) return(invisible())

  if (!is.null(scope)) scope <- sprintf("the %s containing", scope)
  if (is.character(to)) to <- sprintf('"%s"', to)
  cat("* to be added to", scope, "Line", to, "\n")

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
