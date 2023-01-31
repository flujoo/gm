print_bar_offset <- function(bar, offset) {
  if (is.null(bar) && is.null(offset)) return(invisible())

  if (is.null(offset)) {
    cat(sprintf("* to be added at bar %s", bar), "\n")

  } else {
    if (is.null(bar)) bar <- 1L
    cat(sprintf("* to be added at bar %s with offset %s", bar, offset), "\n")
  }
}
