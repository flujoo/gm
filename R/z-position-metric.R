check_bar <- function(bar) {
  if (is.null(bar)) {
    return(invisible())
  }

  erify::check_n(bar)
}


normalize_bar <- function(bar) {
  ifelse(is.null(bar), 1L, as.integer(bar))
}
