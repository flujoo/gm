normalize_bar <- function(bar) {
  ifelse(is.null(bar), 1L, as.integer(bar))
}
