print_bar_offset <- function(bar, offset, phrase = "added at") {
  s_bar <- paste("* to be", phrase, "bar %s")
  s_offset <- "with offset %s"

  if (!is.null(bar)) {
    if (is.null(offset)) {
      cat(sprintf(s_bar, bar), "\n")
    } else {
      cat(sprintf(paste(s_bar, s_offset, sep = " "), bar, offset), "\n")
    }

  } else if (!is.null(offset)) {
    bar <- 1
    cat(sprintf(paste(s_bar, s_offset, sep = " "), bar, offset), "\n")
  }
}
