#' @export
Music <- function() {
  list() %>% `class<-`("Music")
}


#' @export
`+.Music` <- function(music, term) {
  c_l <- class(music)[1]
  c_r <- class(term)[1]
  cs_l <- "Music"
  cs_r <- c("Line", "Meter", "Key")

  check_binary_classes(c_l, c_r, cs_l, cs_r)

  # normalize argument order
  if (c_l %in% cs_r && c_r %in% cs_l) {
    . <- music
    music <- term
    term <- .
  }

  add(term, music)
}


add <- function(term, music) {
  UseMethod("add")
}


#' @export
print.Music <- function(x, ...) {
  ss <- "Music"

  lines <- x$lines
  if (!is.null(lines)) {
    for (i in 1:length(lines)) {
      ss[[length(ss) + 1]] <- lines[[i]] %>% print("inside", TRUE, i)
    }
  }

  meter_line <- x$meter_line
  if (!is.null(meter_line)) {
    ss[[length(ss) + 1]] <- meter_line %>% print(silent = TRUE)
  }

  key_lines <- x$key_lines
  if (!is.null(key_lines)) {
    ss <- key_lines %>%
      sapply(print, silent = TRUE) %>%
      c(ss, .)
  }

  ss %>%
    paste(collapse = "\n\n") %>%
    cat("\n")
}
