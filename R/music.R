#' @export
Music <- function() {
  list() %>% `class<-`(c("Music", "Printable"))
}


#' @export
`+.Music` <- function(music, term) {
  c_m <- class(music)[1]
  c_t <- class(term)[1]
  valid_left <- "Music"
  valid_right <- c("Line", "Meter", "Key")

  check_binary_classes(c_m, c_t, valid_left, valid_right)

  # normalize argument order
  if (c_m %in% valid_right && c_t %in% valid_left) {
    . <- music
    music <- term
    term <- .
  }

  add(term, music)
}


add <- function(term, music) {
  UseMethod("add")
}



# Music -> string ---------------------------------------------------------

#' @keywords internal
#' @export
to_string.Music <- function(x, ...) {
  tab <- "  "
  width <- 75
  ss <- character(0)

  # generate a string and shorten it to proper length
  core <- function(x, n, start, end = NULL) {
    pre <- paste0(strrep(tab, n), start)
    w <- width - nchar(pre)
    x %>%
      to_string() %>%
      shorten_string(w) %>%
      paste0(pre, ., end)
  }

  # convert `x$parts`
  parts <- x$parts
  if (!is.null(parts)) {
    l <- length(parts)

    for (i in 1:l) {
      part <- parts[[i]]
      ss[length(ss) + 1] <- core(i, 0, "Part ", ":")

      # convert key line for `part`
      kl <- part$key_line
      if (!is.null(kl)) {
        ss[length(ss) + 1] <- core(kl, 1, "Key Signatures: ")
      }

      staffs <- part$staffs
      for (staff in staffs) {
        ss[length(ss) + 1] <- core("", 1, "Staff:")

        # convert key line for `staff`
        kl <- staff$key_line
        if (!is.null(kl)) {
          ss[length(ss) + 1] <- core(kl, 2, "Key Signatures: ")
        }

        voices <- staff$voices
        for (voice in voices) {
          ss[length(ss) + 1] <- core(voice$name, 2, 'Voice "', '":')
          ss[length(ss) + 1] <- core(voice$pitches, 3, "Pitches: ")
          ss[length(ss) + 1] <- core(voice$durations, 3, "Durations: ")
        }
      }

      # add an enter at the end of the string of `part`
      ss <- ss %>%
        paste(collapse = "\n") %>%
        paste0("\n")
    }
  }

  # convert `x$meter_line`
  meter_line <- x$meter_line
  if (!is.null(meter_line)) {
    ss[length(ss) + 1] <- core(meter_line, 0, "Time Signatures: ", "\n")

  }

  # convert `x$key_line`
  key_line <- x$key_line
  if (!is.null(key_line)) {
    ss[length(ss) + 1] <- core(key_line, 0, "Key Signatures: ")
  }

  ss %>%
    paste(collapse = "\n")
}
