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

  check_op_classes(
    class_left = c_m, class_right = c_t,
    valid_left = valid_left, valid_right = valid_right,
  )

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



# -> string ---------------------------------------------------------

#' @keywords internal
#' @export
to_string.Music <- function(x, ...) {
  tab <- "  "
  width <- 60
  ss <- character(0)

  # `x$parts` -> strings
  parts <- x$parts
  if (!is.null(parts)) {
    ss <- c("Parts:")
    for (part in parts) {
      ss <- c(ss, paste0(strrep(tab, 1), "Part:"))
      for (staff in part) {
        ss <- c(ss, paste0(strrep(tab, 2), "Staff:"))
        for (voice in staff) {
          s_v <- paste0("Voice ", '"', voice$name, '":')
          ss <- c(ss, paste0(strrep(tab, 3), s_v))

          s_ps <- voice$pitches %>%
            to_string() %>%
            shorten_string(width) %>%
            paste0(strrep(tab, 4), "Pitches: ", .)

          s_ds <- voice$durations %>%
            to_string() %>%
            shorten_string(width - 2) %>%
            paste0(strrep(tab, 4), "Durations: ", .)

          ss <- c(ss, s_ps, s_ds)
        }
      }
    }
  }

  # `x$meters` -> strings
  meters <- x$meters
  if (!is.null(meters)) {
    ss <- meters %>%
      to_string() %>%
      shorten_string(width) %>%
      paste0("\n", "Time Signatures: ", .) %>%
      c(ss, .)
  }

  # `x$keys` -> strings
  keys <- x$keys
  if (!is.null(keys)) {
    ss <- keys %>%
      to_string() %>%
      shorten_string(width + 1) %>%
      paste0("\n", "Key Signatures: ", .) %>%
      c(ss, .)
  }

  ss %>%
    paste(collapse = "\n")
}
