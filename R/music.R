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



# + Meter, Key ------------------------------------------------------

BarAddOnLine <- function(x, add_on_class) {
  add_on_class %>%
    paste0("Line") %>%
    c("BarAddOnLine", "Printable") %>%
    `class<-`(x, .)
}


`+.BarAddOnLine` <- function(add_on_line, add_on) {
  constructor <- function(add_on_line) {
    add_on %>%
      class() %>%
      .[1] %>%
      BarAddOnLine(add_on_line, .)
  }

  l <- length(add_on_line)

  if (l == 0) {
    add_on_line[[1]] <- add_on
    add_on_line

  } else {
    b <- add_on$bar

    for (i in 1:l) {
      m <- add_on_line[[i]]
      b_i <- m$bar

      if (b_i > b) {
        add_on_line <- add_on_line %>%
          append(list(add_on), i - 1) %>%
          constructor()
        return(add_on_line)

      } else if (b_i == b) {
        add_on_line[[i]] <- add_on
        return(add_on_line)

      } else if (b_i < b) {
        if (i == l) {
          add_on_line <- add_on_line %>%
            append(list(add_on)) %>%
            constructor()
          return(add_on_line)

        } else {
          next
        }
      }
    }
  }
}


#' @keywords internal
#' @export
to_string.BarAddOnLine <- function(x, ...) {
  x %>%
    sapply(function(m) paste0("(", m$bar, ", ", to_string(m), ")")) %>%
    paste(collapse = ", ")
}


add.BarAddOn <- function(term, music) {
  name <- term %>%
    class() %>%
    .[1] %>%
    tolower() %>%
    paste0("s")

  if (is.null(music[[name]])) {
    music[[name]] <- BarAddOnLine(list(), class(term)[1])
  }

  music[[name]] <- music[[name]] + term
  music
}



# + Line ------------------------------------------------------------

add.Line <- function(term, music) {
  name <- term$name
  to <- term$to
  as <- term$as
  after <- term$after
  lns <- music$line_names
  parts <- music$parts
  l <- length(parts)

  check_add_line_name(name, lns)
  check_add_line_to(to, lns)

  music$line_names <- c(lns, name)

  # initialize `music$parts`
  if (l == 0) {
    music$parts <- list(list(list(term)))
    return(music)
  }

  # add to the end of `music`, if `to` is not specified
  if (is.null(to)) {
    if (as == "part") {
      music$parts[[l + 1]] <- list(list(term))

    } else {
      part <- music$parts[[l]]
      m <- length(part)

      if (as == "staff") {
        music$parts[[l]][[m + 1]] <- list(term)

      } else if (as == "voice") {
        staff <- part[[m]]
        n <- length(staff)
        music$parts[[l]][[m]][[n + 1]] <- term
      }
    }

    return(music)
  }

  # add to specified `to`
  for (i in 1:l) {
    part <- parts[[i]]
    m <- length(part)

    for (j in 1:m) {
      staff <- part[[j]]
      n <- length(staff)

      for (k in 1:n) {
        voice <- staff[[k]]
        voice_name <- voice$name

        if (to == voice_name) {

          if (as == "part") {
            music$parts <- append(
              parts,
              list(list(list(term))),
              ifelse(after, i, i - 1)
            )

          } else if (as == "staff") {
            music$parts[[i]] <- append(
              part,
              list(list(term)),
              ifelse(after, j, j - 1)
            )

          } else if (as == "voice") {
            check_voice_number(n, voice_name)

            music$parts[[i]][[j]] <- append(
              staff,
              list(term),
              ifelse(after, k, k - 1)
            )
          }

          return(music)
        }
      }
    }
  }
}



# validators

check_add_line_name <- function(name, line_names) {
  con <-
    name %in% line_names

  if (con) {
    glue::glue(
      "Each Line in a Music object must have a unique name, or no name.",
      "\n\n",
      '* Name "{name}" has been used.'
    ) %>% rlang::abort()
  }
}


check_add_line_to <- function(to, line_names) {
  con <-
    !is.null(to) &&
    !(to %in% line_names)

  if (con) {
    glue::glue(
      "If `to` is specified in the Line object,", " ",
      "it must refer to the name of a Line in the Music object.",
      "\n\n",
      '* Can\'t find Line with name "{to}".'
    ) %>% rlang::abort()
  }
}


check_voice_number <- function(voice_number, voice_name) {
  if (voice_number == 4) {
    glue::glue(
      "Each staff in a Music can have 4 voices at most.",
      "\n\n",
      '* Staff containing voice "{voice_name}" already has 4 voices.'
    ) %>% rlang::abort()
  }
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
