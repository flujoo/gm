#' @export
show.Music <- function(x, to = "score", width = "auto", ...) {
  # unpack `x`
  lines <- x$lines
  meter_line <- x$meter_line
  l <- length(lines)

  # check arguments
  check_show_lines(lines)
  check_show_meter_line(meter_line)
  check_show_to(to)
  check_show_width(width)

  # unpack `meters` from `meter_line`,
  # and add `$value` to each Meter in `meters` for further use
  meters <- meter_line$add_ons %>%
    add_meter_value()

  # normalize `as`, `bar` and `offset` in each Line
  lines <- normalize_lines(lines, meters, l)

  # check if there is any over-bar tuplet or group
  check_over_bar_tuplet(lines, meters, l)

  # normalize `key_lines`
  key_lines <- x$key_lines %>% normalize_key_lines()
}



# check arguments ---------------------------------------------------------

# check if `x$lines` is empty
check_show_lines <- function(lines) {
  if (is.null(lines)) {
    glue::glue(
      "To show the Music, it must contain at least one Line.",
      "\n\n",
      "* The Music contains no Line.",
      "\n",
      "* Use `+ Line()` to add a Line."
    ) %>% rlang::abort()
  }
}


# check if there is a Meter at bar 1
check_show_meter_line <- function(meter_line) {
  general <- "To show the Music, it must have a Meter at bar 1."
  hint <- "* Use `+ Meter()` to add a Meter."

  if (is.null(meter_line)) {
    glue::glue(
      general,
      "\n\n",
      "* The Music contains no Meter.",
      "\n",
      hint
    ) %>% rlang::abort()
  }

  if (meter_line$add_ons[[1]]$bar != 1) {
    glue::glue(
      general,
      "\n\n",
      "* The Music has no Meter at bar 1.",
      "\n",
      hint
    ) %>% rlang::abort()
  }
}


check_show_to <- function(to) {
  general <- '`to` must be "score", "audio", or both.'

  check_type(to, "character", general = general)
  check_length(to, 1:2, general = general)

  # check content
  l <- length(to)
  valid <- c("score", "audio")

  # the tricky part is that `to` can be "score", "audio" or both
  if (l == 1) {
    check_content(to, valid, general = general)

  } else if (l == 2) {
    ms <- character(0)

    for (i in 1:l) {
      to_i <- to[[i]]

      if (!(to_i %in% valid)) {
        ms[[length(ms) + 1]] <- '`to[{i}]` is "{to_i}"' %>%
          glue::glue() %>%
          unclass()
      }
    }

    if (length(ms) != 0) {
      ms %>%
        paste(collapse = ", ") %>%
        paste0("* ", ., ".") %>%
        glue::glue(general, "\n\n", .) %>%
        rlang::abort()
    }
  }
}


check_show_width <- function(width) {
  if (!identical(width, "auto")) {
    general <- '`width` must be a positive number or "auto".'

    check_type(width, c("integer", "double"), general = general)
    check_length(width, 1, general = general)
    check_content(width, expression(!is.na(x) && x > 0), general = general)
  }
}



# add value to each Meter -------------------------------------------------

# add `$value` to each Meter in `meters`
add_meter_value <- function(meters) {
  lapply(meters, function(meter) {
    meter$value <- to_value(meter)
    meter
  })
}



# normalize Lines ---------------------------------------------------------

normalize_lines <- function(lines, meters, l) {
  for (i in 1:l) {
    line <- lines[[i]]

    if (is.null(line$as)) {
      lines[[i]]$as <- "part"
    }

    if (is.null(line$bar)) {
      lines[[i]]$bar <- 1L
    }

    offset <- line$offset

    if (is.null(offset)) {
      lines[[i]]$offset <- 0

    # normalize over-bar offset
    } else {
      . <- normalize_bar_offset(lines[[i]]$bar, offset, meters)
      lines[[i]]$bar <- .$bar
      lines[[i]]$offset <- .$offset
    }
  }

  lines
}


normalize_bar_offset <- function(bar, offset, meters) {
  # get the value of the Meter for `bar`
  v <- find_bar_add_on(bar, meters)$value

  while (offset >= v) {
    bar <- bar + 1L
    offset <- offset - v
    v <- find_bar_add_on(bar, meters)$value
  }

  list(bar = bar, offset = offset)
}


find_bar_add_on <- function(bar, add_ons) {
  l <- length(add_ons)

  for (i in 1:l) {
    add_on <- add_ons[[i]]
    bar_i <- add_on$bar

    if (bar > bar_i && i == l) {
      return(add_on)

    } else if (bar == bar_i) {
      return(add_on)

    } else if (bar < bar_i) {
      # there must be an add-on at bar 1
      return(add_ons[[i - 1]])
    }
  }
}



# normalize `key_lines` ---------------------------------------------------

normalize_key_lines <- function(key_lines) {
  # when to add a global KeyLine
  con <- any(
    is.null(key_lines),
    length(key_lines) == 0,
    any(key_lines[[1]]$number != c(0, 0))
  )

  if (con) {
    # create the global KeyLine
    key_line <- KeyLine() + Key(0)
    key_line$number <- c(0L, 0L)

    # insert it
    key_lines %<>% append(list(key_line), 0)
  }

  # add `Key(0)` to any KeyLine containing no Key at bar 1
  l <- length(key_lines)

  for (i in 1:l) {
    key_line <- key_lines[[i]]

    if (key_line$add_ons[[1]]$bar != 1) {
      key_lines[[i]] <- key_line + Key(0)
    }
  }

  key_lines
}
