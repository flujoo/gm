# Key ---------------------------------------------------------------------

#' @export
Key <- function(key, bar = NULL, to = NULL, scope = NULL) {
  check_key(key)

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  if (!is.null(to)) {
    check_line_to(to)
  }

  check_key_scope(scope, to)

  scope <- normalize_key_scope(scope, to)

  list(key = key, bar = bar, to = to, scope = scope) %>% `class<-`("Key")
}


#' @export
print.Key <- function(x, context = "console", silent = FALSE, ...) {
  # convert `x$key`
  steps <- c("F", "C", "G", "D", "A", "E", "B")
  i <- which(x$key == -7:7)

  # major key
  major <- Pitch(steps[i %% 7 + 1], i %/% 7 - 1) %>% print(silent = TRUE)

  # minor key
  minor <- Pitch(steps[(i + 3) %% 7 + 1], (i - 4) %/% 7) %>%
    print(silent = TRUE)

  general <- "{major} major ({minor} minor)"
  specifics <- character(0)

  # convert `x`
  if (context == "inside") {

  } else if (context == "console") {
    general <- paste("Key", general)

    # convert `x$bar`
    bar <- x$bar
    if (!is.null(bar)) {
      specifics[[length(specifics) + 1]] <- "to be added at bar {bar}"
    }

    # convert `x$to`
    to <- x$to
    if (!is.null(to)) {
      s_to <- "to be added only to the {x$scope} containing"

      if (is.character(to)) {
        s_to <- paste(s_to, 'Line "{to}"')
      } else if (is.numeric(to)) {
        s_to <- paste(s_to, "Line {to}")
      }

      specifics[[length(specifics) + 1]] <- s_to
    }
  }

  s <- generate_string(general, specifics, environment())

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


check_key <- function(key) {
  check_type(key, c("double", "integer"))
  check_length(key, 1)
  check_content(key, -7:7, NULL, "`key` must be any integer between -7 and 7.")
}


check_key_scope <- function(scope, to) {
  # ignore `scope`, if it or `to` is `NULL`
  if (is.null(to) || is.null(scope)) {
    return(invisible(NULL))
  }

  check_type(scope, "character")
  check_length(scope, 1)
  check_content(scope, c("part", "staff"))
}


normalize_key_scope <- function(scope, to) {
  # always assign `NULL` to `scope`, if `to` is `NULL`
  if (is.null(to)) {
    NULL

  } else if (is.null(scope)) {
    "part"
    # default value

  } else {
    scope
  }
}



# KeyLine -----------------------------------------------------------------

KeyLine <- function() {
  list(keys = list(), number = NULL) %>% `class<-`("KeyLine")
}


#' @keywords internal
#' @export
`+.KeyLine` <- function(key_line, key) {
  key %<>% normalize_key_bar()
  key_line$keys %<>% merge_key(key)
  key_line
}


normalize_key_bar <- function(key) {
  if (is.null(key$bar)) {
    key$bar <- 1L
  }

  key
}


merge_key <- function(keys, key) {
  l <- length(keys)

  if (l == 0) {
    keys[[1]] <- key
    return(keys)
  }

  bar <- key$bar

  for (i in 1:l) {
    bar_i <- keys[[i]]$bar

    # insert `key`
    if (bar_i > bar) {
      keys %<>% append(list(key), i - 1)
      return(keys)

    # replace the Key in `keys`
    } else if (bar_i == bar) {
      keys[[i]] <- key
      return(keys)

    # append `key`
    } else if (bar_i < bar && i == l) {
      keys %<>% append(list(key))
      return(keys)
    }
  }
}


#' @keywords internal
#' @export
print.KeyLine <- function(x, silent = FALSE, ...) {
  # convert `x$number`
  number <- x$number
  number_1 <- number[1]
  number_2 <- number[2]

  if (is.null(number) || number_1 == 0) {
    s_number <- NULL
  } else {
    s_number <- " for part {number_1}"

    if (number_2 != 0) {
      s_number <- paste(s_number, "staff {number_2}")
    }
  }

  # convert `x`
  keys <- x$keys
  l <- length(keys)

  # empty form
  if (l == 0) {
    s <- ""

  # short form
  } else if (l == 1) {
    key <- keys[[1]]
    bar <- key$bar

    if (bar == 1) {
      s_bar <- NULL
    } else {
      s_bar <- " at bar {bar}"
    }

    s_key <- print(key, context = "inside", silent = TRUE)
    s <- paste0("Key ", s_key, s_bar, s_number) %>% glue::glue()

  # long form
  } else {
    general <- paste0("Keys", s_number)

    specifics <- sapply(keys, function(key) {
      key %>%
        print(context = "inside", silent = TRUE) %>%
        paste("at bar {key$bar}") %>%
        glue::glue() %>%
        unclass()
    })

    s <- generate_string(general, specifics, environment())
  }

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# Music + Key -------------------------------------------------------------

add.Key <- function(term, music) {
  lines <- music$lines
  l <- length(lines)

  # check `term$to`
  names <- lines %>%
    sapply(function(line) line$name) %>%
    unlist()
  to <- term$to
  check_line_to_exist(to, names, l)

  # get `number` of the targeted KeyLine
  number <- generate_key_line_number(lines, to, l, term$scope)

  # add `term`
  key_lines <- music$key_lines
  k <- locate_key_line(key_lines, number)

  if (is.na(k)) {
    key_line <- KeyLine() + term
    key_line$number <- number
    key_lines <- insert_key_line(key_lines, key_line, number)
  } else {
    key_lines[[k]] <- key_lines[[k]] + term
  }

  music$key_lines <- key_lines
  music
}


generate_key_line_number <- function(lines, to, l, scope) {
  if (is.null(to)) {
    c(0, 0)

  } else {
    number <- get_to_number(lines, to, l)

    if (scope == "part") {
      c(number[1], 0)
    } else if (scope == "staff") {
      number[1:2]
    }
  }
}


# get the index of the KeyLine with `number`,
# return NA if find no
locate_key_line <- function(key_lines, number) {
  Position(
    function(key_line) all(key_line$number == number),
    key_lines
  )
}


# used when `number` is not in the numbers of `key_lines`
insert_key_line <- function(key_lines, key_line, number) {
  if (is.null(key_lines)) {
    return(list(key_line))
  }

  p <- number[1]
  s <- number[2]

  f <- function(key_line) {
    number_i <- key_line$number
    p_i <- number_i[1]
    s_i <- number_i[2]

    (p_i == p && s_i < s) || p_i < p
  }

  k <- Position(f, key_lines, right = TRUE, nomatch = 0)
  append(key_lines, list(key_line), k)
}



# show --------------------------------------------------------------------

# normalize `key_lines` in `show.Music`
normalize_key_lines <- function(key_lines) {
  # add a global KeyLine to `key_lines`,
  con <- any(
    # when `key_lines` is empty,
    is.null(key_lines),
    length(key_lines) == 0,
    # or the first KeyLine is not global KeyLine
    any(key_lines[[1]]$number != c(0, 0))
  )

  if (con) {
    # create a global KeyLine
    key_line <- KeyLine() + Key(0)
    key_line$number <- c(0L, 0L)

    # insert it into `key_lines`
    key_lines %<>% append(list(key_line), 0)
  }

  # add `Key(0)` to any KeyLine that has no Key at bar 1
  for (i in 1:length(key_lines)) {
    key_line <- key_lines[[i]]

    if (key_line$keys[[1]]$bar != 1) {
      key_lines[[i]] <- key_line + Key(0)
    }
  }

  key_lines
}
