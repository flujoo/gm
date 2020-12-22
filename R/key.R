# Key ---------------------------------------------------------------------

#' @export
Key <- function(key, bar = NULL, to = NULL, scope = NULL) {
  # check arguments
  check_key_key(key)

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  check_line_to(to)
  check_key_scope(scope, to)

  # normalize `scope`
  scope <- normalize_key_scope(scope, to)

  list(key = key, bar = bar, to = to, scope = scope) %>%
    `class<-`(c("Key", "Printable"))
}



# Key validators ----------------------------------------------------------

check_key_key <- function(key) {
  check_type(key, c("double", "integer"))
  check_length(key, 1)
  check_content(key, -7:7, NULL, "`key` must be any integer between -7 and 7.")
}


check_key_scope <- function(scope, to) {
  if (is.null(to) || is.null(scope)) {
    return(invisible(NULL))
  }

  check_type(scope, "character")
  check_length(scope, 1)
  check_content(scope, c("part", "staff"))
}



# Key normalizer ----------------------------------------------------------

normalize_key_scope <- function(scope, to) {
  # if `to` is not specified, ignore `scope`
  if (is.null(to)) {
    NULL

  # default value
  } else if (is.null(scope)) {
    "part"

  } else {
    scope
  }
}



# Key -> string -----------------------------------------------------------

#' @keywords internal
#' @export
to_string.Key <- function(x, form = 1, ...) {
  steps <- c("F", "C", "G", "D", "A", "E", "B")
  i <- which(x$key == -7:7)

  # major key
  major <- Pitch(
    steps[i %% 7 + 1],
    i %/% 7 - 1
  ) %>% to_string()

  # short form
  if (form == 0) {
    return(major)
  }

  # minor key
  minor <- Pitch(
    steps[(i + 3) %% 7 + 1],
    (i - 4) %/% 7
  ) %>% to_string()

  general <- "Key {major} major ({minor} minor)"
  specifics <- character(0)

  bar <- x$bar
  if (!is.null(bar)) {
    specifics[[length(specifics) + 1]] <- "to be added at bar {bar}"
  }

  to <- x$to
  if (!is.null(to)) {
    s_to <- "to be added only to the {x$scope} containing"

    if (is.character(to)) {
      s_to <- paste(s_to, 'Line with name "{to}"')
    } else if (is.numeric(to)) {
      to <- toOrdinal::toOrdinal(to)
      s_to <- paste(s_to, "the {to} Line")
    }

    specifics[[length(specifics) + 1]] <- s_to
  }

  # long form
  if (form == 1) {
    generate_string(general, specifics, environment())
  }
}



# KeyLine -----------------------------------------------------------------

KeyLine <- function(key_line = list()) {
  c("KeyLine", "BarAddOnLine", "Printable") %>%
    `class<-`(key_line, .)
}



# BarAddOnLine -> string --------------------------------------------------

#' @keywords internal
#' @export
to_string.BarAddOnLine <- function(x, ...) {
  # simplify the output, if `x` only has one add on, and
  # its `$bar` is NULL or 1
  if (length(x) == 1) {
    bar <- x[[1]]$bar

    if (is.null(bar) || bar == 1) {
      s <- x[[1]] %>% to_string(form = 0)
      return(s)
    }
  }

  f <- function(add_on) {
    add_on %>%
      to_string(form = 0) %>%
      paste("at bar {add_on$bar}") %>%
      glue::glue() %>%
      unclass()
  }

  x %>%
    sapply(f) %>%
    paste(collapse = ", ")
}



# BarAddOnLine + BarAddOn -------------------------------------------------

#' @keywords internal
#' @export
`+.BarAddOnLine` <- function(add_on_line, add_on) {
  # store classes of `add_on_line`
  cs <- class(add_on_line)

  # normalize `add_on$bar`
  if (is.null(add_on$bar)) {
    add_on$bar <- 1L
  }

  l <- length(add_on_line)

  if (l == 0) {
    add_on_line[[1]] <- add_on

  } else {
    b <- add_on$bar

    for (i in 1:l) {
      b_i <- add_on_line[[i]]$bar

      # insert `add_on`
      if (b_i > b) {
        add_on_line <- add_on_line %>%
          append(list(add_on), i - 1)

      # replace the add on in `add_on_line`
      } else if (b_i == b) {
        add_on_line[[i]] <- add_on

      # append `add_on`
      } else if (b_i < b && i == l) {
        add_on_line <- add_on_line %>%
          append(list(add_on))
      }
    }
  }

  class(add_on_line) <- cs
  return(add_on_line)
}



# BarAddOnLine util -------------------------------------------------------

find_bar_add_on <- function(bar, add_on_line) {
  l <- length(add_on_line)

  for (i in 1:l) {
    add_on <- add_on_line[[i]]
    bar_i <- add_on$bar

    if (bar > bar_i && i == l) {
      return(add_on)

    } else if (bar == bar_i) {
      return(add_on)

    } else if (bar < bar_i) {
      return(add_on_line[[i - 1]])
    }
  }
}
