# Key ---------------------------------------------------------------------

#' @export
Key <- function(key, bar = NULL, to = NULL, scope = NULL) {
  # check arguments
  check_key_key(key)

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  if (!is.null(to)) {
    check_name(to)
  }

  check_key_scope(scope, to)

  # normalize arguments
  key <- as.integer(key)

  if (!is.null(bar)) {
    bar <- as.integer(bar)
  }

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
    specifics[[length(specifics) + 1]] <-
      'to be added only to the {x$scope} containing Line "{to}"'
  }

  # long form
  if (form == 1) {
    generate_long_form(general, specifics, environment())
  }
}



# KeyLine -----------------------------------------------------------------

KeyLine <- function(key_line = list()) {
  key_line %>% `class<-`(c("KeyLine", "Printable"))
}



# KeyLine -> string -------------------------------------------------------

#' @keywords internal
#' @export
to_string.KeyLine <- function(x, ...) {
  if (length(x) == 1) {
    s <- x[[1]] %>% to_string(form = 0)
    return(s)
  }

  f <- function(key) {
    key %>%
      to_string(form = 0) %>%
      paste("at bar {key$bar}") %>%
      glue::glue() %>%
      unclass()
  }

  x %>%
    sapply(f) %>%
    paste(collapse = ", ")
}



# KeyLine + Key -----------------------------------------------------------

#' @keywords internal
#' @export
`+.KeyLine` <- function(key_line, key) {
  if (is.null(key$bar)) {
    key$bar <- 1L
  }

  merge_bar_add_on(key_line, key) %>%
    KeyLine()
}


# also used in `+.MeterLine`
merge_bar_add_on <- function(add_on_line, add_on) {
  l <- length(add_on_line)

  if (l == 0) {
    add_on_line[[1]] <- add_on

  } else {
    b <- add_on$bar

    for (i in 1:l) {
      b_i <- add_on_line[[i]]$bar

      if (b_i > b) {
        add_on_line <- add_on_line %>%
          append(list(add_on), i - 1)

      } else if (b_i == b) {
        add_on_line[[i]] <- add_on

      } else if (b_i < b && i == l) {
        add_on_line <- add_on_line %>%
          append(list(add_on))
      }
    }
  }

  return(add_on_line)
}
