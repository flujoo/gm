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

  # minor key
  minor <- Pitch(
    steps[(i + 3) %% 7 + 1],
    (i - 4) %/% 7
  ) %>% to_string()

  general <- "{major} major ({minor} minor)"
  specifics <- character(0)

  # short form
  if (form == 0) {
    s <- generate_string(general, specifics, environment())
    return(s)
  }

  general <- paste("Key", general)

  # bar
  bar <- x$bar
  if (!is.null(bar)) {
    specifics[[length(specifics) + 1]] <- "to be added at bar {bar}"
  }

  # to
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
    s <- generate_string(general, specifics, environment())
    return(s)
  }
}



# KeyLine -----------------------------------------------------------------

KeyLine <- function() {
  kl <- list(
    number = NULL,
    add_ons = list()
  )

  cs <- c("KeyLine", "BarAddOnLine", "Printable")

  `class<-`(kl, cs)
}



# BarAddOnLine -> string --------------------------------------------------

#' @keywords internal
#' @export
to_string.BarAddOnLine <- function(x, ...) {
  add_ons <- x$add_ons
  l <- length(add_ons)

  if (l == 0) {
    return("")
  }

  # extract add on class
  c_ <- class(x)[1] %>%
    strsplit("Line") %>%
    .[[1]]

  # number
  number <- x$number

  if (is.null(number)) {
    s_number <- NULL
  } else {
    l_number <- length(number)
    s_number <- " for part {number[1]}"

    if (l_number == 2) {
      s_number <- paste(s_number, "staff {number[2]}")
    }
  }

  # short form
  if (l == 1) {
    add_on <- add_ons[[1]]
    bar <- add_on$bar

    if (bar == 1) {
      s_bar <- NULL
    } else {
      s_bar <- " at bar {bar}"
    }

    s <- add_ons[[1]] %>%
      to_string(form = 0) %>%
      paste0(c_, " ", ., s_bar, s_number) %>%
      glue::glue()

    return(s)
  }

  # long form
  general <- paste0(c_, "s", s_number)

  f <- function(add_on) {
    add_on %>%
      to_string(form = 0) %>%
      paste("at bar {add_on$bar}") %>%
      glue::glue() %>%
      unclass()
  }

  specifics <- sapply(add_ons, f)

  generate_string(general, specifics, environment())
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

  add_ons <- add_on_line$add_ons
  l <- length(add_ons)

  if (l == 0) {
    add_ons[[1]] <- add_on

  } else {
    b <- add_on$bar

    for (i in 1:l) {
      b_i <- add_ons[[i]]$bar

      # insert `add_on`
      if (b_i > b) {
        add_ons <- add_ons %>%
          append(list(add_on), i - 1)

      # replace the add on in `add_ons`
      } else if (b_i == b) {
        add_ons[[i]] <- add_on

      # append `add_on`
      } else if (b_i < b && i == l) {
        add_ons <- add_ons %>%
          append(list(add_on))
      }
    }
  }

  add_on_line$add_ons <- add_ons
  class(add_on_line) <- cs
  return(add_on_line)
}



# BarAddOnLine util -------------------------------------------------------

find_bar_add_on <- function(bar, add_on_line) {
  add_ons <- add_on_line$add_ons
  l <- length(add_ons)

  for (i in 1:l) {
    add_on <- add_ons[[i]]
    bar_i <- add_on$bar

    if (bar > bar_i && i == l) {
      return(add_on)

    } else if (bar == bar_i) {
      return(add_on)

    } else if (bar < bar_i) {
      return(add_ons[[i - 1]])
    }
  }
}
