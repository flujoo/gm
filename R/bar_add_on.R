# Meter/KeyLine -----------------------------------------------------

BarAddOnLine <- function(x, add_on_class) {
  add_on_class %>%
    paste0("Line") %>%
    c("BarAddOnLine", "Printable") %>%
    `class<-`(x, .)
}


#' @keywords internal
#' @export
to_string.BarAddOnLine <- function(x, ...) {
  x %>%
    sapply(function(m) paste0("(", m$bar, ", ", to_string(m), ")")) %>%
    paste(collapse = ", ")
}



# + Meter/Key -------------------------------------------------------

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



# find Meter/Key ----------------------------------------------------

find_BarAddOn <- function(bar, add_on_line) {
  l <- length(add_on_line)

  for (i in 1:l) {
    ao <- add_on_line[[i]]
    b <- ao$bar

    if (bar > b) {
      if (i == l) {
        return(ao)
      } else {
        next
      }

    } else if (bar == b) {
      return(ao)

    } else if (bar < b) {
      return(add_on_line[[i - 1]])
    }
  }
}
