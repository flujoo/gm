#' @export
Meter <- function(number, unit, bar = 1,
                  actual_number = NULL, actual_unit = NULL) {
  check_n(number, "number")
  check_meter_unit(unit)
  check_n(bar, "bar")

  if (is.null(actual_number)) {
    actual_number <- number
  } else {
    check_n(actual_number, "actual_number")
  }

  if (is.null(actual_unit)) {
    actual_unit <- unit
  } else {
    check_meter_unit(actual_unit, "actual_unit")
  }

  list(
    number = number,
    unit = unit,
    bar = bar,
    actual_number = actual_number,
    actual_unit = actual_unit
  ) %>% `class<-`(c("Meter", "Printable"))
}


check_meter_unit <- function(unit, name = "unit") {
  check_type(supplied = unit, valid = c("double", "integer"), name = name)
  check_length(supplied = unit, valid = 1, name = name, type = "numeric")
  check_content(supplied = unit, valid = 2^(0:6), name = name)
}


#' @keywords internal
#' @export
to_string.Meter <- function(x, ...) {
  n <- x$number
  u <- x$unit
  an <- x$actual_number
  au <- x$actual_unit

  s <- paste0(n, "/", u)

  if (n != an || u != au) {
    a <- paste0("(", an, "/", au, ")")
    s <- paste(s, a)
  }

  s
}



# MeterLine ---------------------------------------------------------

MeterLine <- function(meter_line) {
  c("MeterLine", "Printable") %>%
    `class<-`(meter_line, .)
}


`+.MeterLine` <- function(meter_line, meter) {
  l <- length(meter_line)

  if (l == 0) {
    meter_line[[1]] <- meter
    meter_line

  } else {
    b <- meter$bar

    for (i in 1:l) {
      m <- meter_line[[i]]
      b_i <- m$bar

      if (b_i > b) {
        meter_line <- meter_line %>%
          append(list(meter), i - 1) %>%
          MeterLine()
        return(meter_line)

      } else if (b_i == b) {
        meter_line[[i]] <- meter
        return(meter_line)

      } else if (b_i < b) {
        if (i == l) {
          meter_line <- meter_line %>%
            append(list(meter)) %>%
            MeterLine()
          return(meter_line)

        } else {
          next
        }
      }
    }
  }
}


#' @keywords internal
#' @export
to_string.MeterLine <- function(x, ...) {
  x %>%
    sapply(function(m) paste0("(", m$bar, ", ", to_string(m), ")")) %>%
    paste(collapse = ", ")
}
