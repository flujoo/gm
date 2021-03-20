# Meter -------------------------------------------------------------------

#' @title Create `Meter` Object
#'
#' @description Create a `Meter` object.
#'
#' `Meter` objects represent time signatures.
#'
#' @param number A positive number to represent the upper numeral in a
#' time signature symbol, which indicates how many beats are contained in
#' each measure.
#'
#' @param unit 1, 2, 4, 8, 16, 32 or 64 to represent the lower numeral in
#' a time signature symbol, which indicates the duration of one beat.
#'
#' @param bar Optional. A positive integer which indicates the number of
#' the measure into which to insert the `Meter` object. By default, a `Meter`
#' object will be inserted into the first measure(s).
#'
#' @param actual_number,actual_unit Optional, which defines the actual
#' time signature rather than the time signature symbol on score. Usually
#' used to create pickup measures.
#'
#' @param invisible Optional. A single logical, which indicates whether to
#' show the time signature symbol on score.
#'
#' @return A list with class `Meter`.
#'
#' @seealso [mr::+.Music()] for adding `Meter` objects to a `Music` object.
#'
#' @examples
#' # create a 3/4 time signature
#' Meter(3, 4)
#'
#' # insert a time signature into a specific measure
#' Music() + Meter(3, 4, bar = 10)
#'
#' m <- Music() + Line(list("C5"), list(3))
#'
#' # specify the actual time signature
#' ts <- Meter(3, 4, actual_number = 1, actual_unit = 4)
#' ts
#' show(m + ts)
#'
#' # make a time signature invisible on score
#' ts <- Meter(3, 4, invisible = TRUE)
#' show(m + ts)
#' @export
Meter <- function(number, unit, bar = NULL, actual_number = NULL,
                  actual_unit = NULL, invisible = NULL) {
  # check arguments
  check_positive_integer(number)
  check_meter_unit(unit)

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  if (!is.null(actual_number)) {
    check_positive_integer(actual_number)
  }

  if (!is.null(actual_unit)) {
    check_meter_unit(actual_unit)
  }

  check_invisible(invisible)

  # normalize `actual_number` and `actual_unit`
  actual <- normalize_meter_actual(number, unit, actual_number, actual_unit)
  actual_number <- actual$actual_number
  actual_unit <- actual$actual_unit

  # create Meter
  list(
    number = number,
    unit = unit,
    bar = bar,
    actual_number = actual_number,
    actual_unit = actual_unit,
    invisible = invisible
  ) %>% `class<-`("Meter")
}


check_meter_unit <- function(unit) {
  name <- deparse(substitute(unit))

  check_type(unit, c("double", "integer"), name)
  check_length(unit, 1, name)
  check_content(unit, 2^(0:6), name)
}


check_invisible <- function(invisible) {
  if (!is.null(invisible)) {
    check_type(invisible, "logical")
    check_length(invisible, 1)
  }
}


normalize_meter_actual <- function(number, unit, actual_number, actual_unit) {
  # if only one actual is NULL, assign the corresponding nominal to it
  if (is.null(actual_number) && !is.null(actual_unit)) {
    actual_number <- number
  }

  if (!is.null(actual_number) && is.null(actual_unit)) {
    actual_unit <- unit
  }

  # if nominal and actual are the same, assign NULL to both actuals
  if (identical(number, actual_number) && identical(unit, actual_unit)) {
    actual_number <- NULL
    actual_unit <- NULL
  }

  # two actuals must both be NULL or not

  list(actual_number = actual_number, actual_unit = actual_unit)
}


#' @export
print.Meter <- function(x, context = "console", silent = FALSE, ...) {
  # nominal meter
  nominal <- paste0(x$number, "/", x$unit)

  # actual meter
  actual_number <- x$actual_number

  if (is.null(actual_number)) {
    actual <- NULL
  } else {
    actual <- paste0(" (", actual_number, "/", x$actual_unit, ")")
  }

  general <- paste0(nominal, actual)
  specifics <- character(0)

  # convert `x`
  if (context == "inside") {

  } else if (context == "console") {
    general <- paste("Meter", general)

    # convert `x$bar`
    bar <- x$bar
    if (!is.null(bar)) {
      specifics[[length(specifics) + 1]] <- "to be added at bar {bar}"
    }
  }

  s <- generate_string(general, specifics, environment())

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# Music + Meter -----------------------------------------------------------

MeterLine <- function() {
  list(meters = list()) %>% `class<-`("MeterLine")
}


#' @keywords internal
#' @export
`+.MeterLine` <- function(meter_line, meter) {
  meter %<>% normalize_key_bar()
  meter_line$meters %<>% merge_key(meter)
  # the above two utils are borrowed from key.R

  meter_line
}


#' @keywords internal
#' @export
print.MeterLine <- function(x, silent = FALSE, ...) {
  meters <- x$meters
  l <- length(meters)

  # empty form
  if (l == 0) {
    s <- ""

  # short form
  } else if (l == 1) {
    meter <- meters[[1]]
    bar <- meter$bar

    if (bar == 1) {
      s_bar <- NULL
    } else {
      s_bar <- " at bar {bar}"
    }

    s_meter <- print(meter, context = "inside", silent = TRUE)
    s <- paste0("Meter ", s_meter, s_bar) %>% glue::glue()

  # long form
  } else {
    general <- "Meters"

    specifics <- sapply(meters, function(meter) {
      meter %>%
        print(context = "inside", silent = TRUE) %>%
        paste("at bar {meter$bar}") %>%
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


#' @keywords internal
#' @export
add.Meter <- function(term, music) {
  ml <- music$meter_line

  if (is.null(ml)) {
    ml <- MeterLine()
  }

  music$meter_line <- ml + term
  music
}



# utils -------------------------------------------------------------------

#' @keywords internal
#' @export
to_value.Meter <- function(x, ...) {
  actual_unit <- x$actual_unit

  if (is.null(actual_unit)) {
    (4 / x$unit) * x$number

  } else {
    (4 / actual_unit) * x$actual_number
  }
}


# get the Meter for `bar`
# can be used as "find_key"
find_meter <- function(bar, meters) {
  l <- length(meters)

  for (i in 1:l) {
    meter <- meters[[i]]
    bar_i <- meter$bar

    if (bar > bar_i && i == l) {
      return(meter)

    } else if (bar == bar_i) {
      return(meter)

    } else if (bar < bar_i) {
      # there must be a Meter whose `$bar` is less than `bar`
      return(meters[[i - 1]])
    }
  }
}



# show --------------------------------------------------------------------

# merge MeterLine to each part
merge_meter_line <- function(lines, meters) {
  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    number <- line$number

    # skip non-part
    if (any(number[2:3] != c(1, 1))) {
      next
    }

    # unpack
    measures <- line$measures
    l <- length(measures)

    # merge Meters
    for (meter in meters) {
      bar <- meter$bar

      # break when bar is beyond `l`
      if (bar > l) {
        break
      }

      # check if there is an Attributes already
      a <- measures[[bar]]$notes[[1]]
      c_ <- class(a)

      if (c_ == "Attributes") {
        lines[[i]]$measures[[bar]]$notes[[1]]$attributes %<>%
          append(list(meter), 0)
      } else {
        lines[[i]]$measures[[bar]]$notes %<>%
          append(list(Attributes(list(meter))), 0)
      }
    }
  }

  lines
}


#' @keywords internal
#' @export
to_Element.Meter <- function(x, ...) {
  contents <- list(
    Element("beats", x$number),
    Element("beat-type", x$unit)
  )

  attributes <- NULL
  if (isTRUE(x$invisible)) {
    attributes <- list(`print-object` = "no")
  }

  Element("time", contents, attributes)
}
