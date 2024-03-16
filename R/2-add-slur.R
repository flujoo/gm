#' @keywords internal
#' @export
add.Slur <- function(object, music) {
  to <- object[["to"]]
  i <- object[["i"]]
  j <- object[["j"]]
  to_j <- object[["to_j"]]
  lines <- music[["lines"]]
  notes <- music[["notes"]]


  # Validation -------------------------------------------------

  check_add_to(to, lines, object)
  check_add_to(to_j, lines)

  line <- normalize_to(to, lines)
  line_j <- normalize_to(to_j, lines)

  check_i(i, line, notes)
  check_i(j, if (is.na(line_j)) line else line_j, notes)


  # Normalization ----------------------------------------------

  if (!is.na(line_j) && line_j == line) line_j <- NA_integer_

  if (is.na(line_j)) {
    . <- sort(c(i, j))
    object[["i"]] <- .[1]
    object[["j"]] <- .[2]

  } else if (line_j < line) {
    . <- line_j
    line_j <- line
    line <- .

    . <- i
    object[["i"]] <- j
    object[["j"]] <- .
  }

  names(object)[names(object) == "to"] <- "line"
  names(object)[names(object) == "to_j"] <- "line_j"
  object[["line"]] <- line
  object[["line_j"]] <- line_j


  # Construction -----------------------------------------------

  music[["slurs"]] <- update_cases(music[["slurs"]], object)
  music
}


#' @keywords internal
#' @export
locate.Slur <- function(object, ...) {
  c(object[["line"]], object[["i"]], object[["j"]], object[["line_j"]])
}
