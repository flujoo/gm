#' @keywords internal
#' @export
add.Key <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines, "Key")

  global <- music$global
  bar <- normalize_bar(object$bar)
  line <- locate_line(to, lines)
  scope <- ifelse(is.na(line), NA_character_, object$scope)
  number <- generate_key_number(line, scope, lines)

  # remove the Key with the same `$bar` and number in `music$global`
  for (i in seq_len(nrow(global))) {
    if (!inherits(global$object[[i]], "Key") || global$bar[i] != bar) {
      next
    }

    number_i <- generate_key_number(global$line[i], global$scope[i], lines)

    if (all(number_i == number)) {
      global <- global[-i, ]
      break
    }
  }

  # add case
  notation <- signify(object, TRUE)
  value <- quantify(object)

  music$global <- tibble::add_case(
    global,
    object = list(object),
    notation = notation,
    value = value,
    bar = bar,
    line = line,
    scope = scope
  )

  music
}


generate_key_number <- function(line, scope, lines) {
  if (is.na(line)) {
    return(0)
  }

  number <- c(lines$part[line], lines$staff[line])

  if (scope == "part") {
    number[2] <- 0
  }

  number
}
