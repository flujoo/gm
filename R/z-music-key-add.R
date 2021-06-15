#' @keywords internal
#' @export
add.Key <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines, "Key")

  keys <- music$keys

  # initialize `keys`
  if (is.null(keys)) {
    keys <- tibble::tibble(
      line = integer(),
      scope = character(),
      bar = integer(),
      key = list(),
      notation = character(),
      value = integer()
    )
  }

  bar <- normalize_bar(object$bar)
  line <- locate_line(to, lines)
  scope <- ifelse(is.na(line), NA_character_, object$scope)
  number <- generate_key_number(line, scope, lines)

  # remove the Key with the same `$bar` and number in `keys`
  for (i in seq_len(nrow(keys))) {
    if (keys$bar[i] != bar) {
      next
    }

    number_i <- generate_key_number(keys$line[i], keys$scope[i], lines)

    if (all(number_i == number)) {
      keys <- keys[-i, ]
      break
    }
  }

  # add case
  music$keys <- tibble::add_case(
    keys,
    line = line,
    scope = scope,
    bar = bar,
    key = list(object),
    notation = signify(object, TRUE),
    value = quantify(object)
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
