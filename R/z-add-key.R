#' @export
Key <- function(key, bar = NULL, to = NULL, scope = NULL) {
  # check arguments
  erify::check_content(key, -7:7)
  check_bar(bar)
  check_to(to)

  if (!is.null(scope)) {
    erify::check_content(scope, c("part", "staff"))
  }

  # normalize `scope`
  scope <- normalize_key_scope(scope, to)

  # create Key
  list(
    key = key,
    bar = bar,
    to = to,
    scope = scope
  ) %>% `class<-`("Key")
}


normalize_key_scope <- function(scope, to) {
  if (is.null(to)) {
    # always assign `NULL` to `scope`, if `to` is `NULL`
    NULL

  } else if (is.null(scope)) {
    # assign the default value
    "part"

  } else {
    scope
  }
}


#' @keywords internal
#' @export
quantify.Key <- function(x, ...) {
  x$key
}


#' @keywords internal
#' @export
signify.Key <- function(x, short = FALSE, ...) {
  steps <- c("F", "C", "G", "D", "A", "E", "B")
  i <- which(x$key == -7:7)

  major <- Pitch(steps[i %% 7 + 1], i %/% 7 - 1) %>% signify()

  if (short) {
    return(major)
  }

  minor <- Pitch(steps[(i + 3) %% 7 + 1], (i - 4) %/% 7) %>% signify()

  glue::glue("{major} major ({minor} minor)") %>% unclass()
}


#' @export
print.Key <- function(x, ...) {
  cat("Key", signify(x), "\n")

  # unpack
  bar <- x$bar
  to <- x$to
  scope <- x$scope

  # if to print each component
  print_bar <- !is.null(bar)
  print_to <- !is.null(to)

  # if to add enter
  if (print_bar || print_to) {
    cat("\n")
  }

  # `$bar`
  if (print_bar) {
    cat(glue::glue("* to be added at bar {bar}"), "\n")
  }

  # `$to` and `$scope`
  if (print_to) {
    cat(
      "* to be added only to the",
      scope,
      "containing Line",
      signify_to(to),
      "\n"
    )
  }
}


#' @keywords internal
#' @export
add.Key <- function(object, music) {
  to <- object$to
  lines <- music$lines

  check_to_exist(to, lines)

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
