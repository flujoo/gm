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
    s_to <- "* to be added only to the {scope} containing Line"

    if (is.character(to)) {
      s_to %<>% paste('"{to}"')
    } else if (is.numeric(to)) {
      s_to %<>% paste("{to}")
    }

    cat(glue::glue(s_to), "\n")
  }
}
