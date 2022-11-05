check_dynamic_marking <- function(marking) {
  if (is.null(marking)) return(invisible())

  markings <- c(
    sapply(6:1, strrep, x = "p"),
    "mp", "mf",
    sapply(1:6, strrep, x = "f")
  )

  erify::check_content(marking, markings)
}


check_dynamic_marking_velocity <- function(marking, velocity) {
  if (!is.null(marking) || !is.null(velocity)) return(invisible())
  erify::throw("`marking` and `velocity` must not both be `NULL`.")
}


check_dynamic_i <- function(i, to) {
  if (is.null(i)) return(invisible())

  if (is.null(to)) {
    general <- "Only when `to` is specified, can `i` be set."
    specifics <- "`to` is `NULL`."
    erify::throw(general, specifics)

  } else {
    erify::check_n(i)
  }
}


check_dynamic_j <- function(j, i) {
  if (is.null(j)) return(invisible())

  if (is.null(i)) {
    general <- "Only when `i` is specified, can `j` be set."
    specifics <- "`i` is `NULL`."
    erify::throw(general, specifics)

  } else {
    erify::check_n(j)
  }
}


check_dynamic_scope <- function(scope, to, i, j) {
  if (is.null(scope)) return(invisible())

  if (!is.null(j)) {
    valid <- c("note", "chord", "voice", "staff", "part", "score")
    general <- sprintf(
      "When `j` is specified, `scope` must be %s.",
      erify::join(erify::back_quote(valid))
    )
    erify::check_content(scope, valid, NULL, general)

  } else if (!is.null(i)) {
    valid <- c("chord", "voice", "staff", "part", "score")
    general <- sprintf(
      "When `i` is specified, `scope` must be %s.",
      erify::join(erify::back_quote(valid))
    )
    erify::check_content(scope, valid, NULL, general)

  } else if (!is.null(to)) {
    valid <- c("voice", "staff", "part", "score")
    general <- sprintf(
      "When only `to` is specified, `scope` must be %s.",
      erify::join(erify::back_quote(valid))
    )
    erify::check_content(scope, valid, NULL, general)

  } else {
    general <- 'When `to` is not specified, `scope` can only be `"score"`.'
    erify::check_string(scope, NULL, general)
    if (scope == "score") return(invisible())
    specifics <- sprintf('`scope` is `"%s"`.', scope)
    erify::throw(general, specifics)
  }
}
