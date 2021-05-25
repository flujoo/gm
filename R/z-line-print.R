#' @export
print.Line <- function(x, ...) {
  cat("Line\n\n")

  # `$notes`
  cat("* of notes:\n\n")
  print(x$notes)

  # unpack
  name <- x$name
  as <- x$as
  to <- x$to
  after <- x$after
  bar <- x$bar
  offset <- x$offset

  # if to add enter
  if (!(is.null(c(name, as, to, after, bar, offset)))) {
    cat("\n")
  }

  # `$name`
  if (!is.null(name)) {
    cat(glue::glue('* of name "{name}"'), "\n", sep = "")
  }

  # `$as`
  if (!is.null(as)) {
    cat(glue::glue("* as a {as}"), "\n", sep = "")
  }

  # `$to` and `$after`
  s_after <- ifelse(is.null(after) || after == TRUE, "after", "before")

  if (is.character(to)) {
    s_to <- '* to be inserted {s_after} Line "{to}"'
    cat(glue::glue(s_to), "\n", sep = "")

  } else if (is.numeric(to)) {
    s_to <- "* to be inserted {s_after} Line {to}"
    cat(glue::glue(s_to), "\n", sep = "")

  } else if (!is.null(after)) {
    s_to <- "* to be inserted {s_after} the last Line in the score"
    cat(glue::glue(s_to), "\n", sep = "")
  }

  # `$bar` and `$offset`
  s_bar <- "* to be inserted into bar {bar}"
  s_offset <- "with offset {offset}"

  if (!is.null(bar)) {
    if (is.null(offset)) {
      cat(glue::glue(s_bar), "\n", sep = "")
    } else {
      cat(glue::glue(s_bar, s_offset, .sep = " "), "\n", sep = "")
    }

  } else if (!is.null(offset)) {
    bar <- 1
    cat(glue::glue(s_bar, s_offset, .sep = " "), "\n", sep = "")
  }
}
