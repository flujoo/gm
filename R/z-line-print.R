#' @export
print.Line <- function(x, ...) {
  cat("Line\n\n")

  # `$notes`
  cat("* of notes:\n\n")
  print(x$notes)
  cat("\n")

  # `$name`
  name <- x$name

  if (!is.null(name)) {
    cat(glue::glue('* of name "{name}"'), "\n")
  }

  # `$bar` and `$offset`
  bar <- x$bar
  offset <- x$offset
  s_bar <- "* to be inserted into bar {bar}"
  s_offset <- "with offset {offset}"

  if (!is.null(bar)) {
    if (is.null(offset)) {
      cat(glue::glue(s_bar), "\n")
    } else {
      cat(glue::glue(s_bar, s_offset, .sep = " "), "\n")
    }

  } else if (!is.null(offset)) {
    bar <- 1
    cat(glue::glue(s_bar, s_offset, .sep = " "), "\n")
  }

  # `$as`
  as <- x$as
  if (!is.null(as)) {
    cat(glue::glue("* as a {as}"), "\n")
  }

  # `$to` and `$after`
  to <- x$to
  after <- x$after
  s_after <- ifelse(is.null(after) || after == TRUE, "after", "before")

  if (!is.null(to)) {
    if (is.character(to)) {
      cat(glue::glue('* to be inserted {s_after} Line "{to}"'), "\n")
    } else if (is.numeric(to)) {
      cat(glue::glue("* to be inserted {s_after} Line {to}"), "\n")
    }

  } else if (!is.null(after)) {
    # if `$to` is `NULL`, always insert the Line AFTER the last Line,
    # whatever `$after` is
    cat("* to be inserted after the last Line", "\n")
  }
}
