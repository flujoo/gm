#' @export
print.Line <- function(x, ...) {
  cat("Line\n\n")

  # `$notes`
  cat("* of notes:\n\n")
  print(x$notes)

  # unpack
  name <- x$name
  bar <- x$bar
  offset <- x$offset
  as <- x$as
  to <- x$to
  after <- x$after

  if (!(is.null(c(name, bar, offset, as, to, after)))) {
    cat("\n")
  }

  # `$name`
  if (!is.null(name)) {
    cat(glue::glue('* of name "{name}"'), "\n", sep = "")
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

  # `$as`
  as <- x$as
  if (!is.null(as)) {
    cat(glue::glue("* as a {as}"), "\n", sep = "")
  }

  # `$to` and `$after`
  to <- x$to
  after <- x$after
  s_after <- ifelse(is.null(after) || after == TRUE, "after", "before")

  if (!is.null(to)) {
    if (is.character(to)) {
      cat(
        glue::glue('* to be inserted {s_after} Line "{to}"'), "\n", sep = "")
    } else if (is.numeric(to)) {
      cat(glue::glue("* to be inserted {s_after} Line {to}"), "\n", sep = "")
    }

  } else if (!is.null(after)) {
    # if `$to` is `NULL`, always insert the Line AFTER the last Line,
    # whatever `$after` is
    cat("* to be inserted after the last Line\n")
  }
}
