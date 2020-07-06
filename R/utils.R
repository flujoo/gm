#' @title Convert Vector into Formatted String
#'
#' @description Used in printing hierarchical music object like Pitch.
to_string <- function(v, width = 70, left = "(", right = ")",
                      compact = TRUE, space = " ", comma = ",",
                      indent_short_form = "", end_short_form = "\n",
                      indent_long_form = "auto",
                      indent_bracket = "", enter_bracket = "\n",
                      end_long_form = "\n", enter_new_line = "\n") {
  # short form
  s <- paste0(
    indent_short_form, left,
    paste0(v, collapse = paste0(comma, space)),
    right, end_short_form
  )

  # long form
  if (nchar(s) > width) {
    l <- length(v)

    if (indent_long_form == "auto") {
      indent_long_form <- ifelse(
        compact,
        paste(rep(" ", nchar(left)), collapse = ""),
        paste(rep(" ", nchar(left) + nchar(indent_bracket) + 1),
              collapse = "")
      )
    }

    if (compact) {
      v[l] <- paste0(v[l], right)
      # indent for 1st line
      indent_1 <- paste0(
        substr(indent_long_form, 1 + nchar(left), nchar(indent_long_form)),
        left
      )
    } else {
      indent_1 <- indent_long_form
    }

    # concatenate items one by one
    s <- new_line <- paste0(indent_1, v[1], comma)
    for (i in 2:l) {
      s_i <- v[i]
      end_ <- ifelse(i == l, "", comma)
      new_line_tmp <- paste0(new_line, space, s_i, end_)
      if (nchar(new_line_tmp) > width) {
        s <- paste0(s, enter_new_line, indent_long_form, s_i, end_)
        new_line <- paste0(indent_long_form, s_i, end_)
      } else {
        s <- paste0(s, space, s_i, end_)
        new_line <- paste0(new_line, space, s_i, end_)
      }
    }

    # concatenate brackets
    s <- ifelse(
      compact,
      paste0(s, end_long_form),
      paste0(
        indent_bracket, left, enter_bracket, s,
        enter_bracket, indent_bracket, right, end_long_form
      )
    )
  }

  s
}


to_string.list <- function(x, width = 70, left_outer = "{", right_outer = "}",
                           left_inner = "[", right_inner = "]") {
  f <- function(v) {
    to_string(v, width = width,
              left = left_inner, right = right_inner,
              indent_short_form = "  ", end_short_form = "",
              indent_long_form = "   ", end_long_form = "")
  }

  s <- paste0(
    left_outer, "\n",
    paste0(sapply(x, f), collapse = ",\n\n"),
    "\n", right_outer, "\n"
  )

  s
}
