#' @title Add Delimiter at Beginning and End of Vector
#'
#' @description Paste the first delimiter to the left of the first item,
#' and the last delimiter to the right of the last item.
#'
#' @param v A vector.
#' @param delimiter A character vector of delimiter(s).
#'
#' @return A character vector.
delimit.vector <- function(v, delimiter) {
  d_l <- delimiter[1]
  d_r <- delimiter[length(delimiter)]
  v[1] <- paste0(d_l, v[1])
  l <- length(v)
  v[l] <- paste0(v[l], d_r)
  v
}


#' @title Delimit Some Items of List
#'
#' @description Apply \code{delimit.vector} to list items satisfying
#' certain condition.
#'
#' @param l A list of vectors.
#' @param condition A function taking a list item as input and
#' returning a boolean value.
#' @param delimiter A character vector of delimiter(s).
#'
#' @return A list in which some items are delimited.
delimit.list <- function(l, condition, delimiter) {
  for (i in 1:length(l)) {
    o <- l[[i]]
    if (condition(o)) {
      l[[i]] <- delimit.vector(o, delimiter)
    }
  }
  l
}


#' @title Convert Vector into Formatted String
#'
#' @description Used for printing hierarchical musical object like Pitch.
#'
#' @param v A vector.
#' @param delimiter A character vector of delimiter(s).
#' @param indent A string of spaces added before left delimiter.
#' @param end A string of enters added after right delimiter.
#' @param compact A logical indicating displaying style.
#'
#' @return A string.
to_string.vector <- function(v, delimiter, indent = "", end = "\n",
                             compact = TRUE) {
  # width for displaying string
  width <- 70
  # comma (or other symbol) to separate items
  comma = ","
  # space after comma
  space = " "

  d_l <- delimiter[1]
  d_r <- delimiter[length(delimiter)]

  # short form
  s <- paste0(
    indent, d_l,
    paste0(v, collapse = paste0(comma, space)),
    d_r, end
  )

  # long form
  if (nchar(s) > width) {
    l <- length(v)

    if (compact) {
      v[l] <- paste0(v[l], d_r)
      # indent for 1st line
      indent_1 <- paste0(indent, d_l)
      # space after indent
      indent_d <- paste0(rep(" ", nchar(d_l)), collapse = "")
      # indent for other lines
      indent_ <- paste0(indent, indent_d)
    } else {
      indent_d <- paste0(rep(" ", nchar(d_l) + 1), collapse = "")
      indent_1 <- indent_ <- paste0(indent, indent_d)
    }

    # core
    s <- new_line <- paste0(indent_1, v[1], comma)
    for (i in 2:l) {
      s_i <- v[i]
      # not add comma for last item
      comma_ <- ifelse(i == l, "", comma)
      new_line_tmp <- paste0(new_line, space, s_i, comma_)
      if (nchar(new_line_tmp) > width) {
        s <- paste0(s, "\n", indent_, s_i, comma_)
        new_line <- paste0(indent_, s_i, comma_)
      } else {
        s <- paste0(s, space, s_i, comma_)
        new_line <- paste0(new_line, space, s_i, comma_)
      }
    }

    # add delimiter
    s <- ifelse(
      compact,
      paste0(s, end),
      paste0(indent, d_l, "\n", s, "\n", indent, d_r, end)
    )
  }

  s
}


#' @title Convert List of Vectors into Formatted String
#'
#' @description Used for printing hierarchical musical object like Pitch.
#'
#' @param l A list of vectors.
#' @param delimiter_outer A character vector of delimiter(s) for outer
#' level.
#' @param delimiter_inner A character vector of delimiter(s) for inner
#' level.
#'
#' @return A string.
to_string.list <- function(l, delimiter_outer = c("{", "}"),
                           delimiter_inner = c("[", "]")) {
  d_l <- delimiter_outer[1]
  d_r <- delimiter_outer[length(delimiter_outer)]

  f <- function(v) {
    indent <- paste0(rep(" ", nchar(d_l) + 1), collapse = "")
    to_string.vector(v, delimiter_inner, indent, end = "")
  }

  paste0(
    d_l, "\n",
    paste0(sapply(l, f), collapse = ",\n\n"),
    "\n", d_r, "\n"
  )
}
