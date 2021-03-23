Element <- function(tag, contents = NULL, attributes = NULL) {
  list(tag = tag, contents = contents, attributes = attributes) %>%
    `class<-`("Element")
}


#' @keywords internal
#' @export
print.Element <- function(x, silent = FALSE, ...) {
  tab <- "  "

  # add `tab` recursively
  core <- function(x) {
    c_ <- class(x)

    if (c_ == "character") {
      x

    } else if (c_ == "list") {
      l <- length(x)

      # contents, flattened
      cs <- list()
      for (i in 2:(l - 1)) {
        e <- as.list(paste0(tab, core(x[[i]])))
        cs <- append(cs, e, length(cs))
      }

      # insert contents between tags
      append(
        # opening and closing tags
        list(x[[1]], x[[l]]),
        cs,
        after = 1
      )
    }
  }

  x %<>%
    to_semi_musicxml() %>%
    core() %>%
    paste(collapse = "\n")

  if (silent) {
    x
  } else {
    cat(x, "\n")
  }
}


to_semi_musicxml <- function(element) {
  tag <- element$tag
  contents <- element$contents
  attributes <- element$attributes

  cl <- class(contents)[1]

  if (is.null(attributes)) {
    attributes <- ""
  } else {
    attributes <- paste0(
      " ", names(attributes), '="', attributes, '"',
      collapse = ""
    )
  }

  if (is.null(contents)) {
    paste0("<", tag, attributes, "/>")

  } else if (is.atomic(contents)) {
    paste0("<", tag, attributes, ">", contents, "</", tag, ">")

  } else if (cl == "Element") {
    list(
      paste0("<", tag, attributes, ">"),
      to_semi_musicxml(contents),
      paste0("</", tag, ">")
    )

  } else if (cl == "list") {
    append(
      list(paste0("<", tag, attributes, ">"), paste0("</", tag, ">")),
      lapply(contents, to_semi_musicxml),
      after = 1
    )
  }
}


#' @keywords internal
#' @export
to_Element <- function(x, ...) {
  UseMethod("to_Element")
}


#' @keywords internal
#' @export
to_Element.Element <- function(x, ...) {
  x
}
