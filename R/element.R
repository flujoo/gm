Element <- function(tag, contents = NULL, attributes = NULL) {
  list(tag = tag, contents = contents, attributes = attributes) %>%
    `class<-`(c("Element", "Printable"))
}


#' @keywords internal
#' @export
to_string.Element <- function(x, ...) {
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

  x %>%
    to_semi_musicxml() %>%
    core() %>%
    paste(collapse = "\n")
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