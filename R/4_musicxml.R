Element <- function(tag, content = NULL, attribute = NULL) {
  e <- list(tag = tag, content = content, attribute = attribute)
  class(e) <- c("Element")
  e
}


to_SemiMusicXML.Element <- function(element) {
  tag <- element$tag
  content <- element$content
  attribute <- element$attribute
  cl <- class(content)

  if (is.null(attribute)) {
    attribute <- ""
  } else {
    attribute <- paste0(
      " ", names(attribute), '="', attribute, '"',
      collapse = ""
    )
  }

  if (is.null(content)) {
    sm <- paste0("<", tag, attribute, "/>")
  } else if (is.atomic(content)) {
    sm <- paste0("<", tag, attribute, ">", content, "</", tag, ">")
  } else if (cl == "Element") {
    sm <- list(
      paste0("<", tag, attribute, ">"),
      to_SemiMusicXML.Element(content),
      paste0("</", tag, ">")
    )
  } else if (cl == "list") {
    sm <- append(
      list(paste0("<", tag, attribute, ">"), paste0("</", tag, ">")),
      lapply(content, to_SemiMusicXML.Element),
      after = 1
    )
  }

  class(sm) <- "SemiMusicXML"
  sm
}


to_MusicXML.SemiMusicXML <- function(object) {
  # "\t"
  tab <- "  "

  # add "\t" recursively
  core <- function(object) {
    cl <- class(unclass(object))

    if (cl == "character") {
      return(object)
    } else if (cl == "list") {
      l <- length(object)
      # content, flattened
      content <- list()
      for (i in 2:(l - 1)) {
        e <- as.list(paste0(tab, core(object[[i]])))
        content <- append(content, e, length(content))
      }
      # insert content between tags
      m <- append(
        # opening and closing tag
        list(object[[1]], object[[l]]),
        content,
        after = 1
      )
    }

    m
  }

  m <- core(object)
  # add "\n"
  m <- lapply(m, function(sm) paste0(sm, "\n", collapse = ""))
  m <- paste0(m, collapse = "")
  class(m) <- "MusicXML"
  m
}


to_MusicXML.Element <- function(element) {
  sm <- to_SemiMusicXML.Element(element)
  m <- to_MusicXML.SemiMusicXML(sm)
  m
}


#' @export
print.Element <- function(element) {
  m <- to_MusicXML.Element(element)
  cat(m)
}
