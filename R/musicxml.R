#' @title Create an "Element" Object
#'
#' @description The S3 class Element is to represent MusicXML elements.
#' An Element is a list of three components: tag, content, attribute.
#'
#' @param tag A character representing the tag name of the Element.
#' @param content An object representing the content of the Element.
#' It could be a character, an Element, a list of Elements,
#' or \code{NULL}, which indicates that the Element is empty.
#' @param attribute A list or \code{NULL} representing the Element's
#' attributes.
#'
#' @details Refer to
#' \href{https://usermanuals.musicxml.com/MusicXML/Content/EL-MusicXML.htm}{
#' MusicXML Documentation} for a complete list of MusicXML elements.
#'
#' @return An Element.
#'
#' @examples
#' (octave <- Element("octave", 4))
#'
#' # empty Element
#' (chord <- Element("chord"))
#'
#' # two-level Element
#' (pitch <- Element("pitch", list(
#'   Element("step", "G"),
#'   octave
#' )))
#'
#' # complex Element
#' note <- Element(
#'   "note",
#'   list(pitch, chord, Element("duration", 16)),
#'   list("default-x" = "10")
#' )
#' (measure <- Element("measure", note))
#' @export
Element <- function(tag, content = NULL, attribute = NULL) {
  element <- list(tag = tag, content = content, attribute = attribute)
  class(element) <- c("Element")
  element
}


to_SemiMusicXML <- function(object, ...) {
  UseMethod("to_SemiMusicXML")
}


#' @title Convert Element to SemiMusicXML
#'
#' @description SemiMusicXML is the intermediate product in the process
#' of converting an Element to a MusicXML object.
#'
#' @param object An Element.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details A SemiMusicXML has three parts: opening tag, content,
#' closing tag. If the Element's content is a character or \code{NULL},
#' these three parts are concatenated into a single character, which
#' looks like "<tag>content</tag>". Otherwise, a SemiMusicXML is a list
#' whose first and last items are tags, other items in between are other
#' SemiMusicXML objects.
#'
#' @return A SemiMusicXML object, which could be a character or a list.
#'
#' @examples
#' octave <- gm:::Element("octave", 4)
#' gm:::to_SemiMusicXML.Element(octave)
#'
#' # empty Element
#' chord <- gm:::Element("chord")
#' gm:::to_SemiMusicXML.Element(chord)
#'
#' # two-level Element
#' pitch <- gm:::Element("pitch", list(
#'   gm:::Element("step", "G"),
#'   octave
#' ))
#' gm:::to_SemiMusicXML.Element(pitch)
#'
#' # complex Element
#' note <- gm:::Element(
#'   "note",
#'   list(pitch, chord, gm:::Element("duration", 16)),
#'   list("default-x" = "10")
#' )
#' measure <- gm:::Element("measure", note)
#' gm:::to_SemiMusicXML.Element(measure)
to_SemiMusicXML.Element <- function(object, ...) {
  tag <- object$tag
  content <- object$content
  attribute <- object$attribute
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


#' @export
to_MusicXML <- function(object, ...) {
  UseMethod("to_MusicXML")
}


to_MusicXML.SemiMusicXML <- function(object, ...) {
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


#' @export
to_MusicXML.Element <- function(object, ...) {
  sm <- to_SemiMusicXML.Element(object)
  m <- to_MusicXML.SemiMusicXML(sm)
  m
}


#' @export
print.Element <- function(x, ...) {
  m <- to_MusicXML.Element(x)
  cat(m)
  invisible(m)
}
