#' @keywords internal
#' @export
to_MusicXML.Key <- function(x, ...) {
  MusicXML(
    "key",
    MusicXML("fifths", x[["key"]]),
    list(number = x[["staff"]])
  )
}


#' @keywords internal
#' @export
insert.Key <- function(x, to, ...) {
  part <- x[["part"]]
  bar <- x[["bar"]]

  measures <- to[["contents"]][[part + 1]][["contents"]]
  if (length(measures) < bar) return(to)

  measure <- measures[[bar]]
  notes <- measure[["contents"]]
  first <- notes[[1]]

  if (first[["tag"]] == "attributes") {
    attributes <- first[["contents"]]

    to$contents[[part + 1]]$contents[[bar]]$contents[[1]]$contents <- append(
      attributes,
      list(to_MusicXML(x)),
      locate_attribute("key", attributes)
    )

  } else {
    to$contents[[part + 1]]$contents[[bar]]$contents <- append(
      notes,
      list(to_MusicXML(Attributes(x))),
      0
    )
  }

  to
}
