#' @param i The index of target `<part>`. For now, it's part number plus one.
#' @noRd
insert_attribute <- function(object, score, i, bar) {
  measures <- score[["contents"]][[i]][["contents"]]
  if (length(measures) < bar) return(score)

  measure <- measures[[bar]]
  notes <- measure[["contents"]]
  first <- notes[[1]]

  if (first[["tag"]] == "attributes") {
    attributes <- first[["contents"]]
    musicxml <- to_MusicXML(object)

    score$contents[[i]]$contents[[bar]]$contents[[1]]$contents <- append(
      attributes,
      list(musicxml),
      locate_attribute(musicxml[["tag"]], attributes)
    )

  } else {
    score$contents[[i]]$contents[[bar]]$contents <- append(
      notes,
      list(to_MusicXML(Attributes(object))),
      0
    )
  }

  score
}


#' @details `to_url("elements/attributes")`
#' @noRd
locate_attribute <- function(tag, attributes) {
  tags <- c(
    "divisions",
    "key",
    "time",
    "staves",
    "instruments",
    "clef",
    "directive"
  )

  locate_ordered_element(tag, attributes, tags)
}
