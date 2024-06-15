#' @keywords internal
#' @export
insert <- function(x, to, ...) {
  UseMethod("insert")
}


#' Insert Attribute Object (Keys, Meters, ...) to `<score-partwise>`
#'
#' Elements in `<attributes>` should follow the order:
#' `to_url("elements/attributes")`. Initially, `<attributes>` contains
#' only `<divisions>` and `<staves>`.
#'
#' @param i The index of target `<part>`. For now, it's part number plus one.
#' @noRd
insert_attribute <- function(object, score, i, bar) {
  # All tags <attributes> can possibly contain
  tags <- c(
    "divisions",
    "key",
    "time",
    "staves"
  )

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
      locate_element(musicxml[["tag"]], attributes, tags)
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


locate_element <- function(tag, elements, tags) {
  before <- tags[seq_along(tags) <= which(tags == tag)]

  Position(
    \(element) element[["tag"]] %in% before,
    elements,
    right = TRUE,
    nomatch = 0L
  )
}
