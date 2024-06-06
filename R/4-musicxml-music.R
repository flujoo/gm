#' @keywords internal
#' @export
to_MusicXML.Music <- function(x, ...) {
  lines <- x[["lines"]]
  notes <- x[["notes"]]
  meters <- x[["meters"]]

  divisions <- infer_divisions(lines, notes, meters)
  musicxml_score <- to_MusicXML_score(lines, notes, meters, divisions)

  for (name in names(x)) {
    if (name %in% c("lines", "notes")) next
    objects <- x[[name]]

    # At this point, <score-partwise> contains only <part-list> and <part>
    # To make it simple, add other elements like title only afterwards
    if (!is.data.frame(objects)) next

    for (k in seq_len(NROW(objects))) {
      musicxml_score <- insert(objects[k, ], musicxml_score, divisions)
    }
  }

  musicxml_score
}


#' Insert Attribute Object (Keys, Meters, ...) to `<score-partwise>`
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


#' @description Elements in `<attributes>` should follow the order:
#' `to_url("elements/attributes")`. Find the insertion position for the
#' given tag. Initially, `<attributes>` contains only `<divisions>` and
#' `<staves>`.
#'
#' @param tag The tag of the element to insert.
#' @param attributes The contents of an <attributes> element.
#'
#' @noRd
locate_attribute <- function(tag, attributes) {
  tags <- c(
    "divisions",
    "key",
    "time",
    "staves"
  )

  before <- tags[seq_along(tags) <= which(tags == tag)]

  Position(
    \(musicxml) musicxml[["tag"]] %in% before,
    attributes,
    right = TRUE,
    nomatch = 0L
  )
}
