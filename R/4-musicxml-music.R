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
