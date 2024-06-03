#' @keywords internal
#' @export
to_MusicXML.Music <- function(x, ...) {
  lines <- x[["lines"]]
  notes <- x[["notes"]]
  meters <- x[["meters"]]

  divisions <- infer_divisions(lines, notes, meters)
  musicxml_score <- to_MusicXML_score(lines, notes, meters, divisions)

  # At this point, <attributes> contains only <divisions> and <staves>
  # Follow `to_url("elements/attributes")` strictly to insert objects

  # At this point, <score-partwise> contains only <part-list> and <part>
  # Add other elements only afterwards

  groups <- c("keys")

  for (group in groups) {
    objects <- x[[group]]

    for (k in seq_len(NROW(objects))) {
      musicxml_score <- insert(objects[k, ], musicxml_score, divisions)
    }
  }

  musicxml_score
}
