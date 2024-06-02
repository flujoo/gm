#' @keywords internal
#' @export
to_MusicXML.Music <- function(x, ...) {
  lines <- x[["lines"]]
  notes <- x[["notes"]]
  meters <- x[["meters"]]

  divisions <- infer_divisions(lines, notes, meters)
  musicxml_score <- to_MusicXML_score(lines, notes, meters, divisions)

  # For the order of insertion, see `to_url("elements/attributes")`
  groups <- c("keys")

  for (group in groups) {
    objects <- x[[group]]

    for (k in seq_len(NROW(objects))) {
      musicxml_score <- insert(objects[k, ], musicxml_score, divisions)
    }
  }

  musicxml_score
}
