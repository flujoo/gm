#' @keywords internal
#' @export
to_MusicXML.Music <- function(x, ...) {
  lines <- x[["lines"]]
  notes <- x[["notes"]]
  meters <- x[["meters"]]

  divisions <- infer_divisions(lines, notes, meters, x$tempos, x$clefs)
  musicxml_score <- to_MusicXML_score(lines, notes, meters, divisions)

  for (name in names(x)) {
    if (name %in% c("lines", "notes", "ties", "velocities", "graces")) next
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
