#' @keywords internal
#' @export
to_MusicXML.Clef <- function(x, ...) {
  contents <- list(
    MusicXML("sign", x[["sign"]]),
    MusicXML("line", x[["clef_line"]])
  )

  octave <- x[["octave"]]

  if (!is.na(octave)) {
    contents <- c(contents, list(MusicXML("clef-octave-change", octave)))
  }

  attributes <- list(number = x[["staff"]])
  MusicXML("clef", contents, attributes)
}


#' @keywords internal
#' @export
insert.Clef <- function(x, to, divisions, ...) {
  offset <- x[["offset"]]
  i <- x[["part"]] + 1
  bar <- x[["bar"]]

  if (offset == 0) return(insert_attribute(x, to, i, bar))

  duration <- offset * divisions

  musicxml <- list(
    to_MusicXML_forward(duration),
    to_MusicXML(Attributes(x)),
    to_MusicXML_backup(duration)
  )

  measures <- to$contents[[i]]$contents
  if (bar > length(measures)) return(to)
  notes <- measures[[bar]]$contents

  to$contents[[i]]$contents[[bar]]$contents <- append(
    notes,
    musicxml,
    locate_insertion("attributes", notes, "attributes")
  )

  to
}
