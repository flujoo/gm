insert_articulation <- function(object, score, scope) {
  musicxml_articulation <- to_MusicXML(object)
  musicxml_articulations <- MusicXML("articulations", musicxml_articulation)
  musicxml_notations <- MusicXML("notations", musicxml_articulations)

  for (location in locate_notes(object, score, scope)) {
    i <- location[1]
    j <- location[2]
    k <- location[3]

    children <- score$contents[[i]]$contents[[j]]$contents[[k]]$contents
    l <- locate_tag("notations", children)

    if (length(l) == 0) {
      score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- append(
        children,
        list(musicxml_notations),
        locate_note_child("notations", children)
      )

      next
    }

    notations <- children[[l]]$contents
    m <- locate_tag("articulations", notations)

    if (length(m) == 0) {
      children[[l]]$contents <- append(
        notations,
        list(musicxml_articulations)
      )

    } else {
      children[[l]]$contents[[m]]$contents <- append(
        notations[[m]]$contents,
        list(musicxml_articulation)
      )
    }

    score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- children
  }

  score
}
