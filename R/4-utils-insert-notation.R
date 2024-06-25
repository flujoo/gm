insert_notation <- function(object, score, scope) {
  musicxml_notation <- to_MusicXML(object)
  musicxml_notations <- MusicXML("notations", musicxml_notation)

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

    } else {
      notations <- children[[l]]$contents
      children[[l]]$contents <- append(notations, list(musicxml_notation))
      score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- children
    }
  }

  score
}
