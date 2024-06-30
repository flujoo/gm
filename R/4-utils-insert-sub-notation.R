insert_articulation <- function(object, score, scope) {
  insert_sub_notation(object, score, scope, "articulations")
}


insert_sub_notation <- function(object, score, scope, tag) {
  musicxml_sub_notation <- to_MusicXML(object)
  musicxml_notation <- MusicXML(tag, musicxml_sub_notation)
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

      next
    }

    notations <- children[[l]]$contents
    m <- locate_tag(tag, notations)

    if (length(m) == 0) {
      children[[l]]$contents <- append(
        notations,
        list(musicxml_notation)
      )

    } else {
      children[[l]]$contents[[m]]$contents <- append(
        notations[[m]]$contents,
        list(musicxml_sub_notation)
      )
    }

    score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- children
  }

  score
}
