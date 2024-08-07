insert_articulation <- function(object, score, scope) {
  insert_sub_notation(object, score, scope, "articulations")
}


insert_ornament <- function(object, score, scope, type = NULL) {
  insert_sub_notation(object, score, scope, "ornaments", type)
}


insert_sub_notation <- function(object, score, scope, tag, type = NULL) {
  musicxml_sub_notation <- to_MusicXML(object, type)
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
      # Trill lines are already wrapped in lists
      if (inherits(musicxml_sub_notation, "MusicXML")) {
        musicxml_sub_notation <- list(musicxml_sub_notation)
      }

      children[[l]]$contents[[m]]$contents <- append(
        notations[[m]]$contents,
        musicxml_sub_notation
      )
    }

    score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- children
  }

  score
}
