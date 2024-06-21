insert_articulation <- function(object, score, scope) {
  locations <- locate_notes(object, score, scope)

  for (location in locations) {
    i <- location[1]
    j <- location[2]
    k <- location[3]

    children <- score$contents[[i]]$contents[[j]]$contents[[k]]$contents
    l <- locate_tag("notations", children)

    if (length(l) == 0) {
      score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- append(
        children,
        list(to_MusicXML(object, 3)),
        locate_note_child("notations", children)
      )

      next
    }

    notations <- children[[l]]$contents
    m <- locate_tag("articulations", notations)

    if (length(m) == 0) {
      children[[l]]$contents <- append(
        notations,
        list(to_MusicXML(object, 2))
      )

    } else {
      children[[l]]$contents[[m]]$contents <- append(
        notations[[m]]$contents,
        list(to_MusicXML(object, 1))
      )
    }

    score$contents[[i]]$contents[[j]]$contents[[k]]$contents <- children
  }

  score
}
