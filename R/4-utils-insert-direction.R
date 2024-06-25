insert_direction <- function(object, score, scope) {
  musicxml <- to_MusicXML(object)

  for (location in locate_notes(object, score, scope)) {
    i <- location[1]
    j <- location[2]
    k <- location[3]

    score$contents[[i]]$contents[[j]]$contents <- append(
      score$contents[[i]]$contents[[j]]$contents,
      list(musicxml),
      k - 1
    )
  }

  score
}
