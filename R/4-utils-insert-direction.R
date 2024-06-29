insert_direction <- function(object, score, scope, type = "start") {
  musicxml <- to_MusicXML(object, type)

  for (location in locate_notes(object, score, scope)) {
    i <- location[1]
    j <- location[2]
    k <- location[3]
    if (type == "start") k <- k - 1

    score$contents[[i]]$contents[[j]]$contents <- append(
      score$contents[[i]]$contents[[j]]$contents,
      list(musicxml),
      k
    )
  }

  score
}


insert_linelike <- function(object, score) {
  start <- object
  start[["j"]] <- NULL
  score <- insert_direction(start, score, "first", "start")

  stop <- object
  stop[["i"]] <- stop[["j"]]
  stop[["j"]] <- NULL
  insert_direction(stop, score, "last", "stop")
}
