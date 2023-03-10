separate_grace_notes <- function(music) {
  graces <- music$graces
  if (is.null(graces)) return(music)

  notes <- music$notes
  ks <- NULL

  for (k in seq_len(NROW(notes))) {
    note <- notes[k, ]
    is_grace <- any(graces$line == note$line & graces$i == note$i)
    if (is_grace) ks <- c(ks, k)
  }

  if (!is.null(ks)) {
    music$notes <- notes[-ks, ]
    music$grace_notes <- notes[ks, ]
  }

  music
}


restore_grace_notes <- function(music) {
  grace_notes <- music$grace_notes
  if (is.null(grace_notes)) return(music)

  notes <- rbind(music$notes, grace_notes)
  notes <- notes[order(notes$line, notes$i, notes$j), ]

  music$notes <- notes
  music$grace_notes <- NULL
  music
}
