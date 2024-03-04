locate_keys <- function(keys, lines) {
  located <- NULL

  for (k in seq_len(NROW(keys))) {
    key <- keys[k, ]
    . <- locate(key, lines)

    located_k <- data_frame(
      part = .[1],
      staff = .[2],
      bar = .[3],
      key = key[["key"]]
    )

    located <- rbind(located, located_k)
  }

  located
}


find_key <- function(note, notes, keys, lines) {
  # If it's a grace note, find the note it attaches to
  repeat {
    if (!note[["grace"]]) break
    i <- note[["i"]] + 1
    chord <- notes[notes[["line"]] == note[["line"]] & notes[["i"]] == i, ]
    note <- chord[1, ]
  }

  line <- lines[note[["line"]], ]
  part <- line[["part"]]
  staff <- line[["staff"]]
  bar <- note[["start_bar"]]

  # Select the relevant keys
  keys <- keys[keys[["part"]] %in% c(0L, part), ]
  keys <- keys[keys[["staff"]] %in% c(0L, staff), ]
  keys <- keys[keys[["bar"]] <= bar, ]

  . <- order(
    keys[["part"]], keys[["staff"]], keys[["bar"]],
    decreasing = TRUE
  )

  keys[., ][1, ][["key"]]
}
