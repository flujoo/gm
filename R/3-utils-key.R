add_global_key <- function(music) {
  keys <- music[["keys"]]
  global_key <- keys[is.na(keys[["line"]]) & keys[["bar"]] == 1, ]
  if (NROW(global_key) != 0) return(music)

  music + Key(0)
}


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
