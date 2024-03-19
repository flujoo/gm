add_global_key <- function(music) {
  keys <- music[["keys"]]
  global_key <- keys[is.na(keys[["line"]]) & keys[["bar"]] == 1, ]
  if (NROW(global_key) != 0) return(music)

  music + Key(0)
}


locate_keys <- function(keys, lines) {
  keys[["part"]] <- NA_integer_
  keys[["staff"]] <- NA_integer_

  for (k in seq_len(NROW(keys))) {
    location <- locate(keys[k, ], lines)
    keys[["part"]][k] <- location[1]
    keys[["staff"]][k] <- location[2]
  }

  keys[, c("part", "staff", "bar", "key")]
}
