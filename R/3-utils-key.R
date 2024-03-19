prepare_keys <- function(music) {
  music <- add_global_key(music)

  keys <- music[["keys"]]
  lines <- music[["lines"]]

  keys <- locate_keys(keys, lines)
  keys <- specify_keys(keys, lines)

  keys
}


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


#' Make Key for Each Staff Explicit
#'
#' @returns For each staff, the Keys are reverse-ordered on `bar`.
#'
#' @noRd
specify_keys <- function(keys, lines) {
  specified <- NULL

  parts <- keys[["part"]]
  staffs <- keys[["staff"]]
  bars <- keys[["bar"]]

  locations <- unique(lines[, c("part", "staff")])

  for (k in seq_len(NROW(locations))) {
    location <- locations[k, ]
    part <- location[["part"]]
    staff <- location[["staff"]]

    # If a key is assigned to the staff, part, or score
    to_staff <- parts == part & staffs == staff
    to_part <- parts == part & staffs == 0
    to_score <- parts == 0

    # Local keys are more dominant
    if (any(to_staff)) {
      . <- keys[to_staff, ]
      to_part <- to_part & bars < min(.[["bar"]])
      to_score <- to_score & bars < min(.[["bar"]])
    }

    if (any(to_part)) {
      to_score <- to_score & bars < min(keys[to_part, ][["bar"]])
    }

    specified_k <- keys[to_staff | to_part | to_score, ]
    specified_k[["part"]] <- part
    specified_k[["staff"]] <- staff
    specified <- rbind(specified, specified_k)
  }

  . <- order(specified[["part"]], specified[["staff"]], -specified[["bar"]])
  specified[., ]
}
