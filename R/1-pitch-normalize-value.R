#' Convert MIDI Note Number to All Equivalent Pitches
#' @noRd
to_enharmonics <- function(midi) {
  pitches <- list()
  steps <- c("C", "D", "E", "F", "G", "A", "B")

  pitch_class <- midi %% 12
  octave <- midi %/% 12 - 1

  for (alter in -2:2) {
    pitch_class_i <- pitch_class - alter
    octave_i <- octave + pitch_class_i %/% 12
    step <- steps[pitch_class_i %% 12 == c(0, 2, 4, 5, 7, 9, 11)]

    if (length(step) == 0) next
    pitch <- Pitch(step, alter, octave_i)
    pitches <- c(pitches, list(pitch))
  }

  pitches
}


#' Get Pitch Classes of Major Scale in Given Key
#' @noRd
get_scale <- function(key) {
  steps <- c("F", "C", "G", "D", "A", "E", "B")

  alters <- if (key >= 0) {
    c(rep(1, key), rep(0, 7 - key))

  } else {
    c(rep(0, 7 + key), rep(-1, -key))
  }

  lapply(1:7, function(i) Pitch(steps[i], alters[i]))
}


get_sharp_fifth <- function(key) {
  i <- which(-7:7 == key)
  step <- c("G", "D", "A", "E", "B", "F", "C")[(((i - 1) %% 7) + 1)]
  alter <- (i + 1) %/% 7
  Pitch(step, alter)
}


#' Get Chromatic Neighbor Pitch Classes
#' @noRd
get_neighbors <- function(notation) {
  steps <- c("C", "D", "E", "F", "G", "A", "B")
  pitch_classes <- lapply(steps, Pitch, alter = 0, octave = -1)
  values <- sapply(pitch_classes, to_value)

  alter <- pitch[["alter"]]
  k <- which(pitch[["step"]] == steps)
  value <- values[k]

  neighbors <- list()

  for (direction in c(-1, 1)) {
    step_i <- steps[((k + direction - 1) %% 7) + 1]
    value_i <- values[step_i == steps]
    alter_i <- alter - if (abs(value_i - value) == 2) direction else 0
    neighbor <- Pitch(step_i, alter_i)
    neighbors <- c(neighbors, list(neighbor))
  }

  neighbors
}


#' Find Enharmonic That Matches Any Pitch (Class) in Domain
#' @noRd
find_enharmonic <- function(enharmonics, domain) {
  for (enharmonic in enharmonics) {
    for (pitch in domain) {
      is_matched <- all(
        enharmonic[["step"]] == pitch[["step"]],
        enharmonic[["alter"]] == pitch[["alter"]],
        enharmonic[["octave"]] == pitch[["octave"]]
        # With `all()`, NULL octaves do not affect the result
      )

      if (is_matched) return(enharmonic)
    }
  }
}
