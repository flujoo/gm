#' @title Create a Pitch Object
#'
#' @description Create an object of S3 class "Pitch",
#' which is to represent the pitch aspect of music.
#'
#' @export
Pitch <- function(object) {

  l <- length(object)
  # match pitch notation
  reg <- "^[A-G](#{0,2}|-{0,2})[0-9]$"
  # error message
  m <- "invalid input to Pitch"

  if (is.atomic(object)) {

    # PitchRest
    # notice that `is.na(NaN) == TRUE`
    # exclude NaN because it coerces into character,
    # when combined into a character vector
    if (is.na(object) && l == 1 && !is.nan(object)) {
      class(object) <- c("PitchRest", "Pitch")
      return(object)

    # PitchNote
    } else if (all(grepl(reg, object))) {
      if (l == 1) {
        class(object) <- c("PitchNote", "Pitch")
        return(object)

      # PitchChord
      # `l > 1` instead of just `else` to exclude NULL,
      # since `all(grepl(reg, NULL)) == TRUE`
      } else if (l > 1) {
        class(object) <- c("PitchChord", "Pitch")
        return(object)
      }
    }
  }

  if (is.list(object) && l > 0) {

    # PitchVoice
    if (all(sapply(object, is.atomic))) {
      for (i in 1:l) {
        object[[i]] <- Pitch(object[[i]])
      }
      class(object) <- c("PitchVoice", "Pitch")
      return(object)

    # PitchVoices
    } else if (all(sapply(object, is.list))) {
      for (i in 1:l) {
        o <- object[[i]]
        if (all(sapply(o, is.atomic))) {
          object[[i]] <- Pitch(o)
        } else {
          stop(m, call. = FALSE)
        }
      }
      class(object) <- c("PitchVoices", "Pitch")
      return(object)
    }
  }

  stop(m, call. = FALSE)
}

