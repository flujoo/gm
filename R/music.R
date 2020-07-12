#' @title Create Music Object
#'
#' @description Create an object of S3 class "Music",
#' which is to represent music.
#'
#' @export
Music <- function(pitch, duration) {
  p <- Pitch(pitch)
  d <- Duration(duration)
  e <- "Pitch and Duration are not compatible"

  con <- (is.atomic(p) && is.atomic(d)) ||
    ((is.list(p) && is.list(d) && length(p) == length(d)) &&
     ((all(sapply(p, is.atomic)) && all(sapply(d, is.atomic))) ||
      (all(sapply(p, is.list)) && all(sapply(d, is.list)) &&
       all(sapply(d, length) == sapply(p, length)))))

  if (con) {
    m <- list(pitch = p, duration = d)
    class_ <- strsplit(class(p)[1], "Pitch")[[1]][2]
    class(m) <- c(class_, "Music")
    return(m)
  }

  stop(e)
}
