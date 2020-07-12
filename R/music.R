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


#' @export
print.Music <- function(x, ...) {
  p <- x$pitch
  d <- x$duration
  l <- length(p)

  if (is.atomic(p)) {
    if (l > 1) {
      p <- delimit.vector(p, c("<", ">"))
    }
    s <- to_string.vector(c(p, d), c("(", ")"))

  } else if (is.list(p)) {
    if (all(sapply(p, is.atomic))) {
      s <- delimit.list(p, function(x) length(x) > 1, c("<", ">"))
      for (i in 1:l) {
        s[[i]] <- delimit.vector(c(s[[i]], d[[i]]), c("(", ")"))
      }
      s <- to_string.vector(unlist(s), c("[", "]"))

    } else if (all(sapply(p, is.list))) {
      for (i in 1:l) {
        s_i <- delimit.list(p[[i]], function(x) length(x) > 1, c("<", ">"))
        for (j in 1:length(s_i)) {
          p[[i]][[j]] <- delimit.vector(c(s_i[[j]], d[[i]][[j]]), c("(", ")"))
        }
        p[[i]] <- unlist(p[[i]])
      }
      s <- to_string.list(p)
    }
  }

  cat(s)
  invisible(s)
}
