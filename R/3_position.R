#' @title Validate and Normalize Position
#'
#' @param position A numeric vector, which will be converted to list,
#' or a list of numerics, items of which are of length 1 or 2.
#' @param type \code{"note"}, \code{"chord"} or \code{"line"}.
#'
#' @return A list of sorted whole numbers or pairs of ones, which is of class
#' "PositionLine". The pairs are sorted only for type "line".
#'
#' @details Items in PositionLine of type "chord" may contain a second
#' element, which indicates which component of a PitchChord or a TiedDurations
#' is targeted. Items of type "line" indicate starting and ending positions.
PositionLine <- function(position, type) {
  m <- 'argument "position" is invalid'

  c_ <- class(position)
  cs <- c("numeric", "integer")
  # normalize to list
  if (c_ %in% cs) {
    position <- as.list(position)
  # and check items' types
  } else if (c_ == "list") {
    if (!all(sapply(position, class) %in% cs)) {
      stop(m)
    }
  } else {
    stop(m)
  }

  # further check
  for (i in 1:length(position)) {
    p <- position[[i]]

    # NAs are not acceptable
    if (any(is.na(p))) {
      stop(m)
    }

    # have to be whole numbers
    if (class(p) == "numeric" && any(as.integer(p) != p)) {
      stop(m)
    }

    # have to be larger than 0
    if (any(p <= 0)) {
      stop(m)
    }

    # check length
    l <- length(p)
    con <- (type == "note" && l == 1) ||
      (type == "chord" && (l == 1 || l == 2)) ||
      (type == "line" && l == 2)
    if (!con) {
      stop(m)
    }

    # sort items only for type "line"
    if (type == "line") {
      position[[i]] <- sort(p)
    }
  }

  # sort position
  ks <- sapply(position, function(x) x[1])
  ps <- position[order(ks)]

  class(ps) <- "PositionLine"
  ps
}


to_string.PositionLine <- function(position_line) {
  for (i in 1:length(position_line)) {
    p <- position_line[[i]]
    l <- length(p)
    if (l == 2) {
      position_line[[i]] <- paste0("(", paste(p, collapse = ", "), ")")
    }
  }
  paste(position_line, collapse = ", ")
}


#' @export
print.PositionLine <- function(position_line) {
  s <- to_string.PositionLine(position_line)
  cat(s, "\n")
}
