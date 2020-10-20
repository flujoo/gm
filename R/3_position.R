#' @title Validate and Normalize Position
#' @param type \code{"note"}, \code{"chord"} or \code{"line"}.
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

    # sort items
    position[[i]] <- sort(p)
  }

  # sort position
  ks <- sapply(position, function(x) x[1])
  position[order(ks)]
}
