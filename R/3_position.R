#' @title Validate and Normalize Position
#'
#' @param position A numeric vector, which will be converted to list,
#' or a list of numerics, items of which are of length 1 or 2.
#' @param type \code{"note"}, \code{"chord"} or \code{"line"}.
#'
#' @return A list of sorted integers or pairs of ones, which is of class
#' "PositionLine". The pairs are sorted only for type "line". Duplicates are
#' removed.
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

  # one-by-one check
  for (i in 1:length(position)) {
    p <- position[[i]]

    # NAs are not acceptable
    if (any(is.na(p))) {
      stop(m)
    }

    if (class(p) == "numeric") {
      p_int <- as.integer(p)
      # have to be whole numbers
      if (any(p_int != p)) {
        stop(m)
      } else {
        # normalize to integers (make it easy for unique)
        p <- p_int
      }
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
    } else {
      # has to re-assign p since it has been converted to integer
      position[[i]] <- p
    }
  }

  # remove duplicates
  position <- unique(position)

  # add 1L as the 2nd element to singlets,
  # if items of same first elements exist
  # for example, convert 1 to c(1, 1), if there exists c(1, 2)
  if (type == "chord") {
    ks <- sapply(position, function(x) x[1])
    for (i in 1:length(position)) {
      p <- position[[i]]
      if (length(p) == 1 && p %in% ks[-i]) {
        position[[i]] <- c(p, 1L)
      }
    }

    # remove duplicates again in case for example,
    # c(1, 1) already exists and 1 is converted to c(1, 1)
    position <- unique(position)
  }

  # sort by first element
  ks <- sapply(position, function(x) x[1])
  position <- position[order(ks)]

  # sort by second element
  if (type != "note") {
    # have to re-assign ks
    ks <- sapply(position, function(x) x[1])
    for (k in unique(ks)) {
      is_ <- which(ks == k)
      if (length(is_) > 1) {
        ps <- position[is_]
        js <- sapply(ps, function(x) x[2])
        position[is_] <- ps[order(js)]
      }
    }
  }

  class(position) <- "PositionLine"
  position
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
