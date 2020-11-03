# TimeSignature -----------------------------------------------------

validate.numerator <- function(numerator) {
  con <- length(numerator) == 1 &&
    as.integer(numerator) == numerator &&
    numerator >= 1
  con
}


validate.denominator <- function(denominator) {
  con <- length(denominator) == 1 &&
    denominator %in% 2^(0:6)
  con
}


#' @export
TimeSignature <- function(numerator, denominator, position = 1) {
  if (!validate.numerator(numerator)) {
    stop('argument "numerator" is invalid')
  }

  if (!validate.denominator(denominator)) {
    stop('argument "denominator" is invalid')
  }

  position <- PositionLine(position, "chord")

  ts_ <- list(
    numerator = numerator,
    denominator = denominator,
    position = position
  )
  class(ts_) <- "TimeSignature"
  ts_
}



# TimeSignatureLine -------------------------------------------------

TimeSignatureLine <- function(time_signature, duration_line) {
  ps <- unclass(time_signature$position)
  # drop positions beyond voice length
  ps <- Filter(function(p) p[1] <= length(duration_line), ps)

  if (length(ps) == 0) {
    return(NULL)
  }

  # check sub-position
  for (i in 1:length(ps)) {
    p <- ps[[i]]
    p1 <- p[1]
    p2 <- p[2]
    d_ <- duration_line[[p1]]
    c_ <- class(d_)

    if (c_ == "Duration" && !is.na(p2)) {
      # simplify p when its sub-component is 1,
      # but the corresponding item in duration_line is not a TiedDurations
      if (p2 == 1) {
        ps[[i]] <- p1
      } else {
        s <- paste0("c(", paste(p, collapse = ", "), ")")
        stop("invalid position: ", s)
      }
    }

    if (c_ == "TiedDurations") {
      # expand p when it has no sub-component,
      # but the corresponding item in duration_line is a TiedDurations
      # this step is useful for other functions
      if (is.na(p2)) {
        ps[[i]] <- c(p1, 1L)
      } else if (p2 > length(d_)) {
        s <- paste0("c(", paste(p, collapse = ", "), ")")
        stop("invalid position: ", s)
      }
    }
  }

  n <- time_signature$numerator
  d <- time_signature$denominator
  tsl <- lapply(ps, function(p) list(p, c(n, d)))
  class(tsl) <- "TimeSignatureLine"
  tsl
}


to_string.TimeSignatureLine <- function(time_signature_line) {
  tsl <- unclass(time_signature_line)
  l <- length(tsl)

  if (l == 0) {
    return("")
  }

  for (i in 1:l) {
    ts_ <- tsl[[i]]
    tsl[[i]] <- paste0(
      "(",
      # position
      paste(ts_[[1]], collapse = "-"), ", ",
      # time signature
      paste(ts_[[2]], collapse = "/"),
      ")"
    )
  }

  paste(tsl, collapse = ", ")
}


#' @export
print.TimeSignatureLine <- function(time_signature_line) {
  s <- to_string.TimeSignatureLine(time_signature_line)
  cat(s, "\n")
}


merge.TimeSignatureLine <- function(line_1, line_2) {
  if (is.null(line_1)) {
    return(line_2)
  }
  if (is.null(line_2)) {
    return(line_1)
  }

  l1 <- length(line_1)
  l2 <- length(line_2)
  # replace Time Signature Points in line_1 with ones from line_2,
  # the order can not be changed
  # to-delete indices
  i2s <- c()
  # don't need to always start from the 1st
  i <- 1
  for (i2 in 1:l2) {
    if (i > l1) {
      break
    }
    tsp2 <- line_2[[i2]]
    p2 <- tsp2[[1]]
    ts2 <- tsp2[[2]]
    for (i1 in i:l1) {
      tsp1 <- line_1[[i1]]
      p1 <- tsp1[[1]]
      # replace
      if (all(p1 == p2)) {
        i2s <- c(i2s, i2)
        line_1[[i1]][[2]] <- ts2
        i <- i1 + 1
        break
      }
    }
  }

  # and delete those from line_2
  line_2[i2s] <- NULL

  # merge two lines
  line_ <- append(line_1, line_2)

  # sort by 1st
  ks <- sapply(line_, function(p) p[[1]][1])
  line_ <- line_[order(ks)]

  # sort by 2nd
  ks <- sapply(line_, function(p) p[[1]][1])
  for (k in unique(ks)) {
    is_ <- which(ks == k)
    if (length(is_) > 1) {
      tsps <- line_[is_]
      js <- sapply(tsps, function(p) p[[1]][2])
      line_[is_] <- tsps[order(js)]
    }
  }

  class(line_) <- "TimeSignatureLine"
  line_
}



# -------------------------------------------------------------------

#' @export
add.TimeSignature <- function(time_signature, voice) {
  tsl0 <- voice$time_signature
  tsl1 <- TimeSignatureLine(time_signature, voice$duration)
  tsl <- merge.TimeSignatureLine(tsl0, tsl1)
  validate.TimeSignatureLine(tsl, voice$duration)
  voice$time_signature <- tsl
  voice
}



# further check -----------------------------------------------------

get_positions.DurationLine <- function(duration_line) {
  ps <- list()

  for (i in 1:length(duration_line)) {
    d <- duration_line[[i]]
    c_ <- class(d)

    if (c_ == "Duration") {
      ps[[length(ps) + 1]] <- i

    } else if (c_ == "TiedDurations") {
      for (j in 1:length(d)) {
        ps[[length(ps) + 1]] <- c(i, j)
      }
    }
  }

  ps
}


get_values.DurationLine <- function(duration_line) {
  vs <- c()

  for (d in duration_line) {
    c_ <- class(d)

    if (c_ == "Duration") {
      v <- to_value.Duration(d)
    } else if (c_ == "TiedDurations") {
      v <- sapply(d, to_value.Duration)
    }

    vs <- c(vs, v)
  }

  vs
}


#' @return A numeric vector of 0, 1, and 2, which means that the corresponding
#' Duration is not a tuplet, the first one of a tuplet group, and non-first
#' one, respectively.
get_tuplet_indicators <- function(duration_line) {
  # flatten duration_line
  ds <- get_Durations(duration_line)
  # total value of current tuplet group
  v <- 0
  # accumulated values
  vs <- c()
  # indicators
  ks <- c()

  for (d in ds) {
    # non-tuplet
    if (length(d$tuplers) == 0) {
      k <- 0

    # tuplet
    } else {
      # check if it is the 1st one of a tuplet group
      if (v == 0) {
        v <- to_value.type(d$type) * to_value.dot(d$dot)
        k <- 1
      } else {
        k <- 2
      }
      # accumulate value
      vs <- c(vs, to_value.Duration(d))
      # check if it is the end of a group
      if (sum(vs) == v) {
        v <- 0
        vs <- c()
      }
    }

    ks <- c(ks, k)
  }

  ks
}


#' @details An abstract is a list of a Duration's position in a DurationLine,
#' its value, and its tuplet indicator.
#' @return A list of abstracts. Notice that its length is different from the
#' original DurationLine.
get_abstracts <- function(duration_line) {
  ps <- get_positions.DurationLine(duration_line)
  vs <- get_values.DurationLine(duration_line)
  ks <- get_tuplet_indicators(duration_line)

  as_ <- list()
  for (i in 1:length(ks)) {
    p <- ps[[i]]
    v <- vs[i]
    k <- ks[i]
    a <- list(p = p, v = v, k = k)
    as_[[length(as_) + 1]] <- a
  }
  as_
}


#' @details A group index is the position index of an item
#' in a list of abstracts, at which a time signature is added.
#' According to these indices, the abstracts are split into groups.
#' Notice that the position indices are of the derived list of abstracts,
#' not of the original DurationLine, which however the positions in
#' argument "positions" are of.
get_group_indices <- function(abstracts, positions) {
  ks <- c()
  l <- length(abstracts)
  i <- 1
  for (po in positions) {
    for (k in i:l) {
      p <- abstracts[[k]]$p
      if (all(p == po)) {
        ks <- c(ks, k)
        i < i + 1
        break
      }
    }
  }

  if (ks[1] != 1) {
    ks <- c(1, ks)
  }

  # this step is useful for the thereafter grouping function
  ks <- c(ks, l + 1)

  ks
}


group_abstracts <- function(abstracts, indices) {
  gs <- list()
  for (i in 1:(length(indices) - 1)) {
    k1 <- indices[i]
    k2 <- indices[i + 1]
    as_ <- abstracts[k1:(k2 - 1)]
    gs[[length(gs) + 1]] <- as_
  }
  gs
}


to_value.denominator <- function(denominator) {
  to_value.type("whole") / denominator
}


to_value.time_signature <- function(time_signature) {
  n <- time_signature[[1]]
  d <- time_signature[[2]]
  n * to_value.denominator(d)
}


validate.time_signature <- function(time_signature, abstracts, last = FALSE) {
  v_ts <- to_value.time_signature(time_signature)
  v <- 0
  for (a in abstracts) {
    k <- a$k

    if (v == 0 && k == 2) {
      stop("tuplets can not cross barlines")
    }

    v <- v + a$v
    if (v == v_ts) {
      v <- 0
    } else if (v > v_ts) {
      if (k != 0) {
        stop("tuplets can not cross barlines")
      }
      v <- v %% v_ts
    }
  }

  if (v != 0 && last == FALSE) {
    stop("some measure is incomplete under current time signatures")
  }
}


validate.TimeSignatureLine <- function(time_signature_line, duration_line) {
  # get abstracts from duration_line
  as_ <- get_abstracts(duration_line)
  # group abstracts
  pos <- lapply(time_signature_line, function(p) p[[1]])
  ks <- get_group_indices(as_, pos)
  gs <- group_abstracts(as_, ks)

  tss <- lapply(time_signature_line, function(p) p[[2]])
  l <- length(tss)

  # if first group has no corresponding time signature,
  # then there is no need to check it
  if (length(gs) > l) {
    gs <- gs[-1]
  }

  for (i in 1:l) {
    ts_ <- tss[[i]]
    g <- gs[[i]]
    last <- ifelse(i == l, TRUE, FALSE)
    validate.time_signature(ts_, g, last)
  }
}
