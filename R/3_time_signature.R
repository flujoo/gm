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


TimeSignature <- function(numerator, denominator, position = 1) {
  if (!validate.numerator(numerator)) {
    stop('argument "numerator" is invalid')
  }

  if (!validate.denominator(denominator)) {
    stop('argument "denominator" is invalid')
  }

  position <- PositionLine(position, "note")

  ts_ <- list(
    numerator = numerator,
    denominator = denominator,
    position = position
  )
  class(ts_) <- "TimeSignature"
  ts_
}


TimeSignatureLine <- function(time_signature, voice_length) {
  n <- time_signature$numerator
  d <- time_signature$denominator
  ps <- unclass(time_signature$position)

  for (i in 1:length(ps)) {
    p <- ps[[i]]
    # drop those beyond voice length
    # it is p not i
    if (p <= voice_length) {
      ps[[i]] <- c(p, n, d)
    } else {
      if (i == 1) {
        ps <- list()
      } else {
        ps <- ps[1:(i - 1)]
      }
      break
    }
  }

  class(ps) <- "TimeSignatureLine"
  ps
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
      ts_[1], ", ",
      paste(ts_[2:3], collapse = "/"),
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
  i2s <- c()

  # replace TimeSignature Points in line_1 with ones from line_2
  # the order can not be changed
  l1 <- length(line_1)
  l2 <- length(line_2)
  if (l1 > 0 && l2 > 0) {
    # don't need to always start from the 1st
    i <- 1
    for (i2 in 1:l2) {
      tsp2 <- line_2[[i2]]
      p2 <- tsp2[1]
      ts2 <- tsp2[2:3]
      if (i > l1) {
        break
      }
      for (i1 in i:l1) {
        tsp1 <- line_1[[i1]]
        p1 <- tsp1[1]
        if (p1 == p2) {
          i2s <- c(i2s, i2)
          line_1[[i1]][2:3] <- ts2
          i <- i1 + 1
          break
        }
      }
    }
  }

  # and delete those from line_2
  line_2[i2s] <- NULL

  # merge two lines
  l <- append(line_1, line_2)

  # sort
  js <- sapply(l, function(x) x[1])
  tsl <- l[order(js)]

  class(tsl) <- "TimeSignatureLine"
  tsl
}


#' @export
add.TimeSignature <- function(time_signature, voice) {
  tsl0 <- voice$time_signature
  tsl1 <- TimeSignatureLine(time_signature, length(voice$pitch))

  if (is.null(tsl0)) {
    voice$time_signature <- tsl1
  } else {
    voice$time_signature <- merge.TimeSignatureLine(tsl0, tsl1)
  }

  voice
}
