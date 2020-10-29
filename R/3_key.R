validate.fifths <- function(fifths) {
  fifths >= -7 && fifths <= 7 && as.integer(fifths) == fifths
}


#' @export
Key <- function(fifths, position = 1) {
  if (!validate.fifths(fifths)) {
    stop('argument "fifths" is invalid')
  }

  position <- PositionLine(position, "note")

  k <- list(fifths = fifths, position = position)
  class(k) <- "Key"
  k
}


#' @return A list of pairs of a position and a fifths,
#' which is of class "KeyLine".
KeyLine <- function(key, voice_length) {
  k <- key$fifths
  ps <- unclass(key$position)

  # drop positions beyond voice length
  ps <- ps[ps <= voice_length]

  # combine to pairs
  kl <- lapply(ps, function(i) c(i, k))

  class(kl) <- "KeyLine"
  kl
}


to_string.KeyLine <- function(key_line) {
  kl <- unclass(key_line)
  l <- length(kl)
  if (l == 0) {
    return("")
  }
  for (i in 1:l) {
    kl[[i]] <- paste0("(", paste(kl[[i]], collapse = ", "), ")")
  }
  paste(kl, collapse = ", ")
}


#' @export
print.KeyLine <- function(key_line) {
  s <- to_string.KeyLine(key_line)
  cat(s, "\n")
}


merge.KeyLine <- function(line_1, line_2) {
  i2s <- c()

  # replace Key Points in line_1 with ones from line_2
  # the order can not be changed
  l1 <- length(line_1)
  l2 <- length(line_2)
  if (l1 > 0 && l2 > 0) {
    # don't need to always start from the 1st
    i <- 1
    for (i2 in 1:l2) {
      kp2 <- line_2[[i2]]
      p2 <- kp2[1]
      k2 <- kp2[2]
      if (i > l1) {
        break
      }
      for (i1 in i:l1) {
        kp1 <- line_1[[i1]]
        p1 <- kp1[1]
        if (p1 == p2) {
          i2s <- c(i2s, i2)
          line_1[[i1]][2] <- k2
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
  kl <- l[order(js)]

  class(kl) <- "KeyLine"
  kl
}


#' @export
add.Key <- function(key, voice) {
  kl0 <- voice$key
  kl1 <- KeyLine(key, length(voice$pitch))

  if (is.null(kl0)) {
    voice$key <- kl1
  } else {
    voice$key <- merge.KeyLine(kl0, kl1)
  }

  voice
}
