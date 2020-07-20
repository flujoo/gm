duration_types <- c(
  "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
  "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"
)


validate.duration_notations <- function(duration_notations) {
  reg_type <- paste0("(", paste(duration_types, collapse = "|"), ")")
  reg <- paste0(
    "^",
    # always starts with a type
    reg_type,
    # maybe followed by a dot block
    "(\\.{1,4})?",
    # followed by 0-n tuplet operators
    "(/[1-9][0-9]*)*",
    # maybe followed by a slur
    "-?",
    "$"
  )
  grepl(reg, duration_notations)
}


split.duration_notation <- function(duration_notation) {
  core <- function(reg) {
    ks <- gregexpr(reg, duration_notation)[[1]]
    out <- c()
    for (i in 1:length(ks)) {
      out_i <- substr(
        duration_notation,
        ks[i],
        attr(ks, "match.length")[i] + ks[i] - 1
      )
      out <- c(out, out_i)
    }
    out
  }

  list(
    type = core(paste(duration_types, collapse = "|")),
    modifier = core("/[1-9][0-9]*|\\.{1,4}"),
    slur = core("-")
  )
}


to_value.duration_types <- function(quarter = 1, as_fraction = TRUE) {
  l <- length(duration_types)
  vs <- rep(0, l)
  names(vs) <- duration_types

  i_q <- which(duration_types == "quarter")
  vs[i_q] <- quarter
  vs[(i_q - 1):1] <- quarter * 2^(1:(i_q - 1))
  vs[(i_q + 1):l] <- quarter / 2^(1:(l - i_q))

  if (as_fraction) {
    MASS::fractions(vs)
  } else {
    vs
  }
}


to_value.duration_notation <- function(duration_notation,
                                       quarter = 1, as_fraction = TRUE) {
  dn <- split.duration_notation(duration_notation)
  vs <- to_value.duration_types(quarter, as_fraction)

  type <- unname(vs[dn$type])

  dot <- dn$dot
  dot[dot == ""] <- 1
  dot[dot == "."] <- 1.5
  dot[dot == ".."] <- 1.75
  dot[dot == "..."] <- 1.875
  dot[dot == "...."] <- 1.9375
  dot <- as.numeric(dot)

  tuplet <- dn$tuplet
  tuplet[tuplet == ""] <- 1
  tuplet <- as.numeric(tuplet)

  sum(type * dot / tuplet)
}


#' @title Create Duration Object
#'
#' @description Create an object of S3 class "Duration",
#' which is to represent the durational aspect of music.
#'
#' @export
Duration <- function(object) {

  l <- length(object)
  # error message
  m <- "invalid input to Duration"

  if (is.character(object) && l == 1 &&
      validate.duration_notation(object)) {
    class(object) <- c("DurationNote", "Duration")
    return(object)
  }

  if (is.list(object) && l > 0) {

    if (all(sapply(object, is.character))) {
      for (i in 1:l) {
        object[[i]] <- Duration(object[[i]])
      }
      class(object) <- c("DurationVoice", "Duration")
      return(object)


    } else if (all(sapply(object, is.list))) {
      for (i in 1:l) {
        o <- object[[i]]
        if (all(sapply(o, is.character))) {
          object[[i]] <- Duration(o)
        } else {
          stop(m, call. = FALSE)
        }
      }
      class(object) <- c("DurationVoices", "Duration")
      return(object)
    }
  }

  stop(m, call. = FALSE)
}


#' @export
print.Duration <- function(x, ...) {

  if (is.atomic(x)) {
    s <- paste0(x, "\n")

  } else if (is.list(x)) {
    if (all(sapply(x, is.atomic))) {
      s <- to_string.vector(x, c("[", "]"))

    } else if (all(sapply(x, is.list))) {
      s <- to_string.list(x)
    }
  }

  cat(s)
  invisible(s)
}
